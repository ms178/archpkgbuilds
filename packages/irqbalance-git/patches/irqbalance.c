/*
 * Copyright (C) 2006, Intel Corporation
 * Copyright (C) 2012, Neil Horman <nhorman@tuxdriver.com>
 *
 * This file is part of irqbalance
 *
 * This program file is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation; version 2 of the License.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 * for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program in a file named COPYING; if not, write to the
 * Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110-1301 USA
 */
#include "config.h"

#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/time.h>
#include <syslog.h>
#include <signal.h>
#include <time.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <inttypes.h>
#include <errno.h>
#include <limits.h>
#include <stddef.h>
#include <stdbool.h>
#include <ctype.h>

#ifdef HAVE_GETOPT_LONG
#include <getopt.h>
#endif

#ifdef HAVE_LIBCAP_NG
#include <cap-ng.h>
#endif

#ifdef HAVE_IRQBALANCEUI
#include <sys/un.h>
#include <sys/socket.h>
#endif

#include <glib.h>
#include <glib-unix.h>

#include "irqbalance.h"
#include "thermal.h"

/* -------------------------------------------------------------------------- */
/* Global state (preserve public expectations)                                */
/* -------------------------------------------------------------------------- */

volatile int keep_going = 1;
int one_shot_mode;
int debug_mode;
int foreground_mode;
int numa_avail;
int journal_logging = 0;
int need_rescan;
int need_rebuild;

unsigned int log_mask = TO_ALL;
const char *log_indent;

unsigned long power_thresh = ULONG_MAX;
unsigned long deepest_cache = 2;
unsigned long long cycle_count = 0;

char *pidfile = NULL;
char *polscript = NULL;

long HZ;

int sleep_interval = 60;
static int user_min_interval = 60;
int last_interval;

GMainLoop *main_loop;

char *cpu_ban_string = NULL;
unsigned long migrate_ratio = 0;

#ifdef HAVE_IRQBALANCEUI
int socket_fd = -1;
char socket_name[108];
char *banned_cpumask_from_ui = NULL;
#endif

/* -------------------------------------------------------------------------- */
/* Helpers                                                                    */
/* -------------------------------------------------------------------------- */

static inline uint64_t irq_delta(const struct irq_info *irq)
{
	if (irq == NULL)
		return 0;

	if (irq->irq_count >= irq->last_irq_count)
		return irq->irq_count - irq->last_irq_count;

	/*
	 * Counter reset or wrap. Use current count as a conservative delta
	 * instead of underflowing.
	 */
	return irq->irq_count;
}

static bool parse_ulong_strict(const char *s, unsigned long min,
			       unsigned long max, unsigned long *out)
{
	unsigned long v;
	char *endptr;

	if (!s || !*s || !out)
		return false;

	errno = 0;
	v = strtoul(s, &endptr, 10);
	if (errno == ERANGE || s == endptr || *endptr != '\0')
		return false;
	if (v < min || v > max)
		return false;

	*out = v;
	return true;
}

static bool parse_ull_strict(const char *s, unsigned long long min,
			     unsigned long long max, unsigned long long *out)
{
	unsigned long long v;
	char *endptr;

	if (!s || !*s || !out)
		return false;

	errno = 0;
	v = strtoull(s, &endptr, 10);
	if (errno == ERANGE || s == endptr || *endptr != '\0')
		return false;
	if (v < min || v > max)
		return false;

	*out = v;
	return true;
}

static bool parse_long_strict(const char *s, long min, long max, long *out)
{
	long v;
	char *endptr;

	if (!s || !*s || !out)
		return false;

	errno = 0;
	v = strtol(s, &endptr, 10);
	if (errno == ERANGE || s == endptr || *endptr != '\0')
		return false;
	if (v < min || v > max)
		return false;

	*out = v;
	return true;
}

static int write_pidfile(const char *path)
{
	int fd;
	char buf[32];
	int len;

	if (!path || !*path)
		return 0;

retry:
	fd = open(path, O_WRONLY | O_CREAT | O_EXCL | O_TRUNC,
		  S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH);
	if (fd < 0) {
		if (errno == EEXIST) {
			int oldfd;
			char oldbuf[32] = {0};
			pid_t oldpid = -1;

			oldfd = open(path, O_RDONLY);
			if (oldfd >= 0) {
				ssize_t rd = read(oldfd, oldbuf,
						  sizeof(oldbuf) - 1);
				close(oldfd);

				if (rd > 0) {
					char *endptr = NULL;
					long parsed;

					errno = 0;
					parsed = strtol(oldbuf, &endptr, 10);
					if (errno == 0 && endptr != oldbuf &&
					    parsed > 0 &&
					    parsed <= INT_MAX)
						oldpid = (pid_t)parsed;
				}
			}

			if (oldpid > 0) {
				if (kill(oldpid, 0) == 0 || errno == EPERM) {
					log(TO_ALL, LOG_WARNING,
					    "Pidfile %s already exists and process %ld is alive\n",
					    path, (long)oldpid);
					return -1;
				}
			}

			if (unlink(path) == 0)
				goto retry;
		}

		log(TO_ALL, LOG_WARNING,
		    "Unable to create pidfile %s: %s\n",
		    path, strerror(errno));
		return -1;
	}

	len = snprintf(buf, sizeof(buf), "%u\n", (unsigned)getpid());
	if (len > 0 && (size_t)len < sizeof(buf)) {
		ssize_t wr = write(fd, buf, (size_t)len);
		(void)wr;
	}
	close(fd);

	return 0;
}

#ifdef HAVE_IRQBALANCEUI
static bool send_all(int fd, const char *buf, size_t len)
{
	int flags = 0;

#ifdef MSG_NOSIGNAL
	flags = MSG_NOSIGNAL;
#endif

	while (len > 0) {
		ssize_t ret = send(fd, buf, len, flags);

		if (ret < 0) {
			if (errno == EINTR)
				continue;
			return false;
		}
		if (ret == 0)
			return false;

		buf += ret;
		len -= (size_t)ret;
	}

	return true;
}
#endif

/* -------------------------------------------------------------------------- */
/* Adaptive interval policy                                                   */
/* -------------------------------------------------------------------------- */

static inline unsigned adaptive_interval_from_diff(uint64_t diff)
{
	/*
	 * For latency-sensitive desktop systems that pin critical IRQs out of
	 * band, irqbalance should become *less* active during heavy IRQ churn.
	 * Keep modest responsiveness only when activity is low.
	 */
	if (diff > 50000ULL)
		return 300U;
	if (diff > 10000ULL)
		return 180U;
	if (diff > 1000ULL)
		return 120U;
	return 60U;
}

/* -------------------------------------------------------------------------- */
/* Unified diff context                                                       */
/* -------------------------------------------------------------------------- */

struct collect_ctx {
	uint64_t *max_diff_ptr;
};

/* -------------------------------------------------------------------------- */
/* getopt_long options                                                        */
/* -------------------------------------------------------------------------- */

#ifdef HAVE_GETOPT_LONG
static struct option lopts[] = {
	{"oneshot", 0, NULL, 'o'},
	{"debug", 0, NULL, 'd'},
	{"foreground", 0, NULL, 'f'},
	{"powerthresh", 1, NULL, 'p'},
	{"banirq", 1, NULL, 'i'},
	{"deepestcache", 1, NULL, 'c'},
	{"policyscript", 1, NULL, 'l'},
	{"pid", 1, NULL, 's'},
	{"journal", 0, NULL, 'j'},
	{"banmod", 1, NULL, 'm'},
	{"interval", 1, NULL, 't'},
	{"version", 0, NULL, 'V'},
	{"migrateval", 1, NULL, 'e'},
	{0, 0, 0, 0}
};
#endif

/* -------------------------------------------------------------------------- */
/* Usage/version                                                              */
/* -------------------------------------------------------------------------- */

static void usage(void)
{
	log(TO_CONSOLE, LOG_INFO,
	    "irqbalance [--oneshot | -o] [--debug | -d] [--foreground | -f] [--journal | -j]\n");
	log(TO_CONSOLE, LOG_INFO,
	    "    [--powerthresh= | -p <off> | <n>] [--banirq= | -i <n>] [--banmod= | -m <module>] [--policyscript= | -l <script>]\n");
	log(TO_CONSOLE, LOG_INFO,
	    "    [--pid= | -s <file>] [--deepestcache= | -c <n>] [--interval= | -t <n>] [--migrateval= | -e <n>]\n");
}

static void version(void)
{
	log(TO_CONSOLE, LOG_INFO, "irqbalance version " VERSION "\n");
}

/* -------------------------------------------------------------------------- */
/* Command line parsing                                                       */
/* -------------------------------------------------------------------------- */

#ifdef HAVE_GETOPT_LONG
static void parse_command_line(int argc, char **argv)
{
	int opt;
	int longind;

	while ((opt = getopt_long(argc, argv, "odfjVi:p:s:c:l:m:t:e:",
				  lopts, &longind)) != -1) {
		switch (opt) {
		case '?':
			usage();
			exit(EXIT_FAILURE);
		case 'V':
			version();
			exit(EXIT_SUCCESS);
		case 'c': {
			unsigned long v;

			if (!parse_ulong_strict(optarg, 1UL, ULONG_MAX, &v)) {
				usage();
				exit(EXIT_FAILURE);
			}
			deepest_cache = v;
			break;
		}
		case 'd':
			debug_mode = 1;
			foreground_mode = 1;
			break;
		case 'f':
			foreground_mode = 1;
			break;
		case 'i': {
			unsigned long long v;

			if (!parse_ull_strict(optarg, 0ULL,
					      (unsigned long long)INT_MAX,
					      &v)) {
				usage();
				exit(EXIT_FAILURE);
			}
			add_cl_banned_irq((int)v);
			break;
		}
		case 'l': {
			char *tmp = strdup(optarg);

			if (tmp == NULL) {
				log(TO_ALL, LOG_WARNING,
				    "Out of memory allocating policyscript\n");
				exit(EXIT_FAILURE);
			}
			free(polscript);
			polscript = tmp;
			break;
		}
		case 'm':
			add_cl_banned_module(optarg);
			break;
		case 'p':
			if (g_ascii_strcasecmp(optarg, "off") == 0) {
				power_thresh = ULONG_MAX;
			} else {
				unsigned long long p;

				if (!parse_ull_strict(optarg, 0ULL,
						      (unsigned long long)ULONG_MAX,
						      &p)) {
					usage();
					exit(EXIT_FAILURE);
				}
				power_thresh = (unsigned long)p;
			}
			break;
		case 'o':
			one_shot_mode = 1;
			break;
		case 's': {
			char *tmp = strdup(optarg);

			if (tmp == NULL) {
				log(TO_ALL, LOG_WARNING,
				    "Out of memory allocating pidfile path\n");
				exit(EXIT_FAILURE);
			}
			free(pidfile);
			pidfile = tmp;
			break;
		}
		case 'j':
			journal_logging = 1;
			foreground_mode = 1;
			break;
		case 't': {
			long v;

			if (!parse_long_strict(optarg, 1L, INT_MAX, &v)) {
				usage();
				exit(EXIT_FAILURE);
			}
			sleep_interval = (int)v;
			user_min_interval = sleep_interval;
			break;
		}
		case 'e': {
			unsigned long v;

			if (!parse_ulong_strict(optarg, 0UL, ULONG_MAX, &v)) {
				usage();
				exit(EXIT_FAILURE);
			}
			migrate_ratio = v;
			break;
		}
		default:
			usage();
			exit(EXIT_FAILURE);
		}
	}
}
#else
static void parse_command_line(int argc, char **argv)
{
	int i;

	for (i = 1; i < argc; i++) {
		if (!strcmp(argv[i], "--debug")) {
			debug_mode = 1;
			foreground_mode = 1;
		} else if (!strcmp(argv[i], "--foreground")) {
			foreground_mode = 1;
		} else if (!strcmp(argv[i], "--oneshot")) {
			one_shot_mode = 1;
		} else if (!strcmp(argv[i], "--journal")) {
			journal_logging = 1;
			foreground_mode = 1;
		} else if (!strcmp(argv[i], "--version")) {
			version();
			exit(EXIT_SUCCESS);
		}
	}
}
#endif

/* -------------------------------------------------------------------------- */
/* Topology build / free / dump                                               */
/* -------------------------------------------------------------------------- */

/*
 * Build object tree:
 * - numa_nodes at top
 * - cpu packages, cache domains, cpu cores below; adjusted for memory topology
 * Object workload is the sum of workloads below it.
 */
static void build_object_tree(void)
{
	build_numa_node_list();
	parse_cpu_tree();
	rebuild_irq_db();
}

static void free_object_tree(void)
{
	free_numa_node_list();
	clear_cpu_tree();
	free_irq_db();
}

static void dump_object_tree(void)
{
	for_each_object(numa_nodes, dump_numa_node_info, NULL);
}

/* -------------------------------------------------------------------------- */
/* IRQ rebalance helpers                                                      */
/* -------------------------------------------------------------------------- */

void force_rebalance_irq(struct irq_info *info,
			 void *data __attribute__((unused)))
{
	if (info == NULL)
		return;
	if (info->level == BALANCE_NONE)
		return;

	if (info->assigned_obj == NULL) {
		if (g_list_find(rebalance_irq_list, info) == NULL)
			rebalance_irq_list =
				g_list_append(rebalance_irq_list, info);
	} else {
		migrate_irq_obj(info->assigned_obj, NULL, info);
	}
}

/* -------------------------------------------------------------------------- */
/* GLib signal handlers                                                       */
/* -------------------------------------------------------------------------- */

gboolean handler(gpointer data __attribute__((unused)))
{
	keep_going = 0;
	if (main_loop != NULL)
		g_main_loop_quit(main_loop);
	return TRUE;
}

gboolean force_rescan(gpointer data __attribute__((unused)))
{
	need_rescan = 1;
	return TRUE;
}

/* -------------------------------------------------------------------------- */
/* IRQ diff collection                                                        */
/* -------------------------------------------------------------------------- */

static void collect_diff(struct irq_info *info, void *data)
{
	struct collect_ctx *ctx = data;
	uint64_t diff;

	if (info == NULL || ctx == NULL || ctx->max_diff_ptr == NULL)
		return;

	diff = irq_delta(info);
	if (diff > *(ctx->max_diff_ptr))
		*(ctx->max_diff_ptr) = diff;
}

/* -------------------------------------------------------------------------- */
/* Periodic scan()                                                            */
/* -------------------------------------------------------------------------- */

gboolean scan(gpointer data __attribute__((unused)))
{
	int new_interval = sleep_interval;
	uint64_t max_diff = 0;

	if (log_mask & TO_CONSOLE)
		log(TO_CONSOLE, LOG_INFO,
		    "\n\n\n-----------------------------------------------------------------------------\n");

	clear_work_stats();
	parse_proc_interrupts();

	while (__builtin_expect(keep_going && (need_rescan || need_rebuild), 0)) {
		int try_times = 0;

		need_rescan = 0;
		cycle_count = 0;

		if (log_mask & TO_CONSOLE)
			log(TO_CONSOLE, LOG_INFO, "Rescanning CPU topology\n");

		clear_work_stats();

		do {
			free_object_tree();

			if (++try_times > 3) {
				if (log_mask & TO_CONSOLE)
					log(TO_CONSOLE, LOG_WARNING,
					    "Rescanning CPU topology: failed after retries\n");
				goto out;
			}

			need_rebuild = 0;
			build_object_tree();

			if (need_rebuild)
				usleep(100000);
		} while (need_rebuild);

		for_each_irq(NULL, force_rebalance_irq, NULL);
		clear_slots();
		parse_proc_interrupts();
		parse_proc_stat();
		return TRUE;
	}

	parse_proc_stat();

	if (cycle_count != 0ULL)
		update_migration_status();

	calculate_placement();
	activate_mappings();

	{
		struct collect_ctx ctx = {
			.max_diff_ptr = &max_diff,
		};
		for_each_irq(NULL, collect_diff, &ctx);
	}

	{
		unsigned adaptive = adaptive_interval_from_diff(max_diff);

		new_interval = (int)adaptive;
		if (new_interval < user_min_interval)
			new_interval = user_min_interval;
		if (new_interval < 1)
			new_interval = 1;
	}

out:
	if (debug_mode)
		dump_tree();

	if (one_shot_mode)
		keep_going = 0;

	cycle_count++;

	if (!keep_going) {
		if (main_loop != NULL)
			g_main_loop_quit(main_loop);
		return FALSE;
	}

	if (new_interval != sleep_interval) {
		sleep_interval = new_interval;
		last_interval = sleep_interval;
		g_timeout_add_seconds((guint)sleep_interval, scan, NULL);
		return FALSE;
	}

	return TRUE;
}

/* -------------------------------------------------------------------------- */
/* Stats helpers                                                              */
/* -------------------------------------------------------------------------- */

void get_irq_data(struct irq_info *irq, void *data)
{
	if (irq == NULL || data == NULL)
		return;

	GString **str = (GString **)data;
	if (*str == NULL)
		return;

	g_string_append_printf(*str,
			       "IRQ %d LOAD %" PRIu64 " DIFF %" PRIu64 " CLASS %d ",
			       irq->irq, irq->load, irq_delta(irq), irq->class);
}

static void append_irq(struct irq_info *irq, GString *str)
{
	if (irq == NULL || str == NULL)
		return;

	g_string_append_printf(str,
			       "IRQ %d LOAD %" PRIu64 " DIFF %" PRIu64 " CLASS %d ",
			       irq->irq, irq->load, irq_delta(irq), irq->class);
}

void get_object_stat(struct topo_obj *object, void *data)
{
	if (object == NULL || data == NULL)
		return;

	char **stats = (char **)data;
	GString *irq_str = g_string_new("");
	if (irq_str == NULL)
		return;

	for (GList *entry = object->interrupts; entry != NULL;
	     entry = g_list_next(entry)) {
		if (entry->data != NULL)
			append_irq((struct irq_info *)entry->data, irq_str);
	}

	GString *obj_str = g_string_new("");
	if (obj_str == NULL) {
		g_string_free(irq_str, TRUE);
		return;
	}

	g_string_append_printf(obj_str,
			       "TYPE %d NUMBER %d LOAD %" PRIu64 " SAVE_MODE %d %s",
			       object->obj_type, object->number,
			       object->load, object->powersave_mode,
			       irq_str->str);

	if (*stats != NULL && **stats != '\0') {
		char *new_stats = g_strconcat(*stats, obj_str->str, NULL);

		if (new_stats != NULL) {
			g_free(*stats);
			*stats = new_stats;
		}
	} else {
		*stats = g_strdup(obj_str->str);
		if (*stats == NULL)
			*stats = g_strdup("");
	}

	g_string_free(irq_str, TRUE);
	g_string_free(obj_str, TRUE);

	if (object->obj_type != OBJ_TYPE_CPU)
		for_each_object(object->children, get_object_stat, data);
}

/* -------------------------------------------------------------------------- */
/* UI Socket (optional)                                                       */
/* -------------------------------------------------------------------------- */
#ifdef HAVE_IRQBALANCEUI

static void append_setup_irq_gstring(struct irq_info *irq, void *data)
{
	if (irq == NULL || data == NULL)
		return;

	GString *gs = (GString *)data;
	g_string_append_printf(gs,
			       "IRQ %d LOAD %" PRIu64 " DIFF %" PRIu64 " CLASS %d ",
			       irq->irq, irq->load, irq_delta(irq), irq->class);
}

gboolean sock_handle(gint fd, GIOCondition condition,
		     gpointer user_data __attribute__((unused)))
{
	if (condition != G_IO_IN)
		return TRUE;

	int sock = accept(fd, NULL, NULL);
	if (__builtin_expect(sock < 0, 0)) {
		log(TO_ALL, LOG_WARNING, "Connection couldn't be accepted.\n");
		return TRUE;
	}

	char buff[16384];
	ssize_t recv_size = 0;
	int valid_user = 0;

	struct iovec iov = { .iov_base = buff, .iov_len = sizeof(buff) };
	void *ctrl = g_malloc((gsize)CMSG_SPACE(sizeof(struct ucred)));
	if (ctrl == NULL) {
		log(TO_ALL, LOG_WARNING,
		    "No memory for control message buffer.\n");
		close(sock);
		return TRUE;
	}

	struct msghdr msg;
	memset(&msg, 0, sizeof(msg));
	msg.msg_iov = &iov;
	msg.msg_iovlen = 1;
	msg.msg_control = ctrl;
	msg.msg_controllen = CMSG_SPACE(sizeof(struct ucred));

	recv_size = recvmsg(sock, &msg, 0);
	if (__builtin_expect(recv_size < 0, 0)) {
		log(TO_ALL, LOG_WARNING, "Error while receiving data.\n");
		goto out_close;
	}
	if (__builtin_expect((size_t)recv_size == sizeof(buff), 0)) {
		log(TO_ALL, LOG_WARNING, "Received command too long.\n");
		goto out_close;
	}
	buff[(recv_size >= 0) ? (size_t)recv_size : 0] = '\0';

	struct cmsghdr *cmsg = CMSG_FIRSTHDR(&msg);
	if (__builtin_expect(!cmsg, 0)) {
		log(TO_ALL, LOG_WARNING,
		    "No control message; missing credentials.\n");
		goto out_close;
	}
	if ((cmsg->cmsg_level == SOL_SOCKET) &&
	    (cmsg->cmsg_type == SCM_CREDENTIALS)) {
		struct ucred *credentials = (struct ucred *)CMSG_DATA(cmsg);
		if (credentials != NULL && credentials->uid == 0)
			valid_user = 1;
	}
	if (__builtin_expect(!valid_user, 0)) {
		log(TO_ALL, LOG_INFO,
		    "Permission denied for user to connect to socket.\n");
		goto out_close;
	}

	if (g_str_has_prefix(buff, "stats")) {
		char *stats = NULL;

		for_each_object(numa_nodes, get_object_stat, &stats);
		if (stats != NULL) {
			(void)send_all(sock, stats, strlen(stats));
			g_free(stats);
		}
	}

	if (g_str_has_prefix(buff, "settings ")) {
		const char *cmd = buff + strlen("settings ");

		if (g_str_has_prefix(cmd, "sleep ")) {
			const size_t offset = strlen("settings sleep ");
			if ((size_t)recv_size > offset) {
				char sleep_buf[32];
				size_t len = (size_t)recv_size - offset;
				unsigned long v;

				if (len >= sizeof(sleep_buf))
					len = sizeof(sleep_buf) - 1;

				memcpy(sleep_buf, buff + offset, len);
				sleep_buf[len] = '\0';

				if (parse_ulong_strict(sleep_buf, 1UL,
						       (unsigned long)INT_MAX,
						       &v)) {
					sleep_interval = (int)v;
					user_min_interval = sleep_interval;
				}
			}
		} else if (g_str_has_prefix(cmd, "ban irqs ")) {
			const size_t offset = strlen("settings ban irqs ");

			if ((size_t)recv_size > offset) {
				char irq_buf[256];
				size_t len = (size_t)recv_size - offset;

				if (len >= sizeof(irq_buf))
					len = sizeof(irq_buf) - 1;

				memcpy(irq_buf, buff + offset, len);
				irq_buf[len] = '\0';

				g_list_free_full(cl_banned_irqs, free);
				cl_banned_irqs = NULL;
				need_rescan = 1;

				if (!g_str_has_prefix(irq_buf, "NONE")) {
					const char *cur = irq_buf;

					while (*cur != '\0') {
						while (isspace((unsigned char)*cur))
							cur++;
						if (*cur == '\0')
							break;

						errno = 0;
						char *endp = NULL;
						unsigned long val = strtoul(cur, &endp, 10);

						if (cur == endp)
							break;
						if (errno == 0 &&
						    val <= (unsigned long)INT_MAX)
							add_cl_banned_irq((int)val);

						cur = endp;
					}
				}
			}
		} else if (g_str_has_prefix(cmd, "cpus ")) {
			banned_cpumask_from_ui = NULL;
			free(cpu_ban_string);
			cpu_ban_string = NULL;

			const size_t offset = strlen("settings cpus ");
			if ((size_t)recv_size > offset) {
				size_t len = (size_t)recv_size - offset;

				cpu_ban_string = malloc(len + 1U);
				if (cpu_ban_string == NULL) {
					log(TO_ALL, LOG_WARNING,
					    "Malloc failed for cpu_ban_string.\n");
					goto out_close;
				}

				memcpy(cpu_ban_string, buff + offset, len);
				cpu_ban_string[len] = '\0';

				char *saveptr = NULL;
				char *token = strtok_r(cpu_ban_string, " \t\r\n",
						       &saveptr);

				if (token != NULL &&
				    !g_str_has_prefix(token, "NULL")) {
					banned_cpumask_from_ui = token;
				} else {
					banned_cpumask_from_ui = NULL;
					free(cpu_ban_string);
					cpu_ban_string = NULL;
				}
				need_rescan = 1;
			}
		}
	}

	if (g_str_has_prefix(buff, "setup")) {
		GString *gs = g_string_new(NULL);

		if (gs != NULL) {
			char banned[512];

			g_string_append_printf(gs, "SLEEP %d ", sleep_interval);
			if (cl_banned_irqs != NULL)
				for_each_irq(cl_banned_irqs,
					     append_setup_irq_gstring, gs);

			cpumask_scnprintf(banned, sizeof(banned), banned_cpus);
			g_string_append_printf(gs, "BANNED %s", banned);

			(void)send_all(sock, gs->str, gs->len);
			g_string_free(gs, TRUE);
		}
	}

out_close:
	close(sock);
	g_free(ctrl);
	return TRUE;
}

int init_socket(void)
{
	struct sockaddr_un addr;
	memset(&addr, 0, sizeof(addr));

	socket_fd = socket(AF_LOCAL, SOCK_STREAM, 0);
	if (socket_fd < 0) {
		log(TO_ALL, LOG_WARNING, "Socket couldn't be created.\n");
		return 1;
	}

	addr.sun_family = AF_UNIX;
	{
		int n = snprintf(socket_name, sizeof(socket_name),
				 "%s/%s%d.sock",
				 SOCKET_TMPFS, SOCKET_PATH, getpid());
		if (n >= 0 && (size_t)n < sizeof(socket_name)) {
			strncpy(addr.sun_path, socket_name,
				sizeof(addr.sun_path) - 1U);
			addr.sun_path[sizeof(addr.sun_path) - 1U] = '\0';

			(void)unlink(addr.sun_path);

			socklen_t len =
				(socklen_t)(offsetof(struct sockaddr_un, sun_path) +
					    strlen(addr.sun_path) + 1U);
			if (bind(socket_fd, (struct sockaddr *)&addr, len) == 0) {
				int optval = 1;
				guint source_id;

				if (setsockopt(socket_fd, SOL_SOCKET,
					       SO_PASSCRED, &optval,
					       sizeof(optval)) < 0) {
					log(TO_ALL, LOG_WARNING,
					    "Unable to set socket options.\n");
					close(socket_fd);
					socket_fd = -1;
					return 1;
				}
				if (listen(socket_fd, 1) < 0) {
					log(TO_ALL, LOG_WARNING,
					    "Unable to listen on socket.\n");
					close(socket_fd);
					socket_fd = -1;
					return 1;
				}

				source_id = g_unix_fd_add(socket_fd, G_IO_IN,
							  sock_handle, NULL);
				if (source_id == 0) {
					log(TO_ALL, LOG_WARNING,
					    "Unable to register socket with main loop.\n");
					close(socket_fd);
					socket_fd = -1;
					return 1;
				}
				return 0;
			}
			log(TO_ALL, LOG_WARNING,
			    "Daemon couldn't be bound to the file-based socket.\n");
		}
	}

	memset(&addr, 0, sizeof(addr));
	addr.sun_family = AF_UNIX;
	{
		const char *aname = "irqbalance-ui";
		size_t alen = strlen(aname);

		if (alen + 1U > sizeof(addr.sun_path)) {
			log(TO_ALL, LOG_WARNING,
			    "Abstract socket name too long.\n");
			close(socket_fd);
			socket_fd = -1;
			return 1;
		}

		addr.sun_path[0] = '\0';
		memcpy(addr.sun_path + 1, aname, alen);

		socklen_t len =
			(socklen_t)(offsetof(struct sockaddr_un, sun_path) +
				    1U + alen);
		if (bind(socket_fd, (struct sockaddr *)&addr, len) < 0) {
			log(TO_ALL, LOG_WARNING,
			    "Daemon couldn't be bound to the abstract socket, bailing out.\n");
			close(socket_fd);
			socket_fd = -1;
			return 1;
		}

		int optval = 1;
		guint source_id;

		if (setsockopt(socket_fd, SOL_SOCKET, SO_PASSCRED,
			       &optval, sizeof(optval)) < 0) {
			log(TO_ALL, LOG_WARNING,
			    "Unable to set socket options.\n");
			close(socket_fd);
			socket_fd = -1;
			return 1;
		}
		if (listen(socket_fd, 1) < 0) {
			log(TO_ALL, LOG_WARNING,
			    "Unable to listen on socket.\n");
			close(socket_fd);
			socket_fd = -1;
			return 1;
		}

		source_id = g_unix_fd_add(socket_fd, G_IO_IN,
					  sock_handle, NULL);
		if (source_id == 0) {
			log(TO_ALL, LOG_WARNING,
			    "Unable to register socket with main loop.\n");
			close(socket_fd);
			socket_fd = -1;
			return 1;
		}
	}

	return 0;
}
#endif /* HAVE_IRQBALANCEUI */

/* -------------------------------------------------------------------------- */
/* main()                                                                     */
/* -------------------------------------------------------------------------- */

int main(int argc, char **argv)
{
	sigset_t sigset, old_sigset;
	int ret = EXIT_SUCCESS;

	sigemptyset(&sigset);
	sigaddset(&sigset, SIGINT);
	sigaddset(&sigset, SIGHUP);
	sigaddset(&sigset, SIGTERM);
	sigaddset(&sigset, SIGUSR1);
	sigaddset(&sigset, SIGUSR2);
	(void)sigprocmask(SIG_BLOCK, &sigset, &old_sigset);

	parse_command_line(argc, argv);

	if (getenv("INVOCATION_ID") != NULL) {
		journal_logging = 1;
		foreground_mode = 1;
	}

	openlog(argv[0], LOG_PID, LOG_DAEMON);

	if (getenv("IRQBALANCE_ONESHOT") != NULL)
		one_shot_mode = 1;

	if (getenv("IRQBALANCE_DEBUG") != NULL) {
		debug_mode = 1;
		foreground_mode = 1;
	}

	log_indent = journal_logging ? "...." : "    ";

	if (!debug_mode)
		log_mask &= ~TO_CONSOLE;

	if (numa_available() > -1) {
		numa_avail = 1;
	} else {
		log(TO_CONSOLE, LOG_INFO,
		    "This machine seems not NUMA capable.\n");
	}

	if (geteuid() != 0) {
		log(TO_ALL, LOG_WARNING,
		    "Irqbalance hasn't been executed under root privileges, thus it won't in fact balance interrupts.\n");
	}

	HZ = sysconf(_SC_CLK_TCK);
	if (HZ == -1) {
		log(TO_ALL, LOG_WARNING,
		    "Unable to determine HZ; defaulting to 100\n");
		HZ = 100;
	}

	if (!foreground_mode) {
		if (daemon(0, 0) != 0)
			exit(EXIT_FAILURE);

		if (write_pidfile(pidfile) != 0)
			exit(EXIT_FAILURE);
	}

	build_object_tree();
	if (debug_mode)
		dump_object_tree();

	if (num_online_cpus() <= 1) {
		log(TO_ALL, LOG_WARNING,
		    "%s",
		    "Balancing is ineffective on systems with a single CPU. Shutting down\n");
		goto out;
	}

	g_unix_signal_add(SIGINT, handler, NULL);
	g_unix_signal_add(SIGTERM, handler, NULL);
	g_unix_signal_add(SIGUSR1, handler, NULL);
	g_unix_signal_add(SIGUSR2, handler, NULL);
	g_unix_signal_add(SIGHUP, force_rescan, NULL);

	(void)sigprocmask(SIG_SETMASK, &old_sigset, NULL);

#ifdef HAVE_LIBCAP_NG
	capng_clear(CAPNG_SELECT_BOTH);
	capng_lock();
	capng_apply(CAPNG_SELECT_BOTH);
#endif

	for_each_irq(NULL, force_rebalance_irq, NULL);

	parse_proc_interrupts();
	parse_proc_stat();
	clear_slots();

#ifdef HAVE_IRQBALANCEUI
	if (init_socket() != 0) {
		ret = EXIT_FAILURE;
		goto out;
	}
#endif

	if (init_thermal() != 0)
		log(TO_ALL, LOG_WARNING,
		    "Failed to initialize thermal events.\n");

	main_loop = g_main_loop_new(NULL, FALSE);
	if (main_loop == NULL) {
		log(TO_ALL, LOG_WARNING, "Failed to create main loop.\n");
		ret = EXIT_FAILURE;
		goto out;
	}

	last_interval = sleep_interval;
	user_min_interval = sleep_interval;

	g_timeout_add_seconds((guint)sleep_interval, scan, NULL);
	g_main_loop_run(main_loop);

out:
	deinit_thermal();
	free_object_tree();
	free_cl_opts();
	free(polscript);

	if (!foreground_mode && pidfile != NULL)
		unlink(pidfile);
	free(pidfile);

#ifdef HAVE_IRQBALANCEUI
	free(cpu_ban_string);
	if (socket_fd >= 0) {
		close(socket_fd);
		socket_fd = -1;
	}
	if (socket_name[0] != '\0')
		unlink(socket_name);
#endif

	if (main_loop != NULL) {
		g_main_loop_unref(main_loop);
		main_loop = NULL;
	}

	closelog();
	return ret;
}
