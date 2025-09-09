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
/* Global state (maintain ABI expectations)                                   */
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
static int user_min_interval = 60; /* True user-requested minimum (-t); preserved across adaptation */
int last_interval;

GMainLoop *main_loop;

char *cpu_ban_string = NULL;
unsigned long migrate_ratio = 0;

#ifdef HAVE_IRQBALANCEUI
int socket_fd;
char socket_name[108];
char *banned_cpumask_from_ui = NULL;
#endif

/* -------------------------------------------------------------------------- */
/* Adaptive interval policy                                                   */
/* -------------------------------------------------------------------------- */

static inline unsigned adaptive_interval_from_diff(uint64_t diff)
{
	/* High IRQ churn => shorter rescan interval */
	if (diff > 10000U) {
		return 10U;
	}
	if (diff > 5000U) {
		return 20U;
	}
	if (diff > 1000U) {
		return 30U;
	}
	return 60U;
}

/* -------------------------------------------------------------------------- */
/* Unified collect context (remove strict-aliasing UB)                        */
/* -------------------------------------------------------------------------- */

struct collect_ctx {
	int *idx_ptr;
	uint64_t *max_diff_ptr;
	uint64_t *irq_diffs_ptr;
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
	{"banirq", 1 , NULL, 'i'},
	{"deepestcache", 1, NULL, 'c'},
	{"policyscript", 1, NULL, 'l'},
	{"pid", 1, NULL, 's'},
	{"journal", 0, NULL, 'j'},
	{"banmod", 1 , NULL, 'm'},
	{"interval", 1 , NULL, 't'},
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
	log(TO_CONSOLE, LOG_INFO, "irqbalance [--oneshot | -o] [--debug | -d] [--foreground | -f] [--journal | -j]\n");
	log(TO_CONSOLE, LOG_INFO, "    [--powerthresh= | -p <off> | <n>] [--banirq= | -i <n>] [--banmod= | -m <module>] [--policyscript= | -l <script>]\n");
	log(TO_CONSOLE, LOG_INFO, "    [--pid= | -s <file>] [--deepestcache= | -c <n>] [--interval= | -t <n>] [--migrateval= | -e <n>]\n");
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
	char *endptr;

	while ((opt = getopt_long(argc, argv, "odfjVi:p:s:c:l:m:t:e:", lopts, &longind)) != -1) {
		switch (opt) {
		case '?':
			usage();
			exit(EXIT_FAILURE);
			break;
		case 'V':
			version();
			exit(EXIT_SUCCESS);
			break;
		case 'c': {
			errno = 0;
			unsigned long v = strtoul(optarg, &endptr, 10);
			if (errno == ERANGE || optarg == endptr || v == 0UL) {
				usage();
				exit(EXIT_FAILURE);
			}
			deepest_cache = v;
			} break;
		case 'd':
			debug_mode = 1;
			foreground_mode = 1;
			break;
		case 'f':
			foreground_mode = 1;
			break;
		case 'i': {
			errno = 0;
			unsigned long long v = strtoull(optarg, &endptr, 10);
			if (errno == ERANGE || optarg == endptr) {
				usage();
				exit(EXIT_FAILURE);
			}
			if (v > (unsigned long long)INT_MAX) {
				usage();
				exit(EXIT_FAILURE);
			}
			add_cl_banned_irq((int)v);
			} break;
		case 'l':
			free(polscript);
			polscript = strdup(optarg);
			break;
		case 'm':
			add_cl_banned_module(optarg);
			break;
		case 'p':
			if (g_str_has_prefix(optarg, "off")) {
				power_thresh = ULONG_MAX;
			} else {
				errno = 0;
				unsigned long long p = strtoull(optarg, &endptr, 10);
				if (errno == ERANGE || optarg == endptr || p > ULONG_MAX) {
					usage();
					exit(EXIT_FAILURE);
				}
				power_thresh = (unsigned long)p;
			}
			break;
		case 'o':
			one_shot_mode = 1;
			break;
		case 's':
			/* Note: We intentionally keep the argv-owned pointer here;
			 * it's stable for process lifetime. */
			pidfile = optarg;
			break;
		case 'j':
			journal_logging = 1;
			foreground_mode = 1;
			break;
		case 't': {
			errno = 0;
			long v = strtol(optarg, &endptr, 10);
			if (errno == ERANGE || optarg == endptr || v < 1 || v > INT_MAX) {
				usage();
				exit(EXIT_FAILURE);
			}
			sleep_interval = (int)v;
			user_min_interval = sleep_interval;
			} break;
		case 'e': {
			errno = 0;
			unsigned long v = strtoul(optarg, &endptr, 10);
			if (errno == ERANGE || optarg == endptr) {
				usage();
				exit(EXIT_FAILURE);
			}
			migrate_ratio = v;
			} break;
		default:
			usage();
			exit(EXIT_FAILURE);
			break;
		}
	}
}
#else /* ! HAVE_GETOPT_LONG */
static void parse_command_line(int argc, char **argv)
{
	/* Compat parser for minimal builds */
	if (argc > 1 && strstr(argv[1], "--debug")) {
		debug_mode = 1;
		foreground_mode = 1;
	}
	if (argc > 1 && strstr(argv[1], "--foreground")) {
		foreground_mode = 1;
	}
	if (argc > 1 && strstr(argv[1], "--oneshot")) {
		one_shot_mode = 1;
	}
	if (argc > 1 && strstr(argv[1], "--journal")) {
		journal_logging = 1;
		foreground_mode = 1;
	}
}
#endif /* HAVE_GETOPT_LONG */

/* -------------------------------------------------------------------------- */
/* Topology build / free / dump                                               */
/* -------------------------------------------------------------------------- */

/*
 * This builds our object tree.  The hierarchy is typically:
 * - numa_nodes at top
 * - cpu packages, cache domains, cpu cores below, adjusted for memory topology
 * Object workload is the sum of workloads below it.
 */
static void build_object_tree(void)
{
	build_numa_node_list();
	parse_cpu_tree();
	rebuild_irq_db();

	/* Prefetch children pointers to improve locality for initial traversals */
	GList *node = numa_nodes;
	while (node != NULL) {
		if (node->data != NULL) {
			struct topo_obj *obj = (struct topo_obj *)node->data;
			__builtin_prefetch(obj->children, 0 /* read */, 3 /* high locality */);
		}
		node = g_list_next(node);
	}
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

void force_rebalance_irq(struct irq_info *info, void *data __attribute__((unused)))
{
	if (info == NULL) {
		return;
	}
	if (info->level == BALANCE_NONE) {
		return;
	}

	if (info->assigned_obj == NULL) {
		rebalance_irq_list = g_list_append(rebalance_irq_list, info);
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
	if (main_loop != NULL) {
		g_main_loop_quit(main_loop);
	}
	return TRUE;
}

gboolean force_rescan(gpointer data __attribute__((unused)))
{
	if (cycle_count) {
		need_rescan = 1;
	}
	return TRUE;
}

/* -------------------------------------------------------------------------- */
/* IRQ diff collection (strict aliasing safe)                                 */
/* -------------------------------------------------------------------------- */

static void collect_diff(struct irq_info *info, void *data)
{
	if (info == NULL || data == NULL) {
		return;
	}

	struct collect_ctx *ctx = (struct collect_ctx *)data;
	uint64_t diff = info->irq_count - info->last_irq_count;

	if (*(ctx->idx_ptr) < 1024) {
		ctx->irq_diffs_ptr[*(ctx->idx_ptr)] = diff;
		if (diff > *(ctx->max_diff_ptr)) {
			*(ctx->max_diff_ptr) = diff;
		}
		(*(ctx->idx_ptr))++;
	} else {
		/* >1024 IRQs: update max only (no full cache) */
		if (diff > *(ctx->max_diff_ptr)) {
			*(ctx->max_diff_ptr) = diff;
		}
	}
}

/* -------------------------------------------------------------------------- */
/* Periodic scan()                                                            */
/* -------------------------------------------------------------------------- */

gboolean scan(gpointer data __attribute__((unused)))
{
	static uint64_t irq_diffs[1024] = {0};
	static int cached_count = 0;
	static uint64_t last_cycle = 0;

	if (log_mask & TO_CONSOLE) {
		log(TO_CONSOLE, LOG_INFO, "\n\n\n-----------------------------------------------------------------------------\n");
	}

	clear_work_stats();
	parse_proc_interrupts();

	/* Handle CPU hotplug / topology changes */
	while (__builtin_expect(keep_going && (need_rescan || need_rebuild), 0)) {
		int try_times = 0;

		need_rescan = 0;
		cycle_count = 0;

		if (log_mask & TO_CONSOLE) {
			log(TO_CONSOLE, LOG_INFO, "Rescanning CPU topology\n");
		}

		clear_work_stats();

		do {
			free_object_tree();

			if (++try_times > 3) {
				if (log_mask & TO_CONSOLE) {
					log(TO_CONSOLE, LOG_WARNING, "Rescanning CPU topology: failed after retries\n");
				}
				goto out;
			}

			need_rebuild = 0;
			build_object_tree();

			if (need_rebuild) {
				/* Short delay to allow topology to stabilize before retry */
				usleep(100000);
			}
		} while (need_rebuild);

		for_each_irq(NULL, force_rebalance_irq, NULL);
		clear_slots();
		parse_proc_interrupts();
		parse_proc_stat();

		/* Invalidate cached diffs post-rescan */
		cached_count = 0;
		last_cycle = cycle_count;
		return TRUE;
	}

	parse_proc_stat();

	if (cycle_count != 0ULL) {
		update_migration_status();
	}

	calculate_placement();
	activate_mappings();

	/* Compute max IRQ diff (with optional one-cycle reuse) */
	uint64_t max_diff = 0;

	if (last_cycle == (cycle_count - 1ULL) && cached_count > 0) {
		for (int i = 0; i < cached_count; ++i) {
			if (irq_diffs[i] > max_diff) {
				max_diff = irq_diffs[i];
			}
		}
	} else {
		int idx = 0;
		struct collect_ctx ctx = {
			.idx_ptr = &idx,
			.max_diff_ptr = &max_diff,
			.irq_diffs_ptr = irq_diffs
		};
		for_each_irq(NULL, collect_diff, &ctx);

		if (idx <= 1024) {
			cached_count = idx;
		} else {
			cached_count = 0; /* overflow => don't reuse */
		}
		last_cycle = cycle_count;
	}

	/* Adaptive interval with proper clamping to the true user minimum */
	unsigned adaptive = adaptive_interval_from_diff(max_diff);
	int new_interval = (int)adaptive;
	if (new_interval < user_min_interval) {
		new_interval = user_min_interval;
	}

	if (new_interval < 1) {
		new_interval = 1; /* safety clamp */
	}

out:
	if (debug_mode) {
		dump_tree();
	}

	if (one_shot_mode) {
		keep_going = 0;
	}

	cycle_count++;

	/* Reschedule timer if interval changed */
	if (new_interval != sleep_interval) {
		sleep_interval = new_interval;
		last_interval = sleep_interval;
		g_timeout_add_seconds(sleep_interval, scan, NULL);
		return FALSE; /* stop current source; new one will run */
	}

	if (keep_going) {
		return TRUE; /* keep the same timer */
	}

	if (main_loop != NULL) {
		g_main_loop_quit(main_loop);
	}
	return FALSE;
}

/* -------------------------------------------------------------------------- */
/* Stats helpers                                                              */
/* -------------------------------------------------------------------------- */

void get_irq_data(struct irq_info *irq, void *data)
{
	if (irq == NULL || data == NULL) {
		return;
	}
	GString **str = (GString **)data;
	if (*str == NULL) {
		/* OOM fallback: skip safely */
		return;
	}
	g_string_append_printf(*str, "IRQ %d LOAD %" PRIu64 " DIFF %" PRIu64 " CLASS %d ",
	                       irq->irq, irq->load, (irq->irq_count - irq->last_irq_count), irq->class);
}

static void append_irq(struct irq_info *irq, GString *str)
{
	if (irq == NULL || str == NULL) {
		return;
	}
	g_string_append_printf(str, "IRQ %d LOAD %" PRIu64 " DIFF %" PRIu64 " CLASS %d ",
	                       irq->irq, irq->load, (irq->irq_count - irq->last_irq_count), irq->class);
}

void get_object_stat(struct topo_obj *object, void *data)
{
	if (object == NULL || data == NULL) {
		return;
	}

	char **stats = (char **)data;
	GString *irq_str = g_string_new("");
	if (irq_str == NULL) {
		return; /* OOM */
	}

	/* Iterate interrupt list directly (avoid O(n) g_list_length()) */
	for (GList *entry = object->interrupts; entry != NULL; entry = g_list_next(entry)) {
		if (entry->data != NULL) {
			append_irq((struct irq_info *)entry->data, irq_str);
		}
	}

	GString *obj_str = g_string_new("");
	if (obj_str == NULL) {
		g_string_free(irq_str, TRUE);
		return;
	}

	g_string_append_printf(obj_str, "TYPE %d NUMBER %d LOAD %" PRIu64 " SAVE_MODE %d %s",
	                       object->obj_type, object->number, object->load, object->powersave_mode, irq_str->str);

	char *new_stats = NULL;
	if (*stats != NULL && **stats != '\0') {
		new_stats = g_strconcat(*stats, obj_str->str, NULL);
		if (new_stats == NULL) {
			/* OOM: keep original stats */
			g_string_free(irq_str, TRUE);
			g_string_free(obj_str, TRUE);
			return;
		}
		free(*stats);
		*stats = new_stats;
	} else {
		*stats = g_strdup(obj_str->str);
		if (*stats == NULL) {
			/* OOM: set to empty string to remain consistent */
			*stats = g_strdup("");
		}
	}

	g_string_free(irq_str, TRUE);
	g_string_free(obj_str, TRUE);

	if (object->obj_type != OBJ_TYPE_CPU) {
		for_each_object(object->children, get_object_stat, data);
	}
}

/* -------------------------------------------------------------------------- */
/* UI Socket (optional)                                                       */
/* -------------------------------------------------------------------------- */
#ifdef HAVE_IRQBALANCEUI

/* Helper for "setup" path: append to GString */
static void append_setup_irq_gstring(struct irq_info *irq, void *data)
{
	if (irq == NULL || data == NULL) {
		return;
	}
	GString *gs = (GString *)data;
	g_string_append_printf(gs, "IRQ %d LOAD %" PRIu64 " DIFF %" PRIu64 " CLASS %d ",
	                       irq->irq, irq->load, (irq->irq_count - irq->last_irq_count), irq->class);
}

gboolean sock_handle(gint fd, GIOCondition condition, gpointer user_data __attribute__((unused)))
{
	if (condition != G_IO_IN) {
		return TRUE;
	}

	int sock = accept(fd, NULL, NULL);
	if (__builtin_expect(sock < 0, 0)) {
		log(TO_ALL, LOG_WARNING, "Connection couldn't be accepted.\n");
		return TRUE;
	}

	char buff[16384];
	ssize_t recv_size = 0;
	int valid_user = 0;

	struct iovec iov = { .iov_base = buff, .iov_len = sizeof(buff) };

	/* Allocate control message buffer for credentials */
	void *ctrl = g_malloc((gsize)CMSG_SPACE(sizeof(struct ucred)));
	if (ctrl == NULL) {
		log(TO_ALL, LOG_WARNING, "No memory for control message buffer.\n");
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
		log(TO_ALL, LOG_WARNING, "No control message; missing credentials.\n");
		goto out_close;
	}
	if ((cmsg->cmsg_level == SOL_SOCKET) && (cmsg->cmsg_type == SCM_CREDENTIALS)) {
		struct ucred *credentials = (struct ucred *)CMSG_DATA(cmsg);
		if (credentials != NULL && credentials->uid == 0) {
			valid_user = 1;
		}
	}
	if (__builtin_expect(!valid_user, 0)) {
		log(TO_ALL, LOG_INFO, "Permission denied for user to connect to socket.\n");
		goto out_close;
	}

	/* Commands */
	if (g_str_has_prefix(buff, "stats")) {
		char *stats = NULL;
		for_each_object(numa_nodes, get_object_stat, &stats);
		if (stats != NULL) {
			send(sock, stats, strlen(stats), 0);
			free(stats);
		}
	}

	if (g_str_has_prefix(buff, "settings ")) {
		const char *cmd = buff + strlen("settings ");

		if (g_str_has_prefix(cmd, "sleep ")) {
			const size_t offset = strlen("settings sleep ");
			if ((size_t)recv_size > offset) {
				char sleep_buf[32];
				size_t len = (size_t)recv_size - offset;
				if (len >= sizeof(sleep_buf)) {
					len = sizeof(sleep_buf) - 1;
				}
				memcpy(sleep_buf, buff + offset, len);
				sleep_buf[len] = '\0';

				errno = 0;
				unsigned long v = strtoul(sleep_buf, NULL, 10);
				if (errno == 0 && v >= 1UL && v <= (unsigned long)INT_MAX) {
					sleep_interval = (int)v;
					user_min_interval = sleep_interval; /* update user min on explicit request */
				}
			} else {
				log(TO_ALL, LOG_WARNING, "Invalid sleep command length.\n");
			}
		} else if (g_str_has_prefix(cmd, "ban irqs ")) {
			const size_t offset = strlen("settings ban irqs ");
			if ((size_t)recv_size > offset) {
				char irq_buf[256];
				size_t len = (size_t)recv_size - offset;
				if (len >= sizeof(irq_buf)) {
					len = sizeof(irq_buf) - 1;
				}
				memcpy(irq_buf, buff + offset, len);
				irq_buf[len] = '\0';

				/* Reset list, then parse space-separated integers robustly */
				g_list_free_full(cl_banned_irqs, free);
				cl_banned_irqs = NULL;
				need_rescan = 1;

				if (!g_str_has_prefix(irq_buf, "NONE")) {
					const char *cur = irq_buf;
					while (*cur != '\0') {
						while (isspace((unsigned char)*cur)) { cur++; }
						if (*cur == '\0') { break; }
						errno = 0;
						char *endp = NULL;
						unsigned long val = strtoul(cur, &endp, 10);
						if (cur == endp) {
							/* no conversion; stop parsing */
							break;
						}
						if (errno == 0 && val <= (unsigned long)INT_MAX) {
							add_cl_banned_irq((int)val);
						}
						cur = endp;
					}
				}
			} else {
				log(TO_ALL, LOG_WARNING, "Invalid ban irqs command length.\n");
			}
		} else if (g_str_has_prefix(cmd, "cpus ")) {
			/* Replace banned cpumask from UI */
			banned_cpumask_from_ui = NULL;
			free(cpu_ban_string);
			cpu_ban_string = NULL;

			const size_t offset = strlen("settings cpus ");
			if ((size_t)recv_size > offset) {
				size_t len = (size_t)recv_size - offset;
				cpu_ban_string = (char *)malloc(len + 1U);
				if (cpu_ban_string == NULL) {
					log(TO_ALL, LOG_WARNING, "Malloc failed for cpu_ban_string.\n");
					goto out_close;
				}
				memcpy(cpu_ban_string, buff + offset, len);
				cpu_ban_string[len] = '\0';

				/* Tokenize first token as cpumask string */
				char *saveptr = NULL;
				char *token = strtok_r(cpu_ban_string, " ", &saveptr);
				if (token != NULL && !g_str_has_prefix(token, "NULL")) {
					banned_cpumask_from_ui = token;
				} else {
					banned_cpumask_from_ui = NULL;
					free(cpu_ban_string);
					cpu_ban_string = NULL;
				}
				need_rescan = 1;
			} else {
				log(TO_ALL, LOG_WARNING, "Invalid cpus command length.\n");
			}
		}
	}

	if (g_str_has_prefix(buff, "setup")) {
		/* Build setup response using GString (avoid repeated realloc) */
		GString *gs = g_string_new(NULL);
		if (gs != NULL) {
			char banned[512];
			g_string_append_printf(gs, "SLEEP %d ", sleep_interval);
			if (g_list_length(cl_banned_irqs) > 0) {
				for_each_irq(cl_banned_irqs, append_setup_irq_gstring, gs);
			}
			cpumask_scnprintf(banned, sizeof(banned), banned_cpus);
			g_string_append_printf(gs, "BANNED %s", banned);
			send(sock, gs->str, gs->len, 0);
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

	/* Try file-based socket in tmpfs first */
	addr.sun_family = AF_UNIX;
	int n = snprintf(socket_name, sizeof(socket_name), "%s/%s%d.sock",
	                 SOCKET_TMPFS, SOCKET_PATH, getpid());
	if (n < 0 || (size_t)n >= sizeof(socket_name)) {
		log(TO_ALL, LOG_WARNING, "Socket path truncated.\n");
		/* Continue to abstract fallback */
	} else {
		/* Copy to sun_path (ensure NUL termination) */
		strncpy(addr.sun_path, socket_name, sizeof(addr.sun_path) - 1U);
		addr.sun_path[sizeof(addr.sun_path) - 1U] = '\0';

		/* Compute sockaddr length precisely */
		socklen_t len = (socklen_t)(offsetof(struct sockaddr_un, sun_path) + strlen(addr.sun_path) + 1U);
		if (bind(socket_fd, (struct sockaddr *)&addr, len) == 0) {
			int optval = 1;
			if (setsockopt(socket_fd, SOL_SOCKET, SO_PASSCRED, &optval, sizeof(optval)) < 0) {
				log(TO_ALL, LOG_WARNING, "Unable to set socket options.\n");
				close(socket_fd);
				return 1;
			}
			listen(socket_fd, 1);
			g_unix_fd_add(socket_fd, G_IO_IN, sock_handle, NULL);
			return 0;
		}
		log(TO_ALL, LOG_WARNING, "Daemon couldn't be bound to the file-based socket.\n");
	}

	/* Abstract socket fallback: leading NUL in sun_path; custom length */
	memset(&addr, 0, sizeof(addr));
	addr.sun_family = AF_UNIX;

	const char *aname = "irqbalance-ui";
	size_t alen = strlen(aname);
	if (alen + 1U > sizeof(addr.sun_path)) {
		log(TO_ALL, LOG_WARNING, "Abstract socket name too long.\n");
		close(socket_fd);
		return 1;
	}
	addr.sun_path[0] = '\0';
	memcpy(addr.sun_path + 1, aname, alen);

	socklen_t len = (socklen_t)(offsetof(struct sockaddr_un, sun_path) + 1U + alen);
	if (bind(socket_fd, (struct sockaddr *)&addr, len) < 0) {
		log(TO_ALL, LOG_WARNING, "Daemon couldn't be bound to the abstract socket, bailing out.\n");
		close(socket_fd);
		return 1;
	}

	int optval = 1;
	if (setsockopt(socket_fd, SOL_SOCKET, SO_PASSCRED, &optval, sizeof(optval)) < 0) {
		log(TO_ALL, LOG_WARNING, "Unable to set socket options.\n");
		close(socket_fd);
		return 1;
	}
	listen(socket_fd, 1);
	g_unix_fd_add(socket_fd, G_IO_IN, sock_handle, NULL);
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
	sigprocmask(SIG_BLOCK, &sigset, &old_sigset);

	parse_command_line(argc, argv);

	/*
	 * If run under systemd (v232+ sets INVOCATION_ID), enable journald
	 * logging and force foreground.
	 */
	if (getenv("INVOCATION_ID") != NULL) {
		journal_logging = 1;
		foreground_mode = 1;
	}

	/* Open syslog */
	openlog(argv[0], 0, LOG_DAEMON);

	if (getenv("IRQBALANCE_ONESHOT") != NULL) {
		one_shot_mode = 1;
	}

	if (getenv("IRQBALANCE_DEBUG") != NULL) {
		debug_mode = 1;
		foreground_mode = 1;
	}

	/* Console logging indentation */
	if (journal_logging) {
		log_indent = "....";
	} else {
		log_indent = "    ";
	}

	/* Default: no console logging unless debug */
	if (!debug_mode) {
		log_mask &= ~TO_CONSOLE;
	}

	if (numa_available() > -1) {
		numa_avail = 1;
	} else {
		log(TO_CONSOLE, LOG_INFO, "This machine seems not NUMA capable.\n");
	}

	if (geteuid() != 0) {
		log(TO_ALL, LOG_WARNING, "Irqbalance hasn't been executed under root privileges, thus it won't in fact balance interrupts.\n");
	}

	HZ = sysconf(_SC_CLK_TCK);
	if (HZ == -1) {
		log(TO_ALL, LOG_WARNING, "Unable to determine HZ; defaulting to 100\n");
		HZ = 100;
	}

	if (!foreground_mode) {
		int pidfd = -1;
		if (daemon(0, 0) != 0) {
			exit(EXIT_FAILURE);
		}
		/* Write pidfile to avoid multiple instances */
		if (pidfile != NULL) {
			pidfd = open(pidfile, O_WRONLY | O_CREAT | O_EXCL | O_TRUNC,
			             S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH);
			if (pidfd >= 0) {
				char str[32];
				int sn = snprintf(str, sizeof(str), "%u\n", (unsigned)getpid());
				if (sn > 0 && (size_t)sn < sizeof(str)) {
					ssize_t wr = write(pidfd, str, (size_t)sn);
					(void)wr;
				}
				close(pidfd);
			}
		}
	}

	build_object_tree();
	if (debug_mode) {
		dump_object_tree();
	}

	/* On single-core UP systems irqbalance has no work */
	if (num_online_cpus() <= 1) {
		const char *msg = "Balancing is ineffective on systems with a single CPU. Shutting down\n";
		log(TO_ALL, LOG_WARNING, "%s", msg);
		goto out;
	}

	/* Connect GLib to signals; unblock process signal mask */
	g_unix_signal_add(SIGINT,  handler, NULL);
	g_unix_signal_add(SIGTERM, handler, NULL);
	g_unix_signal_add(SIGUSR1, handler, NULL);
	g_unix_signal_add(SIGUSR2, handler, NULL);
	g_unix_signal_add(SIGHUP,  force_rescan, NULL);

	sigprocmask(SIG_SETMASK, &old_sigset, NULL);

#ifdef HAVE_LIBCAP_NG
	/* Drop capabilities early; we rely on root for initial setup, then reduce */
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

	if (init_thermal() != 0) {
		log(TO_ALL, LOG_WARNING, "Failed to initialize thermal events.\n");
	}

	main_loop = g_main_loop_new(NULL, FALSE);
	last_interval = sleep_interval;
	user_min_interval = sleep_interval; /* lock in true user minimum at startup */

	g_timeout_add_seconds(sleep_interval, scan, NULL);
	g_main_loop_run(main_loop);

	/* loop exited */
	g_main_loop_quit(main_loop);

out:
	deinit_thermal();
	free_object_tree();
	free_cl_opts();
	free(polscript);

	/* Remove pidfile */
	if (!foreground_mode && pidfile != NULL) {
		unlink(pidfile);
	}

#ifdef HAVE_IRQBALANCEUI
	/* Remove socket on exit */
	if (socket_fd > 0) {
		close(socket_fd);
	}
	if (socket_name[0] != '\0') {
		unlink(socket_name);
	}
#endif

	return ret;
}
