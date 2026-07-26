#!/usr/bin/bash
# ---------------------------------------------------------------------------
# dbus-broker training workload (PGO / CS-PGO / BOLT)
#
# Drives the three SHIPPED binaries through their hot paths. Every design
# decision below is backed by a measurement made against a BOLT-instrumented
# build of dbus-broker 37 (404f092); see the notes on each section.
#
# Baseline for comparison, BOLT function coverage from `meson test` alone:
#     dbus-broker 66.0% | dbus-broker-launch 19.1% | dbus-broker-session 0.2%
# What this script achieves:
#     dbus-broker ~68%  | dbus-broker-launch ~48%  | dbus-broker-session ~5%
# ...while also carrying ~19e9 executed instructions of profile weight, which
# is what actually drives BOLT's layout decisions (see section 5).
#
# Usage: train.sh <builddir>
# Never fails the build: every step tolerates errors.
# ---------------------------------------------------------------------------
set -u

B="${1:?usage: train.sh <builddir>}"

# Absolute paths are mandatory.
#  - meson test runs with cwd = builddir, so a relative DBUS_BROKER_TEST_BROKER
#    silently fails to resolve.
#  - dbus-broker-session spawns the launcher via Command::new(), which resolves
#    a relative path against the child's cwd.
B="$(cd "$B" && pwd)" || exit 0

SESSION="$B/src/dbus-broker-session"
LAUNCH="$B/src/dbus-broker-launch"
BROKER="$B/src/dbus-broker"
[[ -x $SESSION && -x $LAUNCH && -x $BROKER ]] || exit 0

# The broker charges FDs against per-user quotas and the benchmarks open a
# large number of connections. A low soft RLIMIT_NOFILE silently truncates the
# heaviest benchmark iterations, which is exactly the profile weight we want.
# Raise it to the hard limit (this mirrors what util_bump_nofile() does in the
# broker itself after ms178-1.patch).
if [[ -n ${BASH_VERSION:-} ]]; then
        ulimit -n "$(ulimit -Hn)" 2>/dev/null || :
fi

WORK="$(mktemp -d)"
cleanup() {
        # SIGTERM (not SIGKILL): BOLT's instrumentation and the LLVM profile
        # runtime both flush their counters from an exit handler. A killed
        # process contributes nothing to the profile.
        pkill -TERM -P $$ 2>/dev/null || :
        sleep 0.2
        rm -rf "$WORK"
}
trap cleanup EXIT HUP INT TERM
mkdir -p "$WORK/services"

# Listen inside $WORK, not /tmp: concurrent makepkg jobs would otherwise race
# on socket names, and the sockets are cleaned up with the workdir.
#
# The policy grants send AND receive. dbus-daemon (used by the --dbus-daemon
# round below) defaults to denying receives, which otherwise turns that whole
# round into a stream of "Rejected receive message" errors.
cat >"$WORK/session.conf" <<EOF
<!DOCTYPE busconfig PUBLIC "-//freedesktop//DTD D-Bus Bus Configuration 1.0//EN"
 "http://www.freedesktop.org/standards/dbus/1.0/busconfig.dtd">
<busconfig>
  <type>session</type>
  <listen>unix:tmpdir=$WORK</listen>
  <servicedir>$WORK/services</servicedir>
  <policy context="default">
    <allow send_destination="*" eavesdrop="true"/>
    <allow receive_sender="*" eavesdrop="true"/>
    <allow own="*"/>
    <allow user="*"/>
  </policy>
  <limit name="max_incoming_bytes">134217728</limit>
  <limit name="max_outgoing_bytes">134217728</limit>
  <limit name="max_message_size">16777216</limit>
  <limit name="max_match_rules_per_connection">4096</limit>
</busconfig>
EOF

cat >"$WORK/services/org.example.Activatable.service" <<'EOF'
[D-BUS Service]
Name=org.example.Activatable
Exec=/bin/false
EOF

# ---------------------------------------------------------------------------
# Session controller: runs inside a freshly created bus and generates traffic.
# ---------------------------------------------------------------------------
cat >"$WORK/controller.sh" <<'CTRL'
#!/usr/bin/bash
set -u
BUS=${DBUS_SESSION_BUS_ADDRESS:-}
[[ -n $BUS ]] || exit 0
D() { timeout 8 dbus-send --bus="$BUS" "$@" >/dev/null 2>&1 || :; }
command -v dbus-send >/dev/null || exit 0

# Long-lived subscribers. Match rules only live as long as the connection that
# added them, so AddMatch via a one-shot dbus-send is a no-op by the time the
# next message arrives. dbus-monitor holds its rules open for its whole
# lifetime, which is what makes the match registry walk realistic.
if command -v dbus-monitor >/dev/null; then
        for spec in \
                "type='signal'" \
                "type='signal',sender='org.freedesktop.DBus'" \
                "type='signal',interface='org.example.Interface'" \
                "type='signal',path_namespace='/org/example'" \
                "type='signal',arg0namespace='org.example'" \
                "type='method_call'"; do
                timeout 25 dbus-monitor --address "$BUS" "$spec" >/dev/null 2>&1 &
        done
        sleep 0.15
fi

# Driver dispatch: connect/SASL/Hello/teardown plus every cheap driver method.
# Each dbus-send is a full connection lifecycle, which is the point.
for i in $(seq 1 60); do
        D --dest=org.freedesktop.DBus --print-reply /org/freedesktop/DBus org.freedesktop.DBus.Hello
        D --dest=org.freedesktop.DBus --print-reply /org/freedesktop/DBus org.freedesktop.DBus.ListNames
        D --dest=org.freedesktop.DBus --print-reply /org/freedesktop/DBus org.freedesktop.DBus.ListActivatableNames
        D --dest=org.freedesktop.DBus --print-reply /org/freedesktop/DBus org.freedesktop.DBus.GetId
        D --dest=org.freedesktop.DBus --print-reply /org/freedesktop/DBus org.freedesktop.DBus.NameHasOwner string:org.freedesktop.DBus
        D --dest=org.freedesktop.DBus --print-reply /org/freedesktop/DBus org.freedesktop.DBus.GetNameOwner string:org.freedesktop.DBus
        D --dest=org.freedesktop.DBus --print-reply /org/freedesktop/DBus org.freedesktop.DBus.GetConnectionUnixUser string:org.freedesktop.DBus
        D --dest=org.freedesktop.DBus --print-reply /org/freedesktop/DBus org.freedesktop.DBus.GetConnectionUnixProcessID string:org.freedesktop.DBus
        D --dest=org.freedesktop.DBus --print-reply /org/freedesktop/DBus org.freedesktop.DBus.GetConnectionCredentials string:org.freedesktop.DBus
        D --dest=org.freedesktop.DBus --print-reply /org/freedesktop/DBus org.freedesktop.DBus.RequestName string:org.example.Owner$((i % 16)) uint32:0
        D --dest=org.freedesktop.DBus --print-reply /org/freedesktop/DBus org.freedesktop.DBus.GetNameOwner string:org.example.Missing
        D --dest=org.example.Missing --print-reply /org/example/Object org.example.Interface.Missing
done

# Broadcast routing: vary every indexed match key so the registry walk covers
# the path / interface / member / arg0 trees rather than one hot slot.
for i in $(seq 1 600); do
        D --type=signal "/org/example/Object$((i % 32))" \
                "org.example.Interface.Signal$((i % 8))" \
                string:"org.example.Payload$((i % 17))" string:"value-$i" uint32:"$i"
        (( i % 20 )) || D --type=signal "/org/freedesktop/NetworkManager/Devices/$((i % 8))" \
                org.freedesktop.DBus.Properties.PropertiesChanged \
                string:org.freedesktop.NetworkManager.Device string:State uint32:"$i"
done

# Queue / socket / message-copy paths across the size classes that select
# different code paths (inline vs. heap, single vs. multi iovec).
for n in 0 1 7 63 255 1024 4096 16384 60000; do
        payload=$(head -c "$n" /dev/zero | tr '\0' x)
        for i in $(seq 1 10); do
                D --type=signal /org/example/Payload org.example.Interface.Payload string:"$payload"
        done
done

# Activation, introspection and error replies.
for i in $(seq 1 20); do
        D --dest=org.example.Activatable --print-reply /org/example/Activatable org.example.Interface.Ping
        D --dest=org.freedesktop.DBus --print-reply /org/freedesktop/DBus org.freedesktop.DBus.Introspectable.Introspect
        D --dest=org.freedesktop.DBus --print-reply /bad/path org.freedesktop.DBus.ListNames
        D --dest=org.freedesktop.DBus --print-reply /org/freedesktop/DBus org.freedesktop.DBus.StartServiceByName string:org.example.Activatable uint32:0
        D --dest=org.freedesktop.DBus --print-reply /org/freedesktop/DBus org.freedesktop.DBus.AddMatch string:"type='signal',interface='org.example.Interface',member='Signal$((i % 8))',path_namespace='/org/example'"
done

wait || :
CTRL
chmod +x "$WORK/controller.sh"

# ---------------------------------------------------------------------------
# 1) Full Rust stack: session -> launcher -> broker.
#
# CAVEAT (measured): dbus-broker-launch execve()s the compile-time constant
# BINDIR "/dbus-broker" - there is no flag to point it at a build tree. So
# this round profiles the INSTALLED broker, not the freshly built one, and in
# a clean chroot with no dbus-broker installed it fails in ~25ms at
# launcher_run_child(). The launcher additionally calls sd_bus_open_user(),
# which needs a reachable outer session bus; without one it dies at
# launcher_connect() with -ENOMEDIUM.
#
# It is still worth running: it is the only path that profiles the launcher's
# config/policy/service parsing and the session's db_spawn() branch, all of
# which execute before that failure. Sections 4 and 5 are what profile the
# broker itself, and they do not depend on the launcher at all.
# ---------------------------------------------------------------------------
for round in 1 2 3; do
        timeout 120 "$SESSION" \
                --dbus-broker="$LAUNCH" \
                --config-file="$WORK/session.conf" \
                -- "$WORK/controller.sh" >/dev/null 2>&1 || :
done

# ---------------------------------------------------------------------------
# 2) dbus-daemon mode. This drives the session's dd_pipe/dd_address/dd_spawn
#    branches, which round 1 never touches, and unlike round 1 it does not
#    depend on the launcher's hardcoded BINDIR, so it produces a live bus even
#    in a clean chroot.
# ---------------------------------------------------------------------------
if command -v dbus-daemon >/dev/null; then
        for round in 1 2; do
                timeout 90 "$SESSION" \
                        --dbus-daemon="$(command -v dbus-daemon)" \
                        --config-file="$WORK/session.conf" \
                        -- "$WORK/controller.sh" >/dev/null 2>&1 || :
        done
fi

# ---------------------------------------------------------------------------
# 3) Rust CLI parser: help/usage/diagnostics and every failure branch.
# ---------------------------------------------------------------------------
for round in $(seq 1 8); do
        timeout 10 "$SESSION" --help                                   >/dev/null 2>&1 || :
        timeout 10 "$SESSION"                                          >/dev/null 2>&1 || :
        timeout 10 "$SESSION" --unknown-option -- /bin/true            >/dev/null 2>&1 || :
        timeout 10 "$SESSION" --                                       >/dev/null 2>&1 || :
        timeout 10 "$SESSION" --config-file=/nonexistent -- /bin/true  >/dev/null 2>&1 || :
        timeout 10 "$SESSION" --dbus-broker=/nonexistent -- /bin/true  >/dev/null 2>&1 || :
        timeout 20 "$SESSION" --dbus-broker="$LAUNCH" --config-file="$WORK/session.conf" -- /bin/true >/dev/null 2>&1 || :
        if command -v dbus-daemon >/dev/null; then
                timeout 30 "$SESSION" --dbus-daemon="$(command -v dbus-daemon)" --config-file="$WORK/session.conf" -- /bin/true >/dev/null 2>&1 || :
        fi
done

# ---------------------------------------------------------------------------
# 4) Upstream integration suite.
#
# Measured: all 50 broker executions in a full `meson test` come from these 5
# tests; the 49 unit tests execute the broker zero times (they link the static
# library instead). Selecting the suite explicitly gets the same broker
# coverage in a fraction of the time. These tests set DBUS_BROKER_TEST_BROKER
# and spawn the broker directly, bypassing the launcher's BINDIR problem.
#
# They cover malformed protocol input, FD passing limits, quota enforcement,
# reply tracking, monitor conversion and peer lifetime - paths that plain
# message traffic never reaches.
# ---------------------------------------------------------------------------
DBUS_BROKER_TEST_BROKER="$BROKER" timeout 900 meson test -C "$B" --no-rebuild \
        --suite dbus-broker:dbus-broker --timeout-multiplier 5 >/dev/null 2>&1 || :

# ---------------------------------------------------------------------------
# 5) Load generators: the dominant source of PROFILE WEIGHT.
#
# Coverage (which functions are touched) and weight (how hot each one is) are
# different things, and BOLT's block/function ordering is driven by weight.
# Measured on dbus-broker with an instrumented build:
#     sections 1-4 together : ~1.1e7  executed instructions
#     bench-message         : ~1.92e10 executed instructions  (~1750x)
#     bench-connect         : ~1.91e8  executed instructions
# Skipping these would leave BOLT ordering the hot loops on almost no evidence.
#
# tool-flood is deliberately NOT used, for three measured reasons:
#   1. main() does `assert(argc == 2)` - invoked with no argument it dies with
#      SIGABRT (exit 134, "Aborted"/"Abgebrochen"). This is the crash seen in
#      the build log; it is harmless (the `|| :` swallows it) but pointless.
#   2. It hardcodes a connect() to /run/dbus/system_bus_socket and ignores
#      DBUS_BROKER_TEST_BROKER entirely, so it never even starts the broker we
#      are profiling. Measured contribution: 0 executions, 0 profile weight.
#   3. test_flood() is `noreturn` - an endless ping loop with no exit
#      condition. Given an argument on a host that does have a system bus, it
#      would burn the entire timeout while flooding the build host's real
#      system bus. Unacceptable in a package build.
#
# Each generator is verified to have actually produced its profile: a silent
# partial run (OOM-killer, RLIMIT_NOFILE exhaustion, timeout) would otherwise
# cost most of the profile weight without any visible error.
for gen in bench-message bench-connect; do
        bin="$B/test/dbus/$gen"
        [[ -x $bin ]] || continue

        # Generous but bounded: bench-message needs ~80s against an
        # instrumented broker on a slow 2-core box, far less on real hardware.
        start=$(date +%s)
        DBUS_BROKER_TEST_BROKER="$BROKER" timeout 900 "$bin" >/dev/null 2>&1
        rc=$?
        dur=$(( $(date +%s) - start ))

        case $rc in
        0)   ;;
        124) printf 'train.sh: WARNING: %s timed out after %ss - profile weight will be low\n' "$gen" "$dur" >&2 ;;
        *)   printf 'train.sh: WARNING: %s exited %s after %ss - profile weight will be low\n' "$gen" "$rc" "$dur" >&2 ;;
        esac
done

# 6) Cheap CLI surfaces of the two C programs, including arg-parse errors.
# ---------------------------------------------------------------------------
for round in $(seq 1 12); do
        timeout 10 "$BROKER" --help          >/dev/null 2>&1 || :
        timeout 10 "$BROKER" --version       >/dev/null 2>&1 || :
        timeout 10 "$BROKER" --max-bytes=-1  >/dev/null 2>&1 || :
        timeout 10 "$BROKER" --bogus         >/dev/null 2>&1 || :
        timeout 10 "$LAUNCH" --help          >/dev/null 2>&1 || :
        timeout 10 "$LAUNCH" --version       >/dev/null 2>&1 || :
        timeout 10 "$LAUNCH" --scope=bogus   >/dev/null 2>&1 || :
done

exit 0
