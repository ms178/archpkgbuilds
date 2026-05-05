# CachyOS linux-cgroup-always – per-terminal cgroup management
# Install to: /usr/share/fish/vendor_functions.d/attach_shell_to_unique_cgroup.fish
#
# This file must be sourced in Fish (happens automatically via
# vendor_functions.d autoloading when any function defined here is
# first called).
#
# Usage – add to ~/.config/fish/config.fish:
#     attach_shell_to_unique_cgroup
set -g __CGTERM_UID (id -u)
# ── Internal helpers ──────────────────────────────────────────────
function __cgterm_read_cgroup_line
    read -l cgroup_line < /proc/self/cgroup; or return 1
    echo $cgroup_line
end
function __cgterm_cgroup_path --argument cgroup_line
    if test -z "$cgroup_line"
        set cgroup_line (__cgterm_read_cgroup_line); or return 1
    end
    string replace -r '^.*::/' '' -- $cgroup_line
end
function __cgterm_is_base_cgroup --argument cgroup_line
    set -l re '^0::/user\.slice/user-'$__CGTERM_UID'\.slice/term-[0-9]+$'
    string match --quiet --regex -- $re -- "$cgroup_line"
end
function __cgterm_online_cpus
    read -l online < /sys/devices/system/cpu/online; or return 1
    echo (string trim -- $online)
end
function __cgterm_online_nodes
    if test -r /sys/devices/system/node/online
        read -l nodes < /sys/devices/system/node/online; and begin
            echo (string trim -- $nodes)
            return 0
        end
    end
    echo 0
end
function __cgterm_expand_ranges --argument ranges
    set -l out
    for part in (string split ',' -- $ranges)
        set part (string trim -- $part)
        if string match --quiet --regex -- '^[0-9]+-[0-9]+$' "$part"
            set -l pair (string split '-' -- $part)
            for i in (seq $pair[1] $pair[2])
                set out $out $i
            end
        else if string match --quiet --regex -- '^[0-9]+$' "$part"
            set out $out $part
        end
    end
    echo $out
end
# Guarded fallback – the canonical copy lives in
# cgterm-autoprofile-init.fish (conf.d/20-…).  This definition
# ensures the function is available in non-login shells where
# conf.d has not been sourced.
if not functions -q __cgterm_compact_int_list
    function __cgterm_compact_int_list
        set -l nums $argv
        if test (count $nums) -eq 0
            return 1
        end
        set nums (printf '%s\n' $nums | sort -n -u)
        set -l ranges
        set -l start $nums[1]
        set -l prev $nums[1]
        for n in $nums[2..-1]
            if test (math "$prev + 1") -eq $n
                set prev $n
            else
                if test $start -eq $prev
                    set ranges $ranges $start
                else
                    set ranges $ranges "$start-$prev"
                end
                set start $n
                set prev $n
            end
        end
        if test $start -eq $prev
            set ranges $ranges $start
        else
            set ranges $ranges "$start-$prev"
        end
        string join ',' -- $ranges
    end
end
function __cgterm_set_build_affinity_env
    # Combine P+E cores for heavy build tools (make/ninja/clang/rustc).
    if set -q CGTERM_PERFORMANCE_CPUS; and test -n "$CGTERM_PERFORMANCE_CPUS"
        if set -q CGTERM_POWERSAVE_CPUS; and test -n "$CGTERM_POWERSAVE_CPUS"
            set -gx CGTERM_BUILD_CPUS "$CGTERM_PERFORMANCE_CPUS,$CGTERM_POWERSAVE_CPUS"
        else
            set -gx CGTERM_BUILD_CPUS (__cgterm_online_cpus)
        end
    else
        set -gx CGTERM_BUILD_CPUS (__cgterm_online_cpus)
    end
end
function __cgterm_cleanup --on-event fish_exit
    if set -q OLDCGROUP_PID
        kill $OLDCGROUP_PID 2>/dev/null
        set -e OLDCGROUP_PID OLDCGROUP
    end
end
# ── Main entry point ──────────────────────────────────────────────
function attach_shell_to_unique_cgroup
    if not status --is-interactive
        or test -n "$INVOCATION_ID"
        return 0
    end
    # ── FIX (BUG 1): re-entry guard ──────────────────────────────
    # systemd-run --scope (without --pty/--pipe) uses execvp() —
    # the PID and ppid stay identical.  Without this guard the new
    # fish instance would re-match the terminal emulator, try to
    # create a scope with an already-taken unit name, and die.
    # Detect that we are already inside a per-shell scope.
    set -l _cg (__cgterm_read_cgroup_line 2>/dev/null)
    if string match --quiet --regex -- '/shell-[0-9]+\.scope$' -- "$_cg"
        return 0
    end
    # Safety: systemd-run must exist.
    if not command -q systemd-run
        return 0
    end
    set -l ppid (ps -o ppid= -p $fish_pid | string trim)
    if test -z "$ppid"
        return 0
    end
    if not test -r "/proc/$ppid/comm"
        return 0
    end
    read -l cmd < "/proc/$ppid/comm"; or return 0
    switch "$cmd"
        # ── Recognised terminal emulators / multiplexers ──────────
        #
        # FIX (BUG 2): added bare "tmux".  /proc/PID/comm contains
        # the kernel task name (≤15 chars).  tmux does *not* call
        # prctl(PR_SET_NAME); its comm is always "tmux", never
        # "tmux: server" (which is only an argv[0] rewrite visible
        # in ps output).  We keep the old value for the rare build
        # that does call PR_SET_NAME, plus "sshd" for older OpenSSH
        # that has not split into sshd-session.
        case alacritty foot ghostty kitty konsole qterminal screen tmux "tmux: server" wezterm sshd sshd-session
        case '*'
            return 0
    end
    # Default interactive-shell affinity:
    # prefer P-cores if a profile exists, else all online CPUs.
    set -l allowed
    if set -q CGTERM_PERFORMANCE_CPUS; and test -n "$CGTERM_PERFORMANCE_CPUS"
        set allowed "$CGTERM_PERFORMANCE_CPUS"
    else
        set allowed (__cgterm_online_cpus); or return 0
    end
    __cgterm_set_build_affinity_env
    set -l FISH (status fish-path)
    # Create a transient scope for this shell.
    #
    # Only resource-control properties are valid for scopes (exec
    # settings like Nice=, TimerSlackNSec=, CPUSchedulingPolicy=
    # are not).  Every -p line below is verified against
    # systemd.io/TRANSIENT-SETTINGS.
    #
    # FIX (BUG 6): added IOWeight=100.
    # FIX (BUG 7): added MemoryAccounting + TasksAccounting.
    exec systemd-run -q --user --scope --unit="shell-$fish_pid" \
        -p Delegate=yes \
        -p CPUAccounting=yes \
        -p CPUWeight=100 \
        -p IOAccounting=yes \
        -p IOWeight=100 \
        -p MemoryAccounting=yes \
        -p TasksAccounting=yes \
        -p AllowedCPUs="$allowed" \
        -- "$FISH"
end
# ── Interactive cgroup commands ───────────────────────────────────
function cgterm_attach
    set -l arg $argv[1]
    set -l cgroup_line (__cgterm_read_cgroup_line); or return 1
    if test -n "$arg"
        # FIX (BUG 5): '^[0-9]$' → '^[0-9]+$' to allow term-10 etc.
        if not string match --quiet --regex -- '^[0-9]+$' "$arg"
            echo "usage: cgterm_attach [N]" >&2
            return 1
        end
        set -l cgroot "/sys/fs/cgroup/user.slice/user-$__CGTERM_UID.slice"
        if not test -d "$cgroot/term-$arg"
            echo "cannot access base cgroup 'term-$arg': not enabled" >&2
            return 1
        end
        if not set -q OLDCGROUP
            set -gx OLDCGROUP "$cgroup_line"
            nohup sleep infinity >/dev/null 2>&1 &
            set -g OLDCGROUP_PID $last_pid
            disown $OLDCGROUP_PID >/dev/null 2>&1
        end
        echo "$fish_pid" > "$cgroot/term-$arg/cgroup.procs"; or return 1
    end
    cat /proc/self/cgroup
end
function cgterm_detach
    if set -q OLDCGROUP_PID
        set -l cpath (__cgterm_cgroup_path "$OLDCGROUP")
        if test -n "$cpath"
            echo "$fish_pid" > "/sys/fs/cgroup/$cpath/cgroup.procs"; or return 1
        end
        kill $OLDCGROUP_PID 2>/dev/null
        set -e OLDCGROUP_PID OLDCGROUP
    end
    cat /proc/self/cgroup
end
function cgterm_cpus
    set -l cgroup_line (__cgterm_read_cgroup_line); or return 1
    set -l cpath (__cgterm_cgroup_path "$cgroup_line"); or return 1
    set -l arg $argv[1]
    set -l cpus_file "/sys/fs/cgroup/$cpath/cpuset.cpus"
    set -l cpus_effective "/sys/fs/cgroup/$cpath/cpuset.cpus.effective"
    if not test -e "$cpus_file"
        echo "cpuset.cpus not available" >&2
        return 1
    end
    if test -z "$arg"
        read -l cpus < "$cpus_file"
        if test -z "$cpus" -a -r "$cpus_effective"
            read -l cpus < "$cpus_effective"
        end
        echo "$cpus"
        return 0
    end
    if __cgterm_is_base_cgroup "$cgroup_line"
        echo "cannot change: base cgroup" >&2
        return 1
    end
    switch "$arg"
        case all
            echo (__cgterm_online_cpus) > "$cpus_file"; or return 1
        case performance
            if test -z "$CGTERM_PERFORMANCE_CPUS"
                echo "CGTERM_PERFORMANCE_CPUS undefined" >&2
                return 1
            end
            echo "$CGTERM_PERFORMANCE_CPUS" > "$cpus_file"; or return 1
        case powersave
            if test -z "$CGTERM_POWERSAVE_CPUS"
                echo "CGTERM_POWERSAVE_CPUS undefined" >&2
                return 1
            end
            echo "$CGTERM_POWERSAVE_CPUS" > "$cpus_file"; or return 1
        case '*'
            echo "$arg" > "$cpus_file"; or return 1
    end
end
function cgterm_memnodes
    set -l cgroup_line (__cgterm_read_cgroup_line); or return 1
    set -l cpath (__cgterm_cgroup_path "$cgroup_line"); or return 1
    set -l arg $argv[1]
    set -l mems_file "/sys/fs/cgroup/$cpath/cpuset.mems"
    set -l mems_effective "/sys/fs/cgroup/$cpath/cpuset.mems.effective"
    if not test -e "$mems_file"
        echo "cpuset.mems not available" >&2
        return 1
    end
    if test -z "$arg"
        read -l nodes < "$mems_file"
        if test -z "$nodes" -a -r "$mems_effective"
            read -l nodes < "$mems_effective"
        end
        echo "$nodes"
        return 0
    end
    if __cgterm_is_base_cgroup "$cgroup_line"
        echo "cannot change: base cgroup" >&2
        return 1
    end
    if test "$arg" = all
        echo (__cgterm_online_nodes) > "$mems_file"; or return 1
        return 0
    end
    echo "$arg" > "$mems_file"; or return 1
end
function cgterm_weight
    set -l cgroup_line (__cgterm_read_cgroup_line); or return 1
    set -l cpath (__cgterm_cgroup_path "$cgroup_line"); or return 1
    set -l arg $argv[1]
    set -l weight_file "/sys/fs/cgroup/$cpath/cpu.weight"
    if test -z "$arg"
        read -l w < "$weight_file"; or return 1
        echo "$w"
        return 0
    end
    if __cgterm_is_base_cgroup "$cgroup_line"
        echo "cannot change: base cgroup" >&2
        return 1
    end
    if not string match --quiet --regex -- '^[0-9]+$' "$arg"
        echo "weight must be integer 1–10000" >&2
        return 1
    end
    if test "$arg" -lt 1 -o "$arg" -gt 10000
        echo "weight must be integer 1–10000" >&2
        return 1
    end
    echo "$arg" > "$weight_file"; or return 1
end
function cgterm_ioweight
    set -l cgroup_line (__cgterm_read_cgroup_line); or return 1
    set -l cpath (__cgterm_cgroup_path "$cgroup_line"); or return 1
    set -l arg $argv[1]
    set -l io_file "/sys/fs/cgroup/$cpath/io.weight"
    if not test -e "$io_file"
        echo "io.weight not available" >&2
        return 1
    end
    if test -z "$arg"
        # io.weight file format: "default NNN" or just "NNN"
        set -l raw (cat "$io_file" 2>/dev/null); or return 1
        # Extract the numeric value after "default " if present
        set -l w (string replace -r '^default\s+' '' -- $raw | string trim)
        echo "$w"
        return 0
    end
    if __cgterm_is_base_cgroup "$cgroup_line"
        echo "cannot change: base cgroup" >&2
        return 1
    end
    if not string match --quiet --regex -- '^[0-9]+$' "$arg"
        echo "weight must be integer 1–10000" >&2
        return 1
    end
    if test "$arg" -lt 1 -o "$arg" -gt 10000
        echo "weight must be integer 1–10000" >&2
        return 1
    end
    echo "$arg" > "$io_file"; or return 1
end
# ── Convenience mode switchers ────────────────────────────────────
function cgterm_build
    # Unlock all CPU cores and set balanced weights.
    # Ideal before: make -j(nproc), ninja, cargo build
    cgterm_cpus all; or return 1
    cgterm_weight 100
    cgterm_ioweight 100
    echo "build mode: all CPUs, weight 100/100"
end
function cgterm_game
    # Pin to P-cores and raise weights for low-latency gaming.
    # BORE scheduler benefits from reduced core contention on the
    # P-cluster while compile jobs run on E-cores elsewhere.
    if test -z "$CGTERM_PERFORMANCE_CPUS"
        echo "CGTERM_PERFORMANCE_CPUS undefined – run cgterm_autoprofile_by_capacity" >&2
        return 1
    end
    cgterm_cpus performance; or return 1
    cgterm_weight 300
    cgterm_ioweight 300
    echo "game mode: P-cores ($CGTERM_PERFORMANCE_CPUS), weight 300/300"
end
function cgterm_default
    # Restore the initial state set by attach_shell_to_unique_cgroup.
    set -l target
    if set -q CGTERM_PERFORMANCE_CPUS; and test -n "$CGTERM_PERFORMANCE_CPUS"
        set target "$CGTERM_PERFORMANCE_CPUS"
    else
        set target (__cgterm_online_cpus)
    end
    cgterm_cpus "$target"
    cgterm_weight 100
    cgterm_ioweight 100
    echo "default mode: CPUs $target, weight 100/100"
end
function cgterm_info
    set -l cgroup_line (__cgterm_read_cgroup_line); or return 1
    set -l cpath (__cgterm_cgroup_path "$cgroup_line"); or return 1
    set -l root "/sys/fs/cgroup/$cpath"
    echo "cgroup  : $cpath"
    if test -r "$root/cpu.weight"
        echo "cpu wt  : "(cat "$root/cpu.weight")
    end
    if test -r "$root/io.weight"
        echo "io wt   : "(cat "$root/io.weight")
    end
    if test -r "$root/cpuset.cpus.effective"
        echo "cpus    : "(cat "$root/cpuset.cpus.effective")
    else if test -r "$root/cpuset.cpus"
        echo "cpus    : "(cat "$root/cpuset.cpus")
    end
    if test -r "$root/cpuset.mems.effective"
        echo "mems    : "(cat "$root/cpuset.mems.effective")
    end
    if test -r "$root/memory.current"
        set -l bytes (cat "$root/memory.current")
        echo "mem     : "(math $bytes / 1048576)" MiB"
    end
    if test -r "$root/pids.current"
        echo "tasks   : "(cat "$root/pids.current")
    end
    if set -q CGTERM_PERFORMANCE_CPUS
        echo "P-cores : $CGTERM_PERFORMANCE_CPUS"
    end
    if set -q CGTERM_POWERSAVE_CPUS
        echo "E-cores : $CGTERM_POWERSAVE_CPUS"
    end
    if set -q CGTERM_BUILD_CPUS
        echo "build   : $CGTERM_BUILD_CPUS"
    end
end
