# This file must be sourced in Fish.

set -g __CGTERM_UID (id -u)

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

function __cgterm_set_build_affinity_env
    # If profile vars exist, force heavy build tools to all cores.
    # This improves throughput for make/ninja/clang/rustc workloads.
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

function attach_shell_to_unique_cgroup
    if not status --is-interactive
        or test -n "$INVOCATION_ID"
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
        case alacritty kitty konsole qterminal screen "tmux: server" ghostty wezterm sshd-session
        case '*'
            return 0
    end

    # Default interactive shell affinity:
    # prefer P-cores if profile exists, else all online CPUs.
    set -l allowed
    if set -q CGTERM_PERFORMANCE_CPUS; and test -n "$CGTERM_PERFORMANCE_CPUS"
        set allowed "$CGTERM_PERFORMANCE_CPUS"
    else
        set allowed (__cgterm_online_cpus); or return 0
    end

    __cgterm_set_build_affinity_env

    set -l FISH (status fish-path)

    exec systemd-run -q --user --scope --unit="shell-$fish_pid" \
        -p Delegate=yes \
        -p CPUAccounting=yes \
        -p CPUWeight=100 \
        -p IOAccounting=yes \
        -p AllowedCPUs="$allowed" \
        -- "$FISH"
end

function cgterm_attach
    set -l arg $argv[1]
    set -l cgroup_line (__cgterm_read_cgroup_line); or return 1

    if test -n "$arg"
        if not string match --quiet --regex -- '^[0-9]$' "$arg"
            echo "invalid argument"
            return 1
        end

        set -l cgroot "/sys/fs/cgroup/user.slice/user-$__CGTERM_UID.slice"
        if not test -d "$cgroot/term-$arg"
            echo "cannot access the base cgroup 'term-$arg': not enabled"
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
        echo "cannot access cpuset.cpus: not available"
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
        echo "cannot apply change: attached to a base cgroup"
        return 1
    end

    if test "$arg" = "all"
        echo (__cgterm_online_cpus) > "$cpus_file"; or return 1
        return 0
    end

    if test "$arg" = "performance"
        if test -z "$CGTERM_PERFORMANCE_CPUS"
            echo "cannot apply change: CGTERM_PERFORMANCE_CPUS undefined"
            return 1
        end
        echo "$CGTERM_PERFORMANCE_CPUS" > "$cpus_file"; or return 1
        return 0
    end

    if test "$arg" = "powersave"
        if test -z "$CGTERM_POWERSAVE_CPUS"
            echo "cannot apply change: CGTERM_POWERSAVE_CPUS undefined"
            return 1
        end
        echo "$CGTERM_POWERSAVE_CPUS" > "$cpus_file"; or return 1
        return 0
    end

    echo "$arg" > "$cpus_file"; or return 1
end

function cgterm_memnodes
    set -l cgroup_line (__cgterm_read_cgroup_line); or return 1
    set -l cpath (__cgterm_cgroup_path "$cgroup_line"); or return 1
    set -l arg $argv[1]
    set -l mems_file "/sys/fs/cgroup/$cpath/cpuset.mems"
    set -l mems_effective "/sys/fs/cgroup/$cpath/cpuset.mems.effective"

    if not test -e "$mems_file"
        echo "cannot access cpuset.mems: not available"
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
        echo "cannot apply change: attached to a base cgroup"
        return 1
    end

    if test "$arg" = "all"
        echo (__cgterm_online_nodes) > "$mems_file"; or return 1
        return 0
    end

    echo "$arg" > "$mems_file"; or return 1
end

function cgterm_weight
    set -l cgroup_line (__cgterm_read_cgroup_line); or return 1
    set -l cpath (__cgterm_cgroup_path "$cgroup_line"); or return 1
    set -l arg $argv[1]

    if test -z "$arg"
        read -l w < "/sys/fs/cgroup/$cpath/cpu.weight"; or return 1
        echo "$w"
        return 0
    end

    if __cgterm_is_base_cgroup "$cgroup_line"
        echo "cannot apply change: attached to a base cgroup"
        return 1
    end

    if string match --quiet --regex -- '^[0-9]+$' "$arg"
        and test "$arg" -ge 1
        and test "$arg" -le 10000
        echo "$arg" > "/sys/fs/cgroup/$cpath/cpu.weight"; or return 1
    else
        echo "invalid argument"
        return 1
    end
end

function cgterm_compile_all
    # One-shot helper for compile-heavy sessions.
    # Expands current shell cgroup to all online cores.
    cgterm_cpus "all"
end

function make
    # For compile throughput, pin build jobs to all cores.
    set -l old (cgterm_cpus)
    if set -q CGTERM_BUILD_CPUS; and test -n "$CGTERM_BUILD_CPUS"
        cgterm_cpus "$CGTERM_BUILD_CPUS" >/dev/null 2>&1
    else
        cgterm_cpus "all" >/dev/null 2>&1
    end

    command make $argv
    set -l rc $status

    test -n "$old"; and cgterm_cpus "$old" >/dev/null 2>&1
    return $rc
end

function ninja
    set -l old (cgterm_cpus)
    if set -q CGTERM_BUILD_CPUS; and test -n "$CGTERM_BUILD_CPUS"
        cgterm_cpus "$CGTERM_BUILD_CPUS" >/dev/null 2>&1
    else
        cgterm_cpus "all" >/dev/null 2>&1
    end

    command ninja $argv
    set -l rc $status

    test -n "$old"; and cgterm_cpus "$old" >/dev/null 2>&1
    return $rc
end

attach_shell_to_unique_cgroup
