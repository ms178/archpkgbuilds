# CachyOS linux-cgroup-always – autoprofile function definitions
# Install to: /usr/share/fish/vendor_conf.d/20-cgterm-autoprofile-init.fish
#
# Loaded BEFORE 30-cgterm-*-profile.fish.  Defines helper functions
# needed by the auto-detection logic.  Must live in conf.d (not
# vendor_functions.d) because cgterm_autoprofile_by_capacity is
# called during conf.d startup, before lazy autoloading runs.
# ── Helper: compact an integer list into range notation ───────────
# e.g.  __cgterm_compact_int_list 0 1 2 3 8 9 10  →  "0-3,8-10"
#
# Canonical home.  A guarded fallback copy also exists in
# attach_shell_to_unique_cgroup.fish for contexts where conf.d
# has not been sourced (non-login shells).
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
# ── Auto-detect P/E core split via cpu_capacity ──────────────────
# Works on Intel hybrid CPUs (Alder Lake, Raptor Lake, Arrow Lake …).
# On non-hybrid CPUs (AMD Zen, older Intel) returns 1 silently and
# the profile variables stay unset – all later code falls back to
# "all online CPUs".
function cgterm_autoprofile_by_capacity
    set -l caps
    set -l maxcap ""
    for cpu_dir in /sys/devices/system/cpu/cpu[0-9]*
        test -d "$cpu_dir"; or continue
        set -l cpu_id (string replace -r '^.*/cpu' '' -- $cpu_dir)
        set -l cap_file "$cpu_dir/cpu_capacity"
        test -r "$cap_file"; or continue
        read -l cap < "$cap_file"; or continue
        string match --quiet --regex -- '^[0-9]+$' "$cap"; or continue
        set caps $caps "$cpu_id:$cap"
        if test -z "$maxcap" -o "$cap" -gt "$maxcap"
            set maxcap "$cap"
        end
    end
    if test (count $caps) -eq 0
        return 1
    end
    set -l perf
    set -l eco
    for pair in $caps
        set -l p (string split ':' -- $pair)
        if test "$p[2]" -eq "$maxcap"
            set perf $perf $p[1]
        else
            set eco $eco $p[1]
        end
    end
    if test (count $perf) -eq 0 -o (count $eco) -eq 0
        return 1
    end
    set -gx CGTERM_PERFORMANCE_CPUS (__cgterm_compact_int_list $perf)
    set -gx CGTERM_POWERSAVE_CPUS   (__cgterm_compact_int_list $eco)
end
