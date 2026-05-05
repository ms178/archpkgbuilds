# Source this after cgterm.fish.
# Auto-detect P/E split by cpu_capacity for hybrid Intel CPUs.

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
        echo "cgterm_autoprofile_by_capacity: cpu_capacity unavailable"
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
        echo "cgterm_autoprofile_by_capacity: could not split P/E cores"
        return 1
    end

    set -gx CGTERM_PERFORMANCE_CPUS (__cgterm_compact_int_list $perf)
    set -gx CGTERM_POWERSAVE_CPUS (__cgterm_compact_int_list $eco)

    echo "CGTERM_PERFORMANCE_CPUS=$CGTERM_PERFORMANCE_CPUS"
    echo "CGTERM_POWERSAVE_CPUS=$CGTERM_POWERSAVE_CPUS"
end
