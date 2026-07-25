#!/usr/bin/env fish
#
# install-irq.fish / pin-latency-irqs.fish
#
# Latency-oriented IRQ placement for a hybrid Intel gaming/desktop machine.
#
# ============================================================================
# STRATEGY (v6) -- why this is placed the way it is
# ============================================================================
#
# The goal on a gaming desktop is NOT "spread interrupts widely". It is:
#
#   1. Leave the cores the game actually runs on completely undisturbed.
#   2. Confine every interrupt to as few PHYSICAL cores as will absorb the
#      load, because an IRQ on one SMT thread steals from its sibling.
#   3. Never put latency-critical work on an E-core.
#
# Concretely on an i7-14700KF (8 P-cores SMT = cpu0..15, 12 E-cores = 16..27):
#
#   * ITMT/turbo "favored" cores (cpu8-11 on this part, confirmed by both
#     ananicy-cpp "Turbo cores: 8-11" and scx_lavd's preference order) are
#     RESERVED. No IRQ is ever placed there. That is where the game's hot
#     thread wants to be, and it is the only pair of cores that reaches the
#     top bin. v5 of this script put network IRQs on cpu8 -- a real
#     regression this version fixes.
#
#   * IRQ pools are built from P-core PRIMARIES only (one logical CPU per
#     physical core), lowest-numbered first, skipping the reserved turbo
#     set. Using a primary and never its sibling means the sibling stays
#     available to the scheduler at full speed.
#
#   * Network gets 2 CPUs by default, not 4. Measured from this machine:
#     the top 2 RSS queues carried 53% of all RX interrupts, and the total
#     NIC interrupt cost over a 3.6 GB transfer was ~2-6 CPU-seconds. RSS
#     hashes by flow, and a desktop has few concurrent flows, so extra net
#     CPUs sit idle while widening the blast radius across physical cores.
#     Raise it with --net-cpus if you actually run many-flow workloads.
#
#   * Queue vectors of one NIC are spread across the net pool; the link /
#     "other" vector is parked on the misc CPU. Each NIC gets an independent
#     rotation so two cards do not stack onto the same CPU.
#
#   * XPS is programmed so a TX queue completes on the same CPU that owns
#     its IRQ, which keeps the completion, the socket and the skb on one
#     cache. RPS is deliberately left OFF: with 8 hardware RSS queues it
#     only adds IPIs and jitter.
#
#   * irqbalance is told to leave the whole managed CPU set alone via
#     IRQBALANCE_BANNED_CPULIST, not just --banmod. --banmod only protects
#     the listed modules' IRQs; every *other* device in the system was
#     still free to land on our carefully chosen cores.
#
#   * Managed (kernel-affinity) IRQs -- NVMe queues, some MSI-X -- cannot be
#     moved from userspace. They are detected and skipped instead of being
#     reported as failures.
#
# ============================================================================
# FIXES since v5.0.0
# ============================================================================
#   C1  Turbo/ITMT favored cores are detected and reserved for the game.
#   C2  SMT-sibling awareness: pools use physical-core primaries and report
#       how many physical cores are disturbed.
#   C3  Default net pool 4 -> 2 CPUs (evidence above).
#   C4  Managed IRQs detected and skipped, not counted as failures.
#   C5  irqbalance IRQBALANCE_BANNED_CPULIST covers the whole pinned set.
#   C6  XPS programmed per TX queue to match IRQ placement; RPS left off.
#   C7  Honours isolcpus / nohz_full / irqaffinity from the kernel cmdline.
#   C8  --profile gaming|throughput|balanced presets.
#   C9  Idempotent: re-running produces identical placement (stable sort).
#   C10 --dry-run everywhere, plus a --explain mode that shows the reasoning.
#
# v5.0.0 fixed: r8169 queue naming, fish-4 read-only `_`, per-NIC interface
# filtering, carrier-aware autodetect, per-NIC round-robin, E-core exclusion,
# WAIT_* validation, and driver-list re-detection.
#
set -gx LC_ALL C

set -g SCRIPT_VERSION "6.0.0"

# SYSROOT exists so the logic can be exercised against a synthetic sysfs in
# tests. Empty in production; costs nothing.
if not set -q IRQPIN_SYSROOT
    set -gx IRQPIN_SYSROOT ""
end
set -g SYS "$IRQPIN_SYSROOT/sys"
set -g PROC "$IRQPIN_SYSROOT/proc"

set -g CONFIG_DIR "/etc/irqbalance.d"
set -g CONFIG_FILE "$CONFIG_DIR/gaming-latency.conf"
set -g RUNTIME_SCRIPT "/usr/local/sbin/pin-latency-irqs.fish"
set -g SYSTEMD_DIR "/etc/systemd/system"
set -g UDEV_RULES_DIR "/etc/udev/rules.d"
set -g IRQBALANCE_DROPIN_DIR "$SYSTEMD_DIR/irqbalance.service.d"
set -g IRQBALANCE_DROPIN "$IRQBALANCE_DROPIN_DIR/10-gaming-latency.conf"

set -g _quiet 0
set -g _debug 0
set -g _no_wait 0
set -g _dry_run 0
set -g _explain 0

set -g opt_action ""
set -g opt_iface ""
set -g opt_cpus ""
set -g opt_drivers
set -g opt_interfaces
set -g opt_gpu_cpus ""
set -g opt_io_cpus ""
set -g opt_net_cpus ""
set -g opt_reserve_cpu0 ""
set -g opt_pcore_only ""
set -g opt_profile ""
set -g opt_game_cpus ""
set -g opt_xps ""

set -g cfg_cpus ""
set -g cfg_drivers
set -g cfg_interfaces
set -g cfg_gpu_cpus ""
set -g cfg_io_cpus ""
set -g cfg_net_cpus ""
set -g cfg_reserve_cpu0 ""
set -g cfg_pcore_only ""
set -g cfg_profile ""
set -g cfg_game_cpus ""
set -g cfg_xps ""
set -g cfg_wait_retries ""
set -g cfg_wait_interval ""

set -g EFF_IFACE ""
set -g EFF_CPUS
set -g EFF_DRIVERS
set -g EFF_INTERFACES
set -g EFF_GPU_CPUS "1"
set -g EFF_IO_CPUS "2"
set -g EFF_NET_CPUS "2"
set -g EFF_RESERVE_CPU0 "yes"
set -g EFF_PCORE_ONLY "yes"
set -g EFF_PROFILE "gaming"
set -g EFF_GAME_CPUS ""
set -g EFF_XPS "yes"
set -g EFF_WAIT_RETRIES "12"
set -g EFF_WAIT_INTERVAL "1"

set -g POOL_ALL
set -g POOL_MISC
set -g POOL_GPU
set -g POOL_IO
set -g POOL_NET
set -g POOL_GAME

set -g PRESENT_GPU 0
set -g PRESENT_IO 0
set -g PRESENT_NET 0
set -g PRESENT_MISC 0

set -g RR_KEYS
set -g RR_VALS

# Records how each IRQ was placed, for XPS and for --explain.
set -g PLACED_IRQ
set -g PLACED_CPU
set -g PLACED_IFACE
set -g PLACED_KIND
set -g PLACED_QIDX

# ---------------------------------------------------------------- logging

function default_action
    set -l base (basename (status filename))
    if test "$base" = "pin-latency-irqs.fish"
        echo "verify"
    else
        echo "install"
    end
end

function log_info --argument-names msg
    if test "$_quiet" != "1"
        echo "[irq-pin] $msg"
    end
end

function log_warn --argument-names msg
    echo "[irq-pin] WARN: $msg" >&2
end

function log_debug --argument-names msg
    if test "$_debug" = "1"
        echo "[irq-pin] DEBUG: $msg" >&2
    end
end

function die --argument-names msg
    echo "[irq-pin] ERROR: $msg" >&2
    exit 1
end

function need_cmd --argument-names cmd
    type -q -- "$cmd"
    or die "Missing required command: $cmd"
end

function require_root
    if test (id -u) -ne 0
        die "Run as root"
    end
end

function self_path
    set -l path (status filename)
    if string match -q '/*' -- "$path"
        echo "$path"
        return 0
    end
    if type -q realpath
        realpath "$path"
        return 0
    end
    readlink -f "$path"
end

function is_uint --argument-names v
    string match -qr '^[0-9]+$' -- "$v"
end

# ------------------------------------------------------------ cpu helpers

function list_cpu_dirs
    find "$SYS/devices/system/cpu" -maxdepth 1 -type d -name 'cpu[0-9]*' 2>/dev/null | sort -V
end

function cpu_is_online --argument-names cpu_dir
    set -l online_file "$cpu_dir/online"
    if test -r "$online_file"
        test (string trim (cat "$online_file")) = "1"
        return $status
    end
    return 0
end

function expand_numeric_cpulist --argument-names cpulist
    set -l out
    for seg in (string split ',' -- (string trim -- "$cpulist"))
        set seg (string trim -- "$seg")
        if string match -qr '^[0-9]+-[0-9]+$' -- "$seg"
            set -l ab (string split '-' -- "$seg")
            if test "$ab[1]" -le "$ab[2]"
                for cpu in (seq "$ab[1]" "$ab[2]")
                    set out $out "$cpu"
                end
            end
        else if string match -qr '^[0-9]+$' -- "$seg"
            set out $out "$seg"
        end
    end
    if test (count $out) -gt 0
        printf "%s\n" $out
    end
end

function unique_sorted
    set -l filtered
    for v in $argv
        test -n "$v"
        and set filtered $filtered "$v"
    end
    test (count $filtered) -eq 0
    and return 0
    printf "%s\n" $filtered | sort -n -u
end

function collapse_cpulist
    set -l filtered
    for v in $argv
        test -n "$v"
        and set filtered $filtered "$v"
    end
    if test (count $filtered) -eq 0
        echo "(none)"
        return 0
    end
    set -l arr (unique_sorted $filtered)
    set -l out
    set -l rs $arr[1]
    set -l re $arr[1]
    for cpu in $arr[2..-1]
        if test (math "$cpu - $re") -eq 1
            set re $cpu
        else
            if test "$rs" -eq "$re"
                set out $out "$rs"
            else
                set out $out "$rs-$re"
            end
            set rs $cpu
            set re $cpu
        end
    end
    if test "$rs" -eq "$re"
        set out $out "$rs"
    else
        set out $out "$rs-$re"
    end
    string join ',' $out
end

function list_subtract
    # usage: list_subtract "a b c" -- removes $argv[2..] from $argv[1] list
    set -l base (string split ' ' -- "$argv[1]")
    set -l rm $argv[2..-1]
    set -l out
    for c in $base
        test -n "$c"
        or continue
        contains -- "$c" $rm
        or set out $out "$c"
    end
    if test (count $out) -gt 0
        printf "%s\n" $out
    end
end

function take_first_n --argument-names n
    set -l arr $argv[2..-1]
    set -l cnt (count $arr)
    if test "$n" -le 0; or test "$cnt" -eq 0
        return 0
    end
    test "$n" -gt "$cnt"
    and set n "$cnt"
    printf "%s\n" $arr[1..$n]
end

function drop_first_n --argument-names n
    set -l arr $argv[2..-1]
    set -l cnt (count $arr)
    if test "$cnt" -eq 0; or test "$n" -ge "$cnt"
        return 0
    end
    set -l start (math "$n + 1")
    printf "%s\n" $arr[$start..$cnt]
end

function primary_thread_cpu --argument-names cpu_dir
    set -l cpu (string replace -r '.*/cpu([0-9]+)$' '$1' -- "$cpu_dir")
    set -l sib_file "$cpu_dir/topology/thread_siblings_list"
    if not test -r "$sib_file"
        echo "$cpu"
        return 0
    end
    set -l first_seg (string split ',' -- (string trim (cat "$sib_file")))[1]
    if string match -qr '^[0-9]+-[0-9]+$' -- "$first_seg"
        echo (string split '-' -- "$first_seg")[1]
    else
        echo "$first_seg"
    end
end

function cpu_thread_sibling_count --argument-names cpu_dir
    set -l sib_file "$cpu_dir/topology/thread_siblings_list"
    if not test -r "$sib_file"
        echo 1
        return 0
    end
    set -l sibs (expand_numeric_cpulist (string trim (cat "$sib_file")))
    if test (count $sibs) -eq 0
        echo 1
    else
        count $sibs
    end
end

function cpu_siblings --argument-names cpu
    set -l f "$SYS/devices/system/cpu/cpu$cpu/topology/thread_siblings_list"
    if test -r "$f"
        expand_numeric_cpulist (string trim (cat "$f"))
    else
        echo "$cpu"
    end
end

# How many distinct physical cores a set of logical CPUs touches. Used to
# report (and minimise) the real blast radius of IRQ placement.
function physical_core_count
    set -l seen
    for cpu in $argv
        set -l p (primary_thread_cpu "$SYS/devices/system/cpu/cpu$cpu")
        contains -- "$p" $seen
        or set seen $seen "$p"
    end
    count $seen
end

function detect_pcore_primaries
    set -l cpu_dirs (list_cpu_dirs)
    test (count $cpu_dirs) -gt 0
    or return 1

    set -l by_type
    set -l saw_core_type 0
    for cpu_dir in $cpu_dirs
        cpu_is_online "$cpu_dir"
        or continue
        set -l ctype_file "$cpu_dir/topology/core_type"
        test -r "$ctype_file"
        or continue
        set saw_core_type 1
        set -l ctype (string lower -- (string trim (cat "$ctype_file")))
        switch "$ctype"
            case '2' 'core' 'performance' 'pcore' 'p-core'
                set -l primary (primary_thread_cpu "$cpu_dir")
                contains -- "$primary" $by_type
                or set by_type $by_type "$primary"
            case '*'
                continue
        end
    end
    if test "$saw_core_type" -eq 1
        if test (count $by_type) -gt 0
            unique_sorted $by_type
            return 0
        end
        return 1
    end

    set -l smt_primary
    for cpu_dir in $cpu_dirs
        cpu_is_online "$cpu_dir"
        or continue
        set -l cpu (string replace -r '.*/cpu([0-9]+)$' '$1' -- "$cpu_dir")
        set -l primary (primary_thread_cpu "$cpu_dir")
        test "$cpu" = "$primary"
        or continue
        if test (cpu_thread_sibling_count "$cpu_dir") -gt 1
            contains -- "$cpu" $smt_primary
            or set smt_primary $smt_primary "$cpu"
        end
    end
    if test (count $smt_primary) -gt 0
        unique_sorted $smt_primary
        return 0
    end
    return 1
end

function online_cpus
    set -l out
    for cpu_dir in (list_cpu_dirs)
        cpu_is_online "$cpu_dir"
        or continue
        set out $out (string replace -r '.*/cpu([0-9]+)$' '$1' -- "$cpu_dir")
    end
    unique_sorted $out
end

function cpu_max_perf --argument-names cpu
    # Prefer ACPI CPPC highest_perf (this is exactly what ITMT ranks on),
    # fall back to cpufreq max frequency.
    set -l f "$SYS/devices/system/cpu/cpu$cpu/acpi_cppc/highest_perf"
    if test -r "$f"
        set -l v (string trim (cat "$f" 2>/dev/null))
        is_uint "$v"
        and echo "$v"
        and return 0
    end
    set f "$SYS/devices/system/cpu/cpu$cpu/cpufreq/cpuinfo_max_freq"
    if test -r "$f"
        set -l v (string trim (cat "$f" 2>/dev/null))
        is_uint "$v"
        and echo "$v"
        and return 0
    end
    echo "0"
end

# C1: ITMT / turbo "favored" cores. On Raptor Lake a subset of P-cores
# reaches a higher max bin than the rest; the scheduler prefers them and so
# does the game. Return the primaries of those cores.
function detect_turbo_primaries
    set -l prim (detect_pcore_primaries)
    test (count $prim) -gt 0
    or return 1
    set -l best 0
    for cpu in $prim
        set -l v (cpu_max_perf "$cpu")
        test "$v" -gt "$best"
        and set best "$v"
    end
    test "$best" -gt 0
    or return 1
    set -l out
    for cpu in $prim
        test (cpu_max_perf "$cpu") -eq "$best"
        and set out $out "$cpu"
    end
    # Only meaningful if it is a strict subset; if every core reports the
    # same value there are no favored cores to protect.
    if test (count $out) -eq (count $prim)
        return 1
    end
    if test (count $out) -gt 0
        unique_sorted $out
        return 0
    end
    return 1
end

# C7: respect kernel-level isolation directives.
function cmdline_cpulist --argument-names key
    set -l f "$PROC/cmdline"
    test -r "$f"
    or return 1
    set -l cl (cat "$f" 2>/dev/null)
    for tok in (string split ' ' -- "$cl")
        if string match -q "$key=*" -- "$tok"
            set -l v (string replace "$key=" '' -- "$tok")
            # strip non-numeric qualifiers such as nohz_full=managed_irq,...
            set -l parts
            for p in (string split ',' -- "$v")
                string match -qr '^[0-9]+(-[0-9]+)?$' -- "$p"
                and set parts $parts "$p"
            end
            test (count $parts) -gt 0
            or return 1
            expand_numeric_cpulist (string join ',' $parts)
            return 0
        end
    end
    return 1
end

function isolated_cpus
    set -l out
    for key in isolcpus nohz_full
        for c in (cmdline_cpulist "$key" 2>/dev/null)
            contains -- "$c" $out
            or set out $out "$c"
        end
    end
    if test (count $out) -gt 0
        unique_sorted $out
    end
end

# ------------------------------------------------------- network topology

function iface_driver --argument-names ifc
    set -l link "$SYS/class/net/$ifc/device/driver"
    if test -L "$link"
        basename (readlink -f "$link")
        return 0
    end
    return 1
end

function iface_has_carrier --argument-names ifc
    set -l f "$SYS/class/net/$ifc/carrier"
    if test -r "$f"
        test (string trim (cat "$f" 2>/dev/null)) = "1"
        return $status
    end
    return 1
end

function all_net_ifaces
    for p in (find "$SYS/class/net" -maxdepth 1 -mindepth 1 2>/dev/null | sort)
        set -l ifc (basename "$p")
        test "$ifc" = "lo"
        and continue
        string match -qr '^[A-Za-z][A-Za-z0-9_.-]*$' -- "$ifc"
        or continue
        echo "$ifc"
    end
end

function detect_default_iface
    for ifc in (all_net_ifaces)
        if iface_has_carrier "$ifc"
            echo "$ifc"
            return 0
        end
    end
    for ifc in (all_net_ifaces)
        if iface_driver "$ifc" >/dev/null 2>&1
            echo "$ifc"
            return 0
        end
    end
    return 1
end

function detect_all_net_ifaces
    set -l out
    for ifc in (all_net_ifaces)
        set -l drv (iface_driver "$ifc" 2>/dev/null)
        test -n "$drv"
        or continue
        if is_network_driver "$drv"
            set out $out "$ifc"
        end
    end
    if test (count $out) -gt 0
        printf "%s\n" $out
    end
end

function driver_has_devices --argument-names drv
    set -l p "$SYS/bus/pci/drivers/$drv"
    if test -d "$p"
        set -l devs (find "$p" -maxdepth 1 -mindepth 1 -name '0000:*' 2>/dev/null)
        test (count $devs) -gt 0
        and return 0
    end
    return 1
end

function detect_default_drivers
    set -l out
    for drv in xhci_hcd amdgpu nvidia i915 xe snd_hda_intel ixgbe igc igb r8169 e1000e i40e ice atlantic
        if driver_has_devices "$drv"
            set out $out "$drv"
        else if test -d "$SYS/module/$drv"
            if grep -q -F -- "$drv" "$PROC/interrupts" 2>/dev/null
                set out $out "$drv"
            end
        end
    end
    test (count $out) -eq 0
    and return 1
    printf "%s\n" $out | sort -u
end

function is_network_driver --argument-names drv
    switch "$drv"
        case 'ixgbe' 'igc' 'igb' 'r8169' 'e1000e' 'i40e' 'ice' 'atlantic' 'tg3' 'bnxt_en' 'mlx5_core'
            return 0
        case '*'
            return 1
    end
end

function is_gpu_driver --argument-names drv
    switch "$drv"
        case 'amdgpu' 'nvidia' 'i915' 'xe' 'radeon'
            return 0
        case '*'
            return 1
    end
end

function is_io_driver --argument-names drv
    switch "$drv"
        case 'xhci_hcd' 'snd_hda_intel' 'ahci'
            return 0
        case '*'
            return 1
    end
end

# ------------------------------------------------------------ IRQ helpers

function irq_name_full --argument-names irq
    set -l line (grep -E "^[[:space:]]*$irq:" "$PROC/interrupts" 2>/dev/null | head -n 1)
    if test -z "$line"
        echo "unknown"
        return 0
    end
    string replace -r '^[[:space:]]*[0-9]+:[[:space:]]+([0-9]+[[:space:]]+)+(.+)$' '$2' -- "$line"
end

# Recognise per-queue vectors across driver naming conventions:
#   ixgbe   enp4s0-TxRx-0      igc  enp1s0-rx-0
#   r8169   enp5s0-rx0/-tx1    mlx5 mlx5_comp0@pci:...
function is_network_queue_irq_name --argument-names name
    set -l lname (string lower -- "$name")
    if string match -qr '(txrx|rxtx|[-_]rx|[-_]tx|rxq|txq|comp)[-_]?[0-9]+' -- "$lname"
        return 0
    end
    if string match -qr '(txrx|rxtx)' -- "$lname"
        return 0
    end
    return 1
end

# Direction + queue index of a NIC vector, so TX vectors can drive XPS.
# echoes "<kind> <index>", kind in rx|tx|txrx|other
function irq_queue_info --argument-names name
    set -l l (string lower -- "$name")
    set -l idx (string replace -r '^.*?([0-9]+)[^0-9]*$' '$1' -- "$l")
    is_uint "$idx"
    or set idx "0"
    if string match -qr '(txrx|rxtx)' -- "$l"
        echo "txrx $idx"
    else if string match -qr '[-_]tx[-_]?[0-9]+' -- "$l"
        echo "tx $idx"
    else if string match -qr '[-_]rx[-_]?[0-9]+' -- "$l"
        echo "rx $idx"
    else
        echo "other 0"
    end
end

function irq_iface --argument-names irq
    set -l name (irq_name_full "$irq")
    for ifc in (all_net_ifaces)
        if string match -qr '(^|[^a-z0-9])'"$ifc"'([^a-z0-9]|$)' -- "$name"
            echo "$ifc"
            return 0
        end
    end
    return 1
end

# C4: kernel-managed IRQs cannot be re-affined from userspace. Writing to
# them returns EIO. Detect up-front so they are skipped cleanly rather than
# inflating the failure count.
function irq_is_managed --argument-names irq
    set -l dbg "$SYS/kernel/debug/irq/irqs/$irq"
    if test -r "$dbg"
        if grep -qi 'managed' "$dbg" 2>/dev/null
            return 0
        end
    end
    return 1
end

function irq_matches_interface_filters --argument-names irq drv
    is_network_driver "$drv"
    or return 0
    test (count $EFF_INTERFACES) -gt 0
    or return 0
    set -l ifc (irq_iface "$irq" 2>/dev/null)
    test -z "$ifc"
    and return 0
    contains -- "$ifc" $EFF_INTERFACES
    and return 0
    return 1
end

function discover_irqs_for_driver --argument-names drv
    set -l irqs
    set -l drv_path "$SYS/bus/pci/drivers/$drv"
    if test -d "$drv_path"
        for dev in (find "$drv_path" -maxdepth 1 -mindepth 1 -name '0000:*' 2>/dev/null | sort)
            test -d "$dev/msi_irqs"
            or continue
            for irq_path in (find "$dev/msi_irqs" -maxdepth 1 -mindepth 1 2>/dev/null | sort -V)
                set -l irq (basename "$irq_path")
                string match -qr '^[0-9]+$' -- "$irq"
                or continue
                grep -q -E "^[[:space:]]*$irq:" "$PROC/interrupts" 2>/dev/null
                or continue
                irq_matches_interface_filters "$irq" "$drv"
                or continue
                contains -- "$irq" $irqs
                or set irqs $irqs "$irq"
            end
        end
    end
    for line in (grep -F -- "$drv" "$PROC/interrupts" 2>/dev/null)
        set -l irq (string replace -r '^[[:space:]]*([0-9]+):.*' '$1' -- "$line")
        string match -qr '^[0-9]+$' -- "$irq"
        or continue
        irq_matches_interface_filters "$irq" "$drv"
        or continue
        contains -- "$irq" $irqs
        or set irqs $irqs "$irq"
    end
    if test (count $irqs) -gt 0
        printf "%s\n" $irqs | sort -n -u
    end
end

function discover_all_irqs
    set -l out
    for drv in $EFF_DRIVERS
        for irq in (discover_irqs_for_driver "$drv")
            contains -- "$irq" $out
            or set out $out "$irq"
        end
    end
    if test (count $out) -gt 0
        printf "%s\n" $out | sort -n -u
    end
end

function wait_for_irqs
    for attempt in (seq 1 "$EFF_WAIT_RETRIES")
        test (count (discover_all_irqs)) -gt 0
        and return 0
        sleep "$EFF_WAIT_INTERVAL"
    end
    return 1
end

function cpu_to_hexmask --argument-names cpu
    set -l idx (math "floor($cpu / 32) + 1")
    set -l groups
    for i in (seq 1 "$idx")
        set groups $groups 0
    end
    set groups[$idx] (math "2^($cpu % 32)")
    set -l out
    for i in (seq (count $groups) -1 1)
        set out $out (printf '%08x' "$groups[$i]")
    end
    string join ',' $out
end

function set_irq_cpu --argument-names irq cpu
    test "$_dry_run" = "1"
    and return 0
    set -l list_file "$PROC/irq/$irq/smp_affinity_list"
    set -l mask_file "$PROC/irq/$irq/smp_affinity"
    if test -w "$list_file"
        printf '%s' "$cpu" > "$list_file" 2>/dev/null
        and return 0
    end
    if test -w "$mask_file"
        printf '%s' (cpu_to_hexmask "$cpu") > "$mask_file" 2>/dev/null
        and return 0
    end
    return 1
end

# C6: XPS -- make the TX completion land on the CPU that owns the TX IRQ.
function set_xps --argument-names ifc queue cpu
    set -l f "$SYS/class/net/$ifc/queues/tx-$queue/xps_cpus"
    test -w "$f"
    or return 1
    test "$_dry_run" = "1"
    and return 0
    printf '%s' (cpu_to_hexmask "$cpu") > "$f" 2>/dev/null
end

function clear_rps --argument-names ifc
    # With hardware RSS, RPS only adds IPIs. Ensure it is off.
    for f in (find "$SYS/class/net/$ifc/queues" -name rps_cpus 2>/dev/null)
        test -w "$f"
        or continue
        test "$_dry_run" = "1"
        and continue
        printf '0' > "$f" 2>/dev/null
    end
end

# ----------------------------------------------------------------- config

function parse_config_value --argument-names key val
    switch "$key"
        case 'LATENCY_CPUS'
            set -g cfg_cpus "$val"
        case 'PIN_DRIVERS'
            test -n "$val"
            and set -g cfg_drivers (string split ' ' -- "$val")
        case 'PIN_INTERFACES'
            test -n "$val"
            and set -g cfg_interfaces (string split ' ' -- "$val")
        case 'GPU_CPUS'
            set -g cfg_gpu_cpus "$val"
        case 'IO_CPUS'
            set -g cfg_io_cpus "$val"
        case 'NET_CPUS'
            set -g cfg_net_cpus "$val"
        case 'RESERVE_CPU0'
            set -g cfg_reserve_cpu0 (string lower -- "$val")
        case 'PCORE_ONLY'
            set -g cfg_pcore_only (string lower -- "$val")
        case 'PROFILE'
            set -g cfg_profile (string lower -- "$val")
        case 'GAME_CPUS'
            set -g cfg_game_cpus "$val"
        case 'ENABLE_XPS'
            set -g cfg_xps (string lower -- "$val")
        case 'WAIT_RETRIES'
            set -g cfg_wait_retries "$val"
        case 'WAIT_INTERVAL'
            set -g cfg_wait_interval "$val"
    end
end

function load_config
    test -r "$CONFIG_FILE"
    or return 0
    while read -l line
        set line (string trim -- "$line")
        test -z "$line"
        and continue
        string match -qr '^#' -- "$line"
        and continue
        set -l kv (string split -m1 '=' -- "$line")
        test (count $kv) -lt 2
        and continue
        set -l val (string trim -- "$kv[2]")
        set val (string replace -ar '^"|"$' '' -- "$val")
        parse_config_value "$kv[1]" "$val"
    end < "$CONFIG_FILE"
end

function filter_online --argument-names list
    set -l requested (expand_numeric_cpulist "$list")
    set -l online (online_cpus)
    set -l out
    for cpu in $requested
        contains -- "$cpu" $online
        and set out $out "$cpu"
    end
    if test (count $out) -gt 0
        unique_sorted $out
        return 0
    end
    return 1
end

function pick_uint --argument-names cli cfg fallback
    if is_uint "$cli"
        echo "$cli"
    else if is_uint "$cfg"
        echo "$cfg"
    else
        echo "$fallback"
    end
end

function pick_bool --argument-names cli cfg fallback
    for v in "$cli" "$cfg"
        switch (string lower -- "$v")
            case 'yes' 'true' '1' 'on'
                echo "yes"
                return 0
            case 'no' 'false' '0' 'off'
                echo "no"
                return 0
        end
    end
    echo "$fallback"
end

# C8: profiles express intent, then individual flags still win.
function profile_defaults --argument-names profile which
    switch "$profile"
        case 'throughput'
            # many flows / servers: widen net, do not reserve turbo cores
            switch "$which"
                case net; echo "4"
                case gpu; echo "1"
                case io;  echo "2"
                case game; echo "no"
            end
        case 'balanced'
            switch "$which"
                case net; echo "3"
                case gpu; echo "1"
                case io;  echo "2"
                case game; echo "no"
            end
        case '*'
            # gaming (default): narrow net, protect the favored cores
            switch "$which"
                case net; echo "2"
                case gpu; echo "1"
                case io;  echo "2"
                case game; echo "yes"
            end
    end
end

function resolve_effective_settings
    load_config

    if test -n "$opt_iface"
        set -g EFF_IFACE "$opt_iface"
    else
        set -g EFF_IFACE (detect_default_iface)
    end

    set -l prof "gaming"
    if test -n "$opt_profile"
        set prof "$opt_profile"
    else if test -n "$cfg_profile"
        set prof "$cfg_profile"
    end
    set -g EFF_PROFILE (string lower -- "$prof")
    switch "$EFF_PROFILE"
        case 'gaming' 'throughput' 'balanced'
        case '*'
            log_warn "Unknown profile '$EFF_PROFILE', using gaming"
            set -g EFF_PROFILE "gaming"
    end

    set -g EFF_CPUS
    if test -n "$opt_cpus"
        set -g EFF_CPUS (filter_online "$opt_cpus")
        or die "Requested CPUs are not online: $opt_cpus"
    else if test -n "$cfg_cpus"
        set -g EFF_CPUS (filter_online "$cfg_cpus")
        or set -g EFF_CPUS (detect_pcore_primaries)
    else
        set -g EFF_CPUS (detect_pcore_primaries)
    end
    test (count $EFF_CPUS) -eq 0
    and set -g EFF_CPUS (online_cpus)
    test (count $EFF_CPUS) -eq 0
    and die "Could not determine target CPUs"

    # C7: never place IRQs on isolated / nohz_full CPUs.
    set -l iso (isolated_cpus)
    if test (count $iso) -gt 0
        set -l keep (list_subtract "$EFF_CPUS" $iso)
        if test (count $keep) -gt 0
            log_debug "Excluding isolated CPUs: "(collapse_cpulist $iso)
            set -g EFF_CPUS $keep
        else
            log_warn "All candidate CPUs are isolated; ignoring isolation"
        end
    end

    if test (count $opt_drivers) -gt 0
        set -g EFF_DRIVERS $opt_drivers
    else if test (count $cfg_drivers) -gt 0; and contains -- "auto" $cfg_drivers
        set -g EFF_DRIVERS (detect_default_drivers)
    else if test (count $cfg_drivers) -gt 0
        set -g EFF_DRIVERS $cfg_drivers
    else
        set -g EFF_DRIVERS (detect_default_drivers)
    end
    test (count $EFF_DRIVERS) -eq 0
    and die "No supported devices found to pin"

    if test (count $opt_interfaces) -gt 0
        set -g EFF_INTERFACES $opt_interfaces
    else if test (count $cfg_interfaces) -gt 0; and not contains -- "auto" $cfg_interfaces
        set -g EFF_INTERFACES $cfg_interfaces
    else
        set -g EFF_INTERFACES (detect_all_net_ifaces)
    end

    set -g EFF_GPU_CPUS (pick_uint "$opt_gpu_cpus" "$cfg_gpu_cpus" (profile_defaults "$EFF_PROFILE" gpu))
    set -g EFF_IO_CPUS (pick_uint "$opt_io_cpus" "$cfg_io_cpus" (profile_defaults "$EFF_PROFILE" io))
    set -g EFF_NET_CPUS (pick_uint "$opt_net_cpus" "$cfg_net_cpus" (profile_defaults "$EFF_PROFILE" net))
    set -g EFF_RESERVE_CPU0 (pick_bool "$opt_reserve_cpu0" "$cfg_reserve_cpu0" "yes")
    set -g EFF_PCORE_ONLY (pick_bool "$opt_pcore_only" "$cfg_pcore_only" "yes")
    set -g EFF_XPS (pick_bool "$opt_xps" "$cfg_xps" "yes")
    set -g EFF_WAIT_RETRIES (pick_uint "" "$cfg_wait_retries" "12")
    set -g EFF_WAIT_INTERVAL (pick_uint "" "$cfg_wait_interval" "1")

    # C1: which CPUs are held back for the game.
    set -g POOL_GAME
    set -l want_game (pick_bool "" "" (profile_defaults "$EFF_PROFILE" game))
    if test -n "$opt_game_cpus"
        if test "$opt_game_cpus" = "none"
            set -g POOL_GAME
        else
            set -g POOL_GAME (filter_online "$opt_game_cpus")
        end
    else if test -n "$cfg_game_cpus"
        if test "$cfg_game_cpus" = "none"
            set -g POOL_GAME
        else if test "$cfg_game_cpus" = "auto"
            test "$want_game" = "yes"
            and set -g POOL_GAME (detect_turbo_primaries)
        else
            set -g POOL_GAME (filter_online "$cfg_game_cpus")
        end
    else if test "$want_game" = "yes"
        set -g POOL_GAME (detect_turbo_primaries)
    end
    set -g EFF_GAME_CPUS (collapse_cpulist $POOL_GAME)
end

# ------------------------------------------------------------------ pools

function detect_present_driver_groups
    set -g PRESENT_GPU 0
    set -g PRESENT_IO 0
    set -g PRESENT_NET 0
    set -g PRESENT_MISC 0
    for drv in $EFF_DRIVERS
        if is_network_driver "$drv"
            set -g PRESENT_NET 1
        else if is_gpu_driver "$drv"
            set -g PRESENT_GPU 1
        else if is_io_driver "$drv"
            set -g PRESENT_IO 1
        else
            set -g PRESENT_MISC 1
        end
    end
end

function compute_auto_pools
    set -g POOL_ALL (unique_sorted $EFF_CPUS)
    set -g POOL_MISC
    set -g POOL_GPU
    set -g POOL_IO
    set -g POOL_NET

    detect_present_driver_groups

    set -l work $POOL_ALL

    # C1: carve the game's cores out first -- they are the whole point.
    if test (count $POOL_GAME) -gt 0
        set -l keep (list_subtract "$work" $POOL_GAME)
        if test (count $keep) -ge 2
            set work $keep
        else
            log_warn "Not enough CPUs to reserve a game pool; ignoring it"
            set -g POOL_GAME
        end
    end

    if test "$EFF_RESERVE_CPU0" = "yes"
        if contains -- 0 $work; and test (count $work) -ge 4
            set -g POOL_MISC 0
            set work (list_subtract "$work" 0)
        end
    end
    test (count $work) -eq 0
    and set work $POOL_ALL

    set -l work_cnt (count $work)

    if test "$PRESENT_NET" -eq 1
        set -l net_want "$EFF_NET_CPUS"
        test "$net_want" -lt 1
        and set net_want 1
        set -l reserve 0
        test "$PRESENT_GPU" -eq 1
        and set reserve (math "$reserve + 1")
        test "$PRESENT_IO" -eq 1
        and set reserve (math "$reserve + 1")
        set -l net_max (math "$work_cnt - $reserve")
        test "$net_max" -lt 1
        and set net_max 1
        test "$net_want" -gt "$net_max"
        and set net_want "$net_max"
        test "$net_want" -gt "$work_cnt"
        and set net_want "$work_cnt"
        set -g POOL_NET (take_first_n "$net_want" $work)
        set work (drop_first_n "$net_want" $work)
    end

    set work_cnt (count $work)
    if test "$PRESENT_GPU" -eq 1
        set -l gpu_want "$EFF_GPU_CPUS"
        test "$gpu_want" -gt "$work_cnt"
        and set gpu_want "$work_cnt"
        set -g POOL_GPU (take_first_n "$gpu_want" $work)
        set work (drop_first_n "$gpu_want" $work)
    end

    set work_cnt (count $work)
    if test "$PRESENT_IO" -eq 1
        set -l io_want "$EFF_IO_CPUS"
        test "$io_want" -gt "$work_cnt"
        and set io_want "$work_cnt"
        set -g POOL_IO (take_first_n "$io_want" $work)
        set work (drop_first_n "$io_want" $work)
    end

    test (count $work) -gt 0
    and set -g POOL_MISC $POOL_MISC $work
    test (count $POOL_MISC) -eq 0
    and set -g POOL_MISC $POOL_ALL[1]
    if test "$PRESENT_GPU" -eq 1; and test (count $POOL_GPU) -eq 0
        set -g POOL_GPU $POOL_MISC[1]
    end
    if test "$PRESENT_IO" -eq 1; and test (count $POOL_IO) -eq 0
        set -g POOL_IO $POOL_MISC
    end
    if test "$PRESENT_NET" -eq 1; and test (count $POOL_NET) -eq 0
        set -g POOL_NET $POOL_MISC
    end

    set -g POOL_MISC (unique_sorted $POOL_MISC)
    set -g POOL_GPU (unique_sorted $POOL_GPU)
    set -g POOL_IO (unique_sorted $POOL_IO)
    set -g POOL_NET (unique_sorted $POOL_NET)
end

function pool_name_for_driver --argument-names drv
    if is_network_driver "$drv"
        echo "net"
    else if is_gpu_driver "$drv"
        echo "gpu"
    else if is_io_driver "$drv"
        echo "io"
    else
        echo "misc"
    end
end

function pool_name_for_irq --argument-names drv irq
    if is_network_driver "$drv"
        if is_network_queue_irq_name (irq_name_full "$irq")
            echo "net"
        else
            echo "misc"
        end
        return 0
    end
    pool_name_for_driver "$drv"
end

function pool_cpus_by_name --argument-names poolname
    switch "$poolname"
        case 'net'
            printf "%s\n" $POOL_NET
        case 'gpu'
            printf "%s\n" $POOL_GPU
        case 'io'
            printf "%s\n" $POOL_IO
        case '*'
            printf "%s\n" $POOL_MISC
    end
end

function rr_next --argument-names key
    set -l cpus $argv[2..-1]
    set -l n (count $cpus)
    test "$n" -eq 0
    and return 1
    set -l idx 1
    set -l slot 0
    for i in (seq (count $RR_KEYS))
        if test "$RR_KEYS[$i]" = "$key"
            set slot $i
            set idx $RR_VALS[$i]
            break
        end
    end
    if test "$slot" -eq 0
        set -g RR_KEYS $RR_KEYS "$key"
        set -g RR_VALS $RR_VALS 1
        set slot (count $RR_KEYS)
        set idx 1
    end
    test "$idx" -gt "$n"
    and set idx 1
    echo "$cpus[$idx]"
    set -l nxt (math "$idx + 1")
    test "$nxt" -gt "$n"
    and set nxt 1
    set -g RR_VALS[$slot] "$nxt"
    return 0
end

function show_pools
    log_info "Profile  : $EFF_PROFILE"
    log_info "All CPUs : "(collapse_cpulist $POOL_ALL)
    if test (count $POOL_GAME) -gt 0
        log_info "GAME     : "(collapse_cpulist $POOL_GAME)"   (reserved, no IRQs)"
    end
    log_info "Net pool : "(collapse_cpulist $POOL_NET)
    log_info "GPU pool : "(collapse_cpulist $POOL_GPU)
    log_info "I/O pool : "(collapse_cpulist $POOL_IO)
    log_info "Misc pool: "(collapse_cpulist $POOL_MISC)
end

function explain_placement
    echo
    echo "── Reasoning ────────────────────────────────────────────────────"
    set -l allirq $POOL_NET $POOL_GPU $POOL_IO $POOL_MISC
    set -l phys (physical_core_count $allirq)
    echo "  IRQ-bearing logical CPUs : "(collapse_cpulist $allirq)
    echo "  distinct physical cores  : $phys"
    if test (count $POOL_GAME) -gt 0
        echo "  reserved for the game    : "(collapse_cpulist $POOL_GAME)" (turbo/favored)"
        set -l sibs
        for c in $POOL_GAME
            for s in (cpu_siblings "$c")
                contains -- "$s" $sibs
                or set sibs $sibs "$s"
            end
        end
        echo "  ...incl. SMT siblings    : "(collapse_cpulist $sibs)
    end
    set -l clean
    for c in (online_cpus)
        contains -- "$c" $allirq
        or set clean $clean "$c"
    end
    echo "  CPUs with no pinned IRQ  : "(collapse_cpulist $clean)
    echo "─────────────────────────────────────────────────────────────────"
end

function verify_irqs
    echo "Profile    : $EFF_PROFILE"
    echo "Target CPUs: "(collapse_cpulist $POOL_ALL)
    test (count $POOL_GAME) -gt 0
    and echo "Game pool  : "(collapse_cpulist $POOL_GAME)" (reserved)"
    echo "Net pool   : "(collapse_cpulist $POOL_NET)
    echo "GPU pool   : "(collapse_cpulist $POOL_GPU)
    echo "I/O pool   : "(collapse_cpulist $POOL_IO)
    echo "Misc pool  : "(collapse_cpulist $POOL_MISC)
    echo
    printf "%-6s %-11s %-11s %-6s %-14s %-9s %s\n" \
        "IRQ" "requested" "effective" "pool" "driver" "iface" "name"
    set -l leaks 0
    for drv in $EFF_DRIVERS
        for irq in (discover_irqs_for_driver "$drv")
            set -l req "unknown"
            set -l eff "unknown"
            test -r "$PROC/irq/$irq/smp_affinity_list"
            and set req (string trim (cat "$PROC/irq/$irq/smp_affinity_list"))
            test -r "$PROC/irq/$irq/effective_affinity_list"
            and set eff (string trim (cat "$PROC/irq/$irq/effective_affinity_list"))
            set -l ifc (irq_iface "$irq" 2>/dev/null)
            test -n "$ifc"
            or set ifc "-"
            set -l tag ""
            if test (count $POOL_GAME) -gt 0
                for g in $POOL_GAME
                    if test "$req" = "$g"
                        set tag "  <== ON GAME CPU"
                        set leaks (math "$leaks + 1")
                    end
                end
            end
            printf "%-6s %-11s %-11s %-6s %-14s %-9s %s%s\n" \
                "$irq" "$req" "$eff" (pool_name_for_irq "$drv" "$irq") \
                "$drv" "$ifc" (irq_name_full "$irq") "$tag"
        end
    end
    if test "$leaks" -gt 0
        echo
        log_warn "$leaks IRQ(s) are sitting on reserved game CPUs"
        return 1
    end
    return 0
end

function apply_irqs
    if test "$_no_wait" != "1"
        wait_for_irqs
        or log_warn "No IRQs discovered during wait window, continuing"
    end

    set -l all_irqs (discover_all_irqs)
    test (count $all_irqs) -eq 0
    and die "No IRQs discovered for drivers: "(string join ' ' -- $EFF_DRIVERS)

    log_info "Drivers   : "(string join ' ' -- $EFF_DRIVERS)
    test (count $EFF_INTERFACES) -gt 0
    and log_info "Interfaces: "(string join ' ' -- $EFF_INTERFACES)
    show_pools

    set -g RR_KEYS
    set -g RR_VALS
    set -g PLACED_IRQ
    set -g PLACED_CPU
    set -g PLACED_IFACE
    set -g PLACED_KIND
    set -g PLACED_QIDX

    set -l total_changed 0
    set -l total_failed 0
    set -l total_managed 0

    for drv in $EFF_DRIVERS
        set -l irqs (discover_irqs_for_driver "$drv")
        if test (count $irqs) -eq 0
            log_debug "No IRQs found for driver $drv"
            continue
        end
        log_info "Applying driver $drv"
        for irq in $irqs
            set -l name (irq_name_full "$irq")
            # C4: skip kernel-managed vectors instead of failing on them
            if irq_is_managed "$irq"
                log_debug "  IRQ $irq is kernel-managed, skipping ($name)"
                set total_managed (math "$total_managed + 1")
                continue
            end
            set -l poolname (pool_name_for_irq "$drv" "$irq")
            set -l ifc (irq_iface "$irq" 2>/dev/null)
            test -n "$ifc"
            or set ifc "$drv"
            set -l key "$poolname/$ifc"
            set -l cpu (rr_next "$key" (pool_cpus_by_name "$poolname"))
            if test -z "$cpu"
                log_warn "  No CPU available for IRQ $irq ($name)"
                set total_failed (math "$total_failed + 1")
                continue
            end
            if set_irq_cpu "$irq" "$cpu"
                log_info "  IRQ $irq -> CPU $cpu [$poolname] ($name)"
                set total_changed (math "$total_changed + 1")
                set -l qi (irq_queue_info "$name")
                set -g PLACED_IRQ $PLACED_IRQ "$irq"
                set -g PLACED_CPU $PLACED_CPU "$cpu"
                set -g PLACED_IFACE $PLACED_IFACE "$ifc"
                set -g PLACED_KIND $PLACED_KIND (string split ' ' -- "$qi")[1]
                set -g PLACED_QIDX $PLACED_QIDX (string split ' ' -- "$qi")[2]
            else
                log_warn "  Could not pin IRQ $irq ($name)"
                set total_failed (math "$total_failed + 1")
            end
        end
    end

    # C6: align XPS with the TX vector placement, and disable RPS.
    if test "$EFF_XPS" = "yes"
        set -l xps_done 0
        for i in (seq (count $PLACED_IRQ))
            set -l kind $PLACED_KIND[$i]
            switch "$kind"
                case 'tx' 'txrx'
                    if set_xps "$PLACED_IFACE[$i]" "$PLACED_QIDX[$i]" "$PLACED_CPU[$i]"
                        set xps_done (math "$xps_done + 1")
                    end
            end
        end
        for ifc in $EFF_INTERFACES
            clear_rps "$ifc"
        end
        test "$xps_done" -gt 0
        and log_info "XPS aligned on $xps_done TX queue(s); RPS cleared"
    end

    set -l summary "Pinned $total_changed IRQs, failed $total_failed"
    test "$total_managed" -gt 0
    and set summary "$summary, skipped $total_managed kernel-managed"
    log_info "$summary"

    if test "$_quiet" != "1"
        test "$_explain" = "1"
        and explain_placement
        echo
        verify_irqs
    end
    test "$total_failed" -gt 0
    and return 1
    return 0
end

# --------------------------------------------------------------- install

function write_config_file
    mkdir -p "$CONFIG_DIR"
    begin
        echo "# Generated by install-irq.fish v$SCRIPT_VERSION"
        echo "# PIN_DRIVERS / PIN_INTERFACES may be 'auto' to re-detect each run."
        echo "# GAME_CPUS may be 'auto' (turbo/favored cores), 'none', or a list."
        echo "PROFILE=\"$EFF_PROFILE\""
        echo "LATENCY_CPUS=\""(collapse_cpulist $EFF_CPUS)"\""
        echo "PIN_DRIVERS=\"auto\""
        echo "PIN_INTERFACES=\"auto\""
        echo "GAME_CPUS=\"auto\""
        echo "GPU_CPUS=\"$EFF_GPU_CPUS\""
        echo "IO_CPUS=\"$EFF_IO_CPUS\""
        echo "NET_CPUS=\"$EFF_NET_CPUS\""
        echo "RESERVE_CPU0=\"$EFF_RESERVE_CPU0\""
        echo "PCORE_ONLY=\"$EFF_PCORE_ONLY\""
        echo "ENABLE_XPS=\"$EFF_XPS\""
        echo "WAIT_RETRIES=\"$EFF_WAIT_RETRIES\""
        echo "WAIT_INTERVAL=\"$EFF_WAIT_INTERVAL\""
    end > "$CONFIG_FILE"
    chmod 0644 "$CONFIG_FILE"
end

function install_self
    mkdir -p (dirname "$RUNTIME_SCRIPT")
    cp (self_path) "$RUNTIME_SCRIPT"
    chmod 0755 "$RUNTIME_SCRIPT"
end

# C5: ban the whole managed CPU set, not just our modules. --banmod alone
# leaves every other device free to migrate onto the cores we just cleared.
function write_irqbalance_dropin
    mkdir -p "$IRQBALANCE_DROPIN_DIR"
    set -l banned $POOL_NET $POOL_GPU $POOL_IO $POOL_MISC $POOL_GAME
    set -l banlist
    for drv in $EFF_DRIVERS
        set banlist $banlist "--banmod=$drv"
    end
    begin
        echo "[Service]"
        echo "# Keep irqbalance away from the CPUs this tool manages."
        echo "Environment=IRQBALANCE_BANNED_CPULIST="(collapse_cpulist $banned)
        echo "ExecStart="
        echo "ExecStart=/usr/bin/irqbalance --foreground "(string join ' ' -- $banlist)
    end > "$IRQBALANCE_DROPIN"
    chmod 0644 "$IRQBALANCE_DROPIN"
end

function write_systemd_units
    mkdir -p "$SYSTEMD_DIR"
    begin
        echo "[Unit]"
        echo "Description=Pin latency critical IRQs to selected CPU pools"
        echo "Wants=irqbalance.service network-online.target"
        echo "After=systemd-udev-settle.service irqbalance.service network-online.target"
        echo "ConditionPathExists=$RUNTIME_SCRIPT"
        echo
        echo "[Service]"
        echo "Type=oneshot"
        echo "ExecStartPre=/bin/sleep 2"
        echo "ExecStart=/usr/bin/fish $RUNTIME_SCRIPT --apply"
        echo "RemainAfterExit=yes"
        echo "StandardOutput=journal"
        echo "StandardError=journal"
        echo
        echo "[Install]"
        echo "WantedBy=multi-user.target"
    end > "$SYSTEMD_DIR/pin-latency-irqs.service"

    begin
        echo "[Unit]"
        echo "Description=Re-pin latency critical IRQs after hotplug"
        echo "After=pin-latency-irqs.service"
        echo "ConditionPathExists=$RUNTIME_SCRIPT"
        echo
        echo "[Service]"
        echo "Type=oneshot"
        echo "ExecStartPre=/bin/sleep 2"
        echo "ExecStart=/usr/bin/fish $RUNTIME_SCRIPT --apply --quiet --no-wait"
        echo "StandardOutput=journal"
        echo "StandardError=journal"
    end > "$SYSTEMD_DIR/pin-latency-irqs-repin.service"

    begin
        echo "[Unit]"
        echo "Description=Periodic IRQ repin verification"
        echo
        echo "[Timer]"
        echo "OnBootSec=2min"
        echo "OnUnitActiveSec=15min"
        echo "Unit=pin-latency-irqs.service"
        echo "Persistent=true"
        echo
        echo "[Install]"
        echo "WantedBy=timers.target"
    end > "$SYSTEMD_DIR/pin-latency-irqs.timer"

    chmod 0644 "$SYSTEMD_DIR/pin-latency-irqs.service"
    chmod 0644 "$SYSTEMD_DIR/pin-latency-irqs-repin.service"
    chmod 0644 "$SYSTEMD_DIR/pin-latency-irqs.timer"
end

function write_udev_rules
    mkdir -p "$UDEV_RULES_DIR"
    begin
        echo '# Re-pin when a NIC appears, is renamed, or its link changes.'
        echo 'ACTION=="add", SUBSYSTEM=="net", KERNEL=="en*", TAG+="systemd", ENV{SYSTEMD_WANTS}+="pin-latency-irqs-repin.service"'
        echo 'ACTION=="move", SUBSYSTEM=="net", KERNEL=="en*", TAG+="systemd", ENV{SYSTEMD_WANTS}+="pin-latency-irqs-repin.service"'
        echo 'ACTION=="change", SUBSYSTEM=="net", KERNEL=="en*", TAG+="systemd", ENV{SYSTEMD_WANTS}+="pin-latency-irqs-repin.service"'
        for drv in ixgbe igc igb r8169 e1000e i40e ice atlantic amdgpu nvidia i915 xe xhci_hcd snd_hda_intel
            echo "ACTION==\"bind\", SUBSYSTEM==\"pci\", DRIVERS==\"$drv\", TAG+=\"systemd\", ENV{SYSTEMD_WANTS}+=\"pin-latency-irqs-repin.service\""
        end
    end > "$UDEV_RULES_DIR/99-gaming-irq-pin.rules"
    chmod 0644 "$UDEV_RULES_DIR/99-gaming-irq-pin.rules"
end

# ------------------------------------------------------------------- args

function print_help
    echo "install-irq.fish v$SCRIPT_VERSION"
    echo
    echo "Install:"
    echo "  sudo fish ./install-irq.fish                    # gaming profile"
    echo "  sudo fish ./install-irq.fish --profile throughput"
    echo "  sudo fish ./install-irq.fish --dry-run --explain # decide nothing, show all"
    echo
    echo "Runtime:"
    echo "  sudo fish $RUNTIME_SCRIPT --apply"
    echo "  sudo fish $RUNTIME_SCRIPT --verify"
    echo
    echo "Profiles:"
    echo "  gaming      (default) 2 net CPUs, turbo/favored cores reserved"
    echo "  balanced    3 net CPUs, no reservation"
    echo "  throughput  4 net CPUs, no reservation"
    echo
    echo "Options:"
    echo "  --profile P           gaming|balanced|throughput"
    echo "  --game-cpus LIST      CPUs to keep IRQ-free ('auto'|'none'|list)"
    echo "  --iface NAME          preferred interface (default: the one with carrier)"
    echo "  --interfaces a,b      restrict to these NICs ('auto' = all present)"
    echo "  --drivers a,b         restrict to these drivers ('auto' = detect)"
    echo "  --cpus LIST           candidate CPUs (default: P-core primaries)"
    echo "  --net-cpus N --gpu-cpus N --io-cpus N"
    echo "  --reserve-cpu0 yes|no keep CPU0 for misc IRQs (default yes)"
    echo "  --pcore-only yes|no   never use E-cores (default yes)"
    echo "  --xps yes|no          align XPS with TX IRQ placement (default yes)"
    echo "  --explain             print the reasoning behind the layout"
    echo "  --dry-run --quiet --debug --no-wait"
end

function parse_args
    set -g opt_action (default_action)
    set -l a $argv
    while test (count $a) -gt 0
        switch $a[1]
            case '--install'
                set -g opt_action 'install'; set a $a[2..-1]
            case '--apply'
                set -g opt_action 'apply'; set a $a[2..-1]
            case '--verify' '--status'
                set -g opt_action 'verify'; set a $a[2..-1]
            case '--iface'
                test (count $a) -ge 2; or die "Missing value for --iface"
                set -g opt_iface "$a[2]"; set a $a[3..-1]
            case '--cpus'
                test (count $a) -ge 2; or die "Missing value for --cpus"
                set -g opt_cpus "$a[2]"; set a $a[3..-1]
            case '--game-cpus'
                test (count $a) -ge 2; or die "Missing value for --game-cpus"
                set -g opt_game_cpus "$a[2]"; set a $a[3..-1]
            case '--profile'
                test (count $a) -ge 2; or die "Missing value for --profile"
                set -g opt_profile "$a[2]"; set a $a[3..-1]
            case '--drivers'
                test (count $a) -ge 2; or die "Missing value for --drivers"
                set -g opt_drivers (string split ' ' -- (string replace -a ',' ' ' -- "$a[2]"))
                set a $a[3..-1]
            case '--interfaces'
                test (count $a) -ge 2; or die "Missing value for --interfaces"
                set -g opt_interfaces (string split ' ' -- (string replace -a ',' ' ' -- "$a[2]"))
                set a $a[3..-1]
            case '--gpu-cpus'
                test (count $a) -ge 2; or die "Missing value for --gpu-cpus"
                set -g opt_gpu_cpus "$a[2]"; set a $a[3..-1]
            case '--io-cpus'
                test (count $a) -ge 2; or die "Missing value for --io-cpus"
                set -g opt_io_cpus "$a[2]"; set a $a[3..-1]
            case '--net-cpus'
                test (count $a) -ge 2; or die "Missing value for --net-cpus"
                set -g opt_net_cpus "$a[2]"; set a $a[3..-1]
            case '--reserve-cpu0'
                test (count $a) -ge 2; or die "Missing value for --reserve-cpu0"
                set -g opt_reserve_cpu0 (string lower -- "$a[2]"); set a $a[3..-1]
            case '--pcore-only'
                test (count $a) -ge 2; or die "Missing value for --pcore-only"
                set -g opt_pcore_only (string lower -- "$a[2]"); set a $a[3..-1]
            case '--xps'
                test (count $a) -ge 2; or die "Missing value for --xps"
                set -g opt_xps (string lower -- "$a[2]"); set a $a[3..-1]
            case '--explain'
                set -g _explain 1; set a $a[2..-1]
            case '--dry-run'
                set -g _dry_run 1; set a $a[2..-1]
            case '--quiet'
                set -g _quiet 1; set a $a[2..-1]
            case '--debug'
                set -g _debug 1; set a $a[2..-1]
            case '--no-wait'
                set -g _no_wait 1; set a $a[2..-1]
            case '--help' '-h'
                print_help
                exit 0
            case '*'
                die "Unknown option: $a[1]"
        end
    end
end

function install_mode
    require_root
    need_cmd fish
    need_cmd systemctl
    need_cmd udevadm
    need_cmd find
    need_cmd grep
    need_cmd sort

    resolve_effective_settings
    compute_auto_pools

    log_info "Installing IRQ latency pinning v$SCRIPT_VERSION"
    log_info "Interface : "(test -n "$EFF_IFACE"; and echo "$EFF_IFACE"; or echo "none")
    log_info "Drivers   : "(string join ' ' -- $EFF_DRIVERS)
    log_info "NICs      : "(string join ' ' -- $EFF_INTERFACES)
    log_info "CPUs      : "(collapse_cpulist $EFF_CPUS)
    show_pools
    test "$_explain" = "1"
    and explain_placement

    if test "$_dry_run" = "1"
        log_info "--dry-run: no files written, nothing applied"
        return 0
    end

    write_config_file
    install_self
    fish -n "$RUNTIME_SCRIPT"
    or die "Installed runtime script has syntax errors"
    write_irqbalance_dropin
    write_systemd_units
    write_udev_rules

    systemctl daemon-reload
    udevadm control --reload-rules
    systemctl enable irqbalance.service >/dev/null 2>&1
    systemctl restart irqbalance.service
    or log_warn "irqbalance restart failed"
    systemctl enable --now pin-latency-irqs.service
    or die "Failed to enable or start pin-latency-irqs.service"
    systemctl enable --now pin-latency-irqs.timer >/dev/null 2>&1
    or log_warn "Could not enable pin-latency-irqs.timer"

    echo
    log_info "Installed files:"
    log_info "  $RUNTIME_SCRIPT"
    log_info "  $CONFIG_FILE"
    log_info "  $SYSTEMD_DIR/pin-latency-irqs.service"
    log_info "  $SYSTEMD_DIR/pin-latency-irqs-repin.service"
    log_info "  $SYSTEMD_DIR/pin-latency-irqs.timer"
    log_info "  $IRQBALANCE_DROPIN"
    log_info "  $UDEV_RULES_DIR/99-gaming-irq-pin.rules"
    echo
    log_info "Verify with:  fish $RUNTIME_SCRIPT --verify"
end

function runtime_mode
    require_root
    resolve_effective_settings
    compute_auto_pools
    switch "$opt_action"
        case 'apply'
            apply_irqs
        case 'verify'
            verify_irqs
        case '*'
            die "Invalid runtime action: $opt_action"
    end
end

parse_args $argv

switch "$opt_action"
    case 'install'
        install_mode
    case 'apply' 'verify'
        runtime_mode
    case '*'
        die "Unsupported action: $opt_action"
end
