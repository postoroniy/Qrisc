if {[llength $argv] < 4} {
    puts stderr "usage: vivado_fmax.tcl <impl> <netlist.v> <clock_port> <out_dir> ?part? ?period_ns?"
    exit 2
}

set impl [lindex $argv 0]
set netlist [file normalize [lindex $argv 1]]
set clock_port [lindex $argv 2]
set out_dir [file normalize [lindex $argv 3]]
set part "xc7z020clg400-1"
set period_ns 10.000

if {[llength $argv] >= 5} {
    set part [lindex $argv 4]
}
if {[llength $argv] >= 6} {
    set period_ns [lindex $argv 5]
}

file mkdir $out_dir
set_param general.maxThreads 4

read_verilog $netlist
link_design -part $part -top qrisc32 -mode out_of_context
create_clock -name qrisc32_clk -period $period_ns [get_ports $clock_port]

opt_design
place_design
route_design

set timing_summary [file join $out_dir timing_summary.rpt]
set utilization [file join $out_dir utilization.rpt]
report_timing_summary -delay_type max -max_paths 10 -file $timing_summary
report_utilization -file $utilization

set wns [get_property SLACK [get_timing_paths -delay_type max -max_paths 1]]
if {$wns eq ""} {
    puts stderr "No max-delay timing path found for $impl"
    exit 1
}

set min_period [expr {$period_ns - $wns}]
if {$min_period <= 0.0} {
    puts stderr "Invalid min period $min_period for $impl"
    exit 1
}
set fmax_mhz [expr {1000.0 / $min_period}]

set fp [open [file join $out_dir fmax.rpt] w]
puts $fp "implementation,$impl"
puts $fp "part,$part"
puts $fp "clock_port,$clock_port"
puts $fp [format "constraint_period_ns,%.3f" $period_ns]
puts $fp [format "wns_ns,%.3f" $wns]
puts $fp [format "estimated_min_period_ns,%.3f" $min_period]
puts $fp [format "estimated_fmax_mhz,%.1f" $fmax_mhz]
close $fp

puts [format "FMAX,%s,period_ns=%.3f,wns_ns=%.3f,min_period_ns=%.3f,fmax_mhz=%.1f" \
    $impl $period_ns $wns $min_period $fmax_mhz]
