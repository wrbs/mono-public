# vivado -mode batch -source nexys_a7_100t.tcl

set PROJECT_NAME "nexys_a7_100t"
set FPGA_PART "xc7a100tcsg324-1"
set DEBUG false

set_part $FPGA_PART
read_verilog ${PROJECT_NAME}.v
read_xdc ${PROJECT_NAME}.xdc
read_ip clk_wiz.xci
synth_design -top ${PROJECT_NAME}_top -part $FPGA_PART
opt_design
if {$DEBUG} { write_checkpoint -force ${PROJECT_NAME}.synth.dcp }
place_design
if {$DEBUG} { write_checkpoint -force ${PROJECT_NAME}.place.dcp }
route_design
if {$DEBUG} { write_checkpoint -force ${PROJECT_NAME}.route.dcp }
if {$DEBUG} { report_utilization -hierarchical -file ${PROJECT_NAME}.utilization.rpt }
report_timing_summary -file ${PROJECT_NAME}.timing.rpt
write_bitstream -force ${PROJECT_NAME}.bit
set WNS [get_property SLACK [get_timing_paths -max_paths 1 -nworst 1 -setup]]
puts "WNS=$WNS"
