# vivado -mode batch -source flash.tcl

set PROJECT_NAME "nexys_a7_100t"

open_hw
connect_hw_server
open_hw_target
current_hw_device [lindex [get_hw_devices] 0]
set_property PROGRAM.FILE ${PROJECT_NAME}.bit [current_hw_device]
program_hw_devices [current_hw_device]
exit
    