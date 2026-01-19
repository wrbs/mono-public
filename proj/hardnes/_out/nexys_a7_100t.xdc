set_property -dict { PACKAGE_PIN C4 IOSTANDARD LVCMOS33 } [ get_ports { usb_uart_rxd } ];
set_property -dict { PACKAGE_PIN D3 IOSTANDARD LVCMOS33 } [ get_ports { usb_uart_rts } ];
set_property -dict { PACKAGE_PIN E3 IOSTANDARD LVCMOS33 } [ get_ports { clock_100 } ];
set_property -dict { PACKAGE_PIN C12 IOSTANDARD LVCMOS33 } [ get_ports { reset_n } ];
set_property -dict { PACKAGE_PIN D4 IOSTANDARD LVCMOS33 } [ get_ports { usb_uart_txd } ];
set_property -dict { PACKAGE_PIN E5 IOSTANDARD LVCMOS33 } [ get_ports { usb_uart_cts } ];
set_property BITSTREAM.GENERAL.COMPRESS true [current_design];
set_property BITSTREAM.CONFIG.SPI_BUSWIDTH 4 [current_design];
set_property CFGBVS VCCO [current_design]
set_property CONFIG_VOLTAGE 3.3 [current_design]
create_clock -add -name clock_100 -period 10.00 [get_ports {clock_100}];