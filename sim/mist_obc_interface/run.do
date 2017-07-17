vlib -type directory work

vcom -93 -work work {../../hdl/ip_cores/general-cores/modules/genrams/genram_pkg.vhd}
vcom -93 -work work {../../hdl/ip_cores/general-cores/modules/common/gencores_pkg.vhd}
vcom -93 -work work {../../hdl/ip_cores/general-cores/modules/common/gc_sync_ffs.vhd}
vcom -93 -work work {../../hdl/ip_cores/general-cores/modules/wishbone/wishbone_pkg.vhd}
vcom -93 -work work {../../hdl/bemicro/uart.vhd}
vcom -93 -work work {../../hdl/modules/mist_obc_interface.vhd}
vcom -93 -work work {testbench.vhd}

vsim -t 1ps -voptargs="+acc" -lib work work.testbench

do wave.do

run 2 ms
wave zoomfull
