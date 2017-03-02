vlib -type directory work

vcom -93 -work work {../../hdl/ip_cores/general-cores/modules/genrams/genram_pkg.vhd}
vcom -93 -work work {../../hdl/ip_cores/general-cores/modules/common/gencores_pkg.vhd}
vcom -93 -work work {../../hdl/modules/siphra_ctrl/spi_master.vhd}
vcom -93 -work work {testbench.vhd}

vsim -t 1ps -voptargs="+acc" -lib work work.testbench

do wave.do

run 500 us
wave zoomfull
