vlib -type directory work

vcom -93 -work work {../../ip_cores/general-cores/modules/genrams/genram_pkg.vhd}
vcom -93 -work work {../../ip_cores/general-cores/modules/common/gencores_pkg.vhd}
vcom -93 -work work {../../ip_cores/general-cores/modules/common/gc_sync_ffs.vhd}
vcom -93 -work work {../../hdl/bemicro/uart.vhd}
vcom -93 -work work {tb_uart.vhd}

vsim -t 1ps -voptargs="+acc" -lib work work.tb_uart

do wave.do

run 500 us
wave zoomfull
