vlib -type directory work

vcom -93 -work work {../../hdl/ip_cores/general-cores/modules/genrams/genram_pkg.vhd}
vcom -93 -work work {../../hdl/ip_cores/general-cores/modules/genrams/altera/generic_dpram.vhd}
vcom -93 -work work {../../hdl/ip_cores/general-cores/modules/common/gencores_pkg.vhd}
vcom -93 -work work {../../hdl/ip_cores/general-cores/modules/common/gc_sync_ffs.vhd}
vcom -93 -work work {../../hdl/ip_cores/general-cores/modules/wishbone/wishbone_pkg.vhd}
vcom -93 -work work {../../hdl/ip_cores/altera_main_pll/altera_main_pll.vhd}
vcom -93 -work work {../../hdl/ip_cores/altera_cascade_pll/altera_cascade_pll.vhd}
vcom -93 -work work {../../hdl/bemicro/debouncer.vhd}
vcom -93 -work work {../../hdl/bemicro/plls.vhd}
vcom -93 -work work {../../hdl/bemicro/uart.vhd}
vcom -93 -work work {../../hdl/bemicro/mist_uart_wrapper.vhd}
vcom -93 -work work {../../hdl/modules/msp_pkg.vhd}
vcom -93 -work work {../../hdl/modules/mist_obc_interface.vhd}
vcom -93 -work work {../../hdl/modules/hk_regs.vhd}
vcom -93 -work work {../../hdl/modules/siphra_ctrl/siphra_ctrl.vhd}
vcom -93 -work work {../../hdl/modules/siphra_ctrl/siphra_obc_periph.vhd}
vcom -93 -work work {../../hdl/top/bemicro_cubes_btm.vhd}
vcom -2008 -work work {testbench.vhd}

vsim -t 1ps -voptargs="+acc" -lib work work.testbench

do wave.do

run 120 us
wave zoomfull
