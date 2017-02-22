onerror {resume}
quietly WaveActivateNextPane {} 0
add wave -noupdate /tb_uart/clk_100meg
add wave -noupdate /tb_uart/rst_n
add wave -noupdate -divider tb_baud
add wave -noupdate /tb_uart/baud_x8_tick
add wave -noupdate /tb_uart/baud_div
add wave -noupdate /tb_uart/baud_tick
add wave -noupdate /tb_uart/mon_sample
add wave -noupdate -divider tb_tx
add wave -noupdate /tb_uart/tx_ready
add wave -noupdate /tb_uart/tx_start
add wave -noupdate -radix hexadecimal /tb_uart/tx_data
add wave -noupdate -radix unsigned /tb_uart/start_delay
add wave -noupdate -divider tb_rx
add wave -noupdate -radix hexadecimal /tb_uart/rx_sreg
add wave -noupdate /tb_uart/rx_count
add wave -noupdate -radix hexadecimal /tb_uart/rx_data
add wave -noupdate -divider uart
add wave -noupdate -radix unsigned /tb_uart/DUT/baud_div_i
add wave -noupdate /tb_uart/DUT/baud_tick
add wave -noupdate /tb_uart/DUT/baud_halfbit_tick
add wave -noupdate -divider uart_tx
add wave -noupdate /tb_uart/DUT/txd_o
add wave -noupdate -radix hexadecimal -childformat {{/tb_uart/DUT/tx_data_i(7) -radix hexadecimal} {/tb_uart/DUT/tx_data_i(6) -radix hexadecimal} {/tb_uart/DUT/tx_data_i(5) -radix hexadecimal} {/tb_uart/DUT/tx_data_i(4) -radix hexadecimal} {/tb_uart/DUT/tx_data_i(3) -radix hexadecimal} {/tb_uart/DUT/tx_data_i(2) -radix hexadecimal} {/tb_uart/DUT/tx_data_i(1) -radix hexadecimal} {/tb_uart/DUT/tx_data_i(0) -radix hexadecimal}} -subitemconfig {/tb_uart/DUT/tx_data_i(7) {-height 15 -radix hexadecimal} /tb_uart/DUT/tx_data_i(6) {-height 15 -radix hexadecimal} /tb_uart/DUT/tx_data_i(5) {-height 15 -radix hexadecimal} /tb_uart/DUT/tx_data_i(4) {-height 15 -radix hexadecimal} /tb_uart/DUT/tx_data_i(3) {-height 15 -radix hexadecimal} /tb_uart/DUT/tx_data_i(2) {-height 15 -radix hexadecimal} /tb_uart/DUT/tx_data_i(1) {-height 15 -radix hexadecimal} /tb_uart/DUT/tx_data_i(0) {-height 15 -radix hexadecimal}} /tb_uart/DUT/tx_data_i
add wave -noupdate -radix hexadecimal /tb_uart/DUT/tx_sreg
add wave -noupdate -radix unsigned /tb_uart/DUT/tx_data_count
add wave -noupdate /tb_uart/DUT/tx_start_p_i
add wave -noupdate /tb_uart/DUT/tx_ready_o
add wave -noupdate /tb_uart/DUT/state_tx
add wave -noupdate -divider uart_rx
add wave -noupdate /tb_uart/DUT/rxd_i
add wave -noupdate /tb_uart/DUT/state_rx
add wave -noupdate -radix hexadecimal /tb_uart/DUT/rx_sreg
add wave -noupdate -radix unsigned /tb_uart/DUT/rx_data_count
add wave -noupdate -radix hexadecimal /tb_uart/DUT/rx_data_o
add wave -noupdate /tb_uart/DUT/rx_ready_o
add wave -noupdate /tb_uart/DUT/frame_err_o
TreeUpdate [SetDefaultTree]
WaveRestoreCursors {{Cursor 1} {88815789 ps} 0}
quietly wave cursor active 1
configure wave -namecolwidth 202
configure wave -valuecolwidth 100
configure wave -justifyvalue left
configure wave -signalnamewidth 0
configure wave -snapdistance 10
configure wave -datasetprefix 0
configure wave -rowmargin 4
configure wave -childrowmargin 2
configure wave -gridoffset 0
configure wave -gridperiod 1
configure wave -griddelta 40
configure wave -timeline 0
configure wave -timelineunits ns
update
WaveRestoreZoom {0 ps} {525 us}
