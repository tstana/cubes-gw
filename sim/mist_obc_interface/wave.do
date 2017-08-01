onerror {resume}
quietly WaveActivateNextPane {} 0
add wave -noupdate -divider {Clock, reset}
add wave -noupdate /testbench/clk_50meg
add wave -noupdate /testbench/rst_n
add wave -noupdate -divider Master
add wave -noupdate /testbench/trans_state
add wave -noupdate /testbench/frame_state
add wave -noupdate /testbench/frame_end_p
add wave -noupdate /testbench/master_tx_ready
add wave -noupdate -radix hexadecimal /testbench/master_tx_data
add wave -noupdate -radix hexadecimal /testbench/header_buf
add wave -noupdate /testbench/master_tx_start_p
add wave -noupdate -radix unsigned /testbench/frame_byte_count
add wave -noupdate -radix unsigned /testbench/frame_data_bytes
add wave -noupdate -radix unsigned /testbench/trans_data_bytes
add wave -noupdate /testbench/master_rx_ready
add wave -noupdate -radix hexadecimal /testbench/master_rx_data
add wave -noupdate -radix hexadecimal /testbench/opcode
add wave -noupdate /testbench/tid
add wave -noupdate /testbench/fid
add wave -noupdate /testbench/rx_fid
add wave -noupdate -radix unsigned /testbench/dl
add wave -noupdate -divider ERROR
add wave -noupdate /testbench/ERROR
add wave -noupdate -divider DUT
add wave -noupdate /testbench/cmp_dut/clk_50meg_i
add wave -noupdate /testbench/cmp_dut/rst
add wave -noupdate /testbench/cmp_dut/rst_n
add wave -noupdate /testbench/cmp_dut/clk_100meg
add wave -noupdate -radix hexadecimal /testbench/led
add wave -noupdate -divider DUT/mist_obc_interface
add wave -noupdate /testbench/cmp_dut/cmp_obc_interface/frame_state
add wave -noupdate /testbench/cmp_dut/cmp_obc_interface/trans_state
add wave -noupdate -radix unsigned /testbench/cmp_dut/cmp_obc_interface/frame_byte_count
add wave -noupdate -radix unsigned /testbench/cmp_dut/cmp_obc_interface/frame_data_bytes
add wave -noupdate -radix unsigned /testbench/cmp_dut/cmp_obc_interface/trans_data_bytes
TreeUpdate [SetDefaultTree]
WaveRestoreCursors {{Cursor 1} {320000 ps} 0}
quietly wave cursor active 1
configure wave -namecolwidth 413
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
configure wave -timelineunits us
update
WaveRestoreZoom {0 ps} {15750016 ps}
