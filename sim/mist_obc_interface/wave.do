onerror {resume}
quietly WaveActivateNextPane {} 0
add wave -noupdate /testbench/clk_100meg
add wave -noupdate /testbench/rst_n
add wave -noupdate /testbench/trans_active
add wave -noupdate /testbench/master_state
add wave -noupdate /testbench/trans_state
add wave -noupdate /testbench/master_tx_ready
add wave -noupdate /testbench/master_rx_ready
add wave -noupdate -radix hexadecimal /testbench/header_buf
add wave -noupdate /testbench/master_tx_start_p
add wave -noupdate -radix unsigned /testbench/frame_byte_count
add wave -noupdate -divider ERROR
add wave -noupdate /testbench/ERROR
add wave -noupdate -divider DUT
add wave -noupdate /testbench/U_DUT/state
add wave -noupdate /testbench/U_DUT/state_d0
add wave -noupdate /testbench/U_DUT/trans_state
add wave -noupdate /testbench/U_DUT/tx_start_p
add wave -noupdate -radix hexadecimal /testbench/U_DUT/i2c_tx_byte
add wave -noupdate -radix hexadecimal /testbench/U_DUT/i2c_rx_byte
add wave -noupdate /testbench/U_DUT/i2c_addr_match_p
add wave -noupdate /testbench/U_DUT/i2c_op
add wave -noupdate /testbench/U_DUT/i2c_r_done_p
add wave -noupdate /testbench/U_DUT/i2c_w_done_p
add wave -noupdate /testbench/U_DUT/fid
add wave -noupdate /testbench/U_DUT/fid_prev
add wave -noupdate /testbench/U_DUT/tid
add wave -noupdate -radix hexadecimal /testbench/U_DUT/opcode
add wave -noupdate -radix hexadecimal /testbench/U_DUT/current_op
add wave -noupdate -radix unsigned /testbench/U_DUT/data_len
add wave -noupdate -radix unsigned /testbench/U_DUT/data_byte_count
add wave -noupdate -radix unsigned /testbench/U_DUT/frame_byte_count
add wave -noupdate -radix unsigned /testbench/U_DUT/nr_data_bytes
TreeUpdate [SetDefaultTree]
WaveRestoreCursors {{Cursor 1} {203403141 ps} 0}
quietly wave cursor active 1
configure wave -namecolwidth 244
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
WaveRestoreZoom {0 ps} {2100 us}
