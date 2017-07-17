onerror {resume}
quietly WaveActivateNextPane {} 0
add wave -noupdate /testbench/clk_100meg
add wave -noupdate /testbench/rst_n
add wave -noupdate /testbench/master_state
add wave -noupdate /testbench/transaction_state
add wave -noupdate /testbench/transaction_ongoing
add wave -noupdate /testbench/master_tx_ready
add wave -noupdate /testbench/master_rx_ready
add wave -noupdate -radix hexadecimal /testbench/header_buf
add wave -noupdate /testbench/master_tx_start_p
add wave -noupdate -radix unsigned /testbench/frame_byte_count
TreeUpdate [SetDefaultTree]
WaveRestoreCursors {{Cursor 1} {608983852 ps} 0}
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
WaveRestoreZoom {604898437 ps} {613101563 ps}
