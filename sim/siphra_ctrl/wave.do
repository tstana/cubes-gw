onerror {resume}
quietly WaveActivateNextPane {} 0
add wave -noupdate /testbench/clk_100meg
add wave -noupdate /testbench/rst_n
add wave -noupdate -divider DUT
add wave -noupdate /testbench/DUT/reg_op_start_p_i
add wave -noupdate /testbench/DUT/reg_op_i
add wave -noupdate -radix binary /testbench/DUT/reg_addr_i
add wave -noupdate -radix binary /testbench/DUT/reg_data_i
add wave -noupdate /testbench/DUT/reg_op_ready_o
add wave -noupdate /testbench/DUT/state
add wave -noupdate -radix unsigned /testbench/DUT/bits_to_send
add wave -noupdate -radix unsigned /testbench/DUT/shift_count
add wave -noupdate /testbench/DUT/data_sreg
add wave -noupdate /testbench/DUT/addr_sreg
add wave -noupdate /testbench/DUT/spi_data_in
add wave -noupdate /testbench/DUT/spi_cs_n_o
add wave -noupdate /testbench/DUT/spi_sclk_o
add wave -noupdate /testbench/DUT/spi_mosi_o
add wave -noupdate /testbench/DUT/spi_miso_i
add wave -noupdate /testbench/DUT/spi_cs
TreeUpdate [SetDefaultTree]
WaveRestoreCursors {{Cursor 1} {43805000 ps} 0}
quietly wave cursor active 1
configure wave -namecolwidth 225
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
WaveRestoreZoom {16838750 ps} {49651250 ps}
