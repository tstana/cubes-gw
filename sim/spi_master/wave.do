onerror {resume}
quietly WaveActivateNextPane {} 0
add wave -noupdate /testbench/clk_100meg
add wave -noupdate /testbench/rst_n
add wave -noupdate /testbench/chip_sel
add wave -noupdate /testbench/spi_cpol
add wave -noupdate -radix unsigned /testbench/bits_to_send
add wave -noupdate -radix hexadecimal -childformat {{/testbench/spi_data_in(31) -radix hexadecimal} {/testbench/spi_data_in(30) -radix hexadecimal} {/testbench/spi_data_in(29) -radix hexadecimal} {/testbench/spi_data_in(28) -radix hexadecimal} {/testbench/spi_data_in(27) -radix hexadecimal} {/testbench/spi_data_in(26) -radix hexadecimal} {/testbench/spi_data_in(25) -radix hexadecimal} {/testbench/spi_data_in(24) -radix hexadecimal} {/testbench/spi_data_in(23) -radix hexadecimal} {/testbench/spi_data_in(22) -radix hexadecimal} {/testbench/spi_data_in(21) -radix hexadecimal} {/testbench/spi_data_in(20) -radix hexadecimal} {/testbench/spi_data_in(19) -radix hexadecimal} {/testbench/spi_data_in(18) -radix hexadecimal} {/testbench/spi_data_in(17) -radix hexadecimal} {/testbench/spi_data_in(16) -radix hexadecimal} {/testbench/spi_data_in(15) -radix hexadecimal} {/testbench/spi_data_in(14) -radix hexadecimal} {/testbench/spi_data_in(13) -radix hexadecimal} {/testbench/spi_data_in(12) -radix hexadecimal} {/testbench/spi_data_in(11) -radix hexadecimal} {/testbench/spi_data_in(10) -radix hexadecimal} {/testbench/spi_data_in(9) -radix hexadecimal} {/testbench/spi_data_in(8) -radix hexadecimal} {/testbench/spi_data_in(7) -radix hexadecimal} {/testbench/spi_data_in(6) -radix hexadecimal} {/testbench/spi_data_in(5) -radix hexadecimal} {/testbench/spi_data_in(4) -radix hexadecimal} {/testbench/spi_data_in(3) -radix hexadecimal} {/testbench/spi_data_in(2) -radix hexadecimal} {/testbench/spi_data_in(1) -radix hexadecimal} {/testbench/spi_data_in(0) -radix hexadecimal}} -subitemconfig {/testbench/spi_data_in(31) {-height 15 -radix hexadecimal} /testbench/spi_data_in(30) {-height 15 -radix hexadecimal} /testbench/spi_data_in(29) {-height 15 -radix hexadecimal} /testbench/spi_data_in(28) {-height 15 -radix hexadecimal} /testbench/spi_data_in(27) {-height 15 -radix hexadecimal} /testbench/spi_data_in(26) {-height 15 -radix hexadecimal} /testbench/spi_data_in(25) {-height 15 -radix hexadecimal} /testbench/spi_data_in(24) {-height 15 -radix hexadecimal} /testbench/spi_data_in(23) {-height 15 -radix hexadecimal} /testbench/spi_data_in(22) {-height 15 -radix hexadecimal} /testbench/spi_data_in(21) {-height 15 -radix hexadecimal} /testbench/spi_data_in(20) {-height 15 -radix hexadecimal} /testbench/spi_data_in(19) {-height 15 -radix hexadecimal} /testbench/spi_data_in(18) {-height 15 -radix hexadecimal} /testbench/spi_data_in(17) {-height 15 -radix hexadecimal} /testbench/spi_data_in(16) {-height 15 -radix hexadecimal} /testbench/spi_data_in(15) {-height 15 -radix hexadecimal} /testbench/spi_data_in(14) {-height 15 -radix hexadecimal} /testbench/spi_data_in(13) {-height 15 -radix hexadecimal} /testbench/spi_data_in(12) {-height 15 -radix hexadecimal} /testbench/spi_data_in(11) {-height 15 -radix hexadecimal} /testbench/spi_data_in(10) {-height 15 -radix hexadecimal} /testbench/spi_data_in(9) {-height 15 -radix hexadecimal} /testbench/spi_data_in(8) {-height 15 -radix hexadecimal} /testbench/spi_data_in(7) {-height 15 -radix hexadecimal} /testbench/spi_data_in(6) {-height 15 -radix hexadecimal} /testbench/spi_data_in(5) {-height 15 -radix hexadecimal} /testbench/spi_data_in(4) {-height 15 -radix hexadecimal} /testbench/spi_data_in(3) {-height 15 -radix hexadecimal} /testbench/spi_data_in(2) {-height 15 -radix hexadecimal} /testbench/spi_data_in(1) {-height 15 -radix hexadecimal} /testbench/spi_data_in(0) {-height 15 -radix hexadecimal}} /testbench/spi_data_in
add wave -noupdate -radix hexadecimal /testbench/spi_data_out
add wave -noupdate /testbench/spi_ready
add wave -noupdate /testbench/spi_cs_n_o
add wave -noupdate /testbench/spi_mosi_o
add wave -noupdate /testbench/spi_miso_i
add wave -noupdate /testbench/spi_sclk_o
add wave -noupdate -divider DUT
add wave -noupdate /testbench/DUT/state
add wave -noupdate -radix unsigned /testbench/DUT/divider
add wave -noupdate -radix unsigned /testbench/DUT/counter
add wave -noupdate -radix hexadecimal /testbench/DUT/sreg
add wave -noupdate -radix hexadecimal /testbench/DUT/rx_sreg
TreeUpdate [SetDefaultTree]
WaveRestoreCursors {{Cursor 1} {35309451 ps} 0} {{Cursor 2} {2320884 ps} 0}
quietly wave cursor active 1
configure wave -namecolwidth 180
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
WaveRestoreZoom {15750 ns} {42 us}
