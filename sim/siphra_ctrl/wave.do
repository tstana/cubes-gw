onerror {resume}
quietly WaveActivateNextPane {} 0
add wave -noupdate /testbench/clk_100meg
add wave -noupdate /testbench/rst_n
add wave -noupdate /testbench/siphra_sysclk
add wave -noupdate /testbench/siphra_txd
add wave -noupdate -radix hexadecimal /testbench/siphra_adc_delay
add wave -noupdate -radix hexadecimal /testbench/siphra_adc_value
add wave -noupdate -radix hexadecimal /testbench/siphra_adc_chan
add wave -noupdate /testbench/siphra_adc_trig_type
add wave -noupdate /testbench/siphra_adc_tf
add wave -noupdate /testbench/siphra_adc_shifting
add wave -noupdate /testbench/siphra_adc_sreg
add wave -noupdate -radix hexadecimal /testbench/siphra_adc_sreg_count
add wave -noupdate -divider {Expected output}
add wave -noupdate -radix hexadecimal /testbench/adc_value
add wave -noupdate -radix hexadecimal /testbench/adc_chan
add wave -noupdate /testbench/adc_trig_type
add wave -noupdate /testbench/adc_valid
add wave -noupdate -divider DUT
add wave -noupdate /testbench/DUT/txd_i
add wave -noupdate -radix hexadecimal /testbench/DUT/adc_value_o
add wave -noupdate -radix hexadecimal /testbench/DUT/adc_chan_o
add wave -noupdate /testbench/DUT/adc_trig_type_o
add wave -noupdate /testbench/DUT/adc_valid_o
TreeUpdate [SetDefaultTree]
WaveRestoreCursors {{Cursor 1} {20742610 ps} 0}
quietly wave cursor active 1
configure wave -namecolwidth 269
configure wave -valuecolwidth 216
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
WaveRestoreZoom {0 ps} {210 us}
