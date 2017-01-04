module blink (
    input clk_i,
    output [1:0] stat_led_o,
    output [1:0] phy_led_o
);
    
    reg [26:0] div;
    reg [1:0] count;
    
    always @(posedge clk_i)
    begin
        div <= div + 1;
        if ( div == 129_999_999 )
		begin
			div <= 0;
			count <= count + 1;
        end
    end
    
	assign stat_led_o = count;
	assign phy_led_o  = count;
	
endmodule
