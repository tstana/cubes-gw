// (C) 2001-2016 Altera Corporation. All rights reserved.
// Your use of Altera Corporation's design tools, logic functions and other 
// software and tools, and its AMPP partner logic functions, and any output 
// files any of the foregoing (including device programming or simulation 
// files), and any associated documentation or information are expressly subject 
// to the terms and conditions of the Altera Program License Subscription 
// Agreement, Altera MegaCore Function License Agreement, or other applicable 
// license agreement, including, without limitation, that your use is for the 
// sole purpose of programming logic devices manufactured by Altera and sold by 
// Altera or its authorized distributors.  Please refer to the applicable 
// agreement for further details.


// *******************************************************************************************************
// **   Filename       : fiftyfivenm_adcblock_top_wrapper.v
// **
// **   Description    : Represent the ADC BLOCK primitive with channel code mapped for software
// **                    in incremental order.  The hardware chsel code is not in sequence due to routability.
// **   Change Log     :
// **   08/31/2013     : Initial design
// **
// *******************************************************************************************************

// synthesis translate_off
`timescale 1ns / 1ps
// synthesis translate_on

// *******************************************************************************************************
// **   MODULE
// *******************************************************************************************************
module fiftyfivenm_adcblock_top_wrapper(
    //reset,               // reset
    chsel,               // 5-bits channel selection.
    soc,                 // signal Start-of-Conversion to ADC
    eoc,                 // signal end of conversion. Data can be latched on the positive edge of clkout_adccore after this signal becomes high.  EOC becomes low at every positive edge of the clkout_adccore signal.
    dout,                // 12-bits DOUT valid after EOC rise, still valid at falling edge, but not before the next EOC rising edge
    usr_pwd,             // User Power Down during run time.  0 = Power Up;  1 = Power Down.
    tsen,                // 0 = Normal Mode; 1 = Temperature Sensing Mode.
    clkout_adccore,      // Output clock from the clock divider
    clkin_from_pll_c0   // Clock source from PLL1 c-counter[0] at BL corner or PLL3 c-counter[0] at TL corner
    //dout_ch              // Indicator to tell that current dout data is for which channel
);

// *******************************************************************************************************
// **   PARAMETERS
// *******************************************************************************************************

// -------------------------------------------------------------------------------------------------------
//      WRAPPED PARAMETERS USED BY fiftyfivenm_adcblock
// -------------------------------------------------------------------------------------------------------

    // 3-bits 1st stage clock divider.
    // 0..5 = DIV by 1/2/10/20/40/80;
    // 6..7 = invalid
    parameter           clkdiv    = 1;

    // 2nd stage clock divider.
    // 0 = DIV by 10;
    // 1 = DIV by 20
    parameter           tsclkdiv  = 0;

    // 0 = Use 1st stage clock divider when TSEN.
    // 1 = Use 2nd stage clock divider when TSEN.
    parameter           tsclksel  = 0;

    // 2-bits To enable the R ladder for the prescalar input channels.
    // 00 = disable prescalar for CH8 and CH16 (CH8 for device with 2nd ADC)
    // 01 = enable prescalar for CH8 only
    // 10 = enable prescalar for CH16 only (CH8 for device with 2nd ADC)
    // 11 = enable prescalar for CH8 and CH16 (CH8 for device with 2nd ADC)
    // please note that this is not supported in VR mode
    parameter           prescalar = 0;

    // Reference voltage selection for ADC.
    // 0 = external;
    // 1 = internal VCCADC_2P5
    parameter           refsel    = 0;

// -------------------------------------------------------------------------------------------------------
//      WRAPPED PARAMETERS  USED BY both chsel_code_converter_sw_to_hw and fiftyfivenm_adcblock
// -------------------------------------------------------------------------------------------------------

    // Ordering Part Number 10MXX... code
    parameter           device_partname_fivechar_prefix = "10M08";      // Valid options = 04, 08, 16, 25, 40, 50

    // Some part have two ADC, instantiate which ADC?  1st or 2nd?
    parameter           is_this_first_or_second_adc = 1;    // Valid options = 1 or 2

    // 16 bits to indicate whether each of the dual purpose analog input pin (ADCIN) is in use or not.
    // 1 bit to indicate whether dedicated analog input pin (ANAIN) is in use or not (bit 16) 
    parameter           analog_input_pin_mask = 17'h0;
    
    // Power Down. Use to disable the ADC during compilation time if no ADC is in use.
    parameter           hard_pwd = 0;

    // Logic simulation parameters which only affects simulation behavior
    parameter           enable_usr_sim = 0;
    parameter           reference_voltage_sim = 65536;
    parameter           simfilename_ch0 = "simfilename_ch0";
    parameter           simfilename_ch1 = "simfilename_ch1";
    parameter           simfilename_ch2 = "simfilename_ch2";
    parameter           simfilename_ch3 = "simfilename_ch3";
    parameter           simfilename_ch4 = "simfilename_ch4";
    parameter           simfilename_ch5 = "simfilename_ch5";
    parameter           simfilename_ch6 = "simfilename_ch6";
    parameter           simfilename_ch7 = "simfilename_ch7";
    parameter           simfilename_ch8 = "simfilename_ch8";
    parameter           simfilename_ch9 = "simfilename_ch9";
    parameter           simfilename_ch10 = "simfilename_ch10";
    parameter           simfilename_ch11 = "simfilename_ch11";
    parameter           simfilename_ch12 = "simfilename_ch12";
    parameter           simfilename_ch13 = "simfilename_ch13";
    parameter           simfilename_ch14 = "simfilename_ch14";
    parameter           simfilename_ch15 = "simfilename_ch15";
    parameter           simfilename_ch16 = "simfilename_ch16";

// *******************************************************************************************************
// **   INPUTS
// *******************************************************************************************************

    //input               reset;      // reset

    input               clkin_from_pll_c0;  // Clock source from PLL1 c-counter[0] at BL corner or PLL3 c-counter[0] at TL corner

    input               soc;        // signal Start-of-Conversion to ADC
    input               usr_pwd;    // User Power Down during run time.  0 = Power Up;  1 = Power Down.
    input               tsen;       // 0 = Normal Mode; 1 = Temperature Sensing Mode.
    input   [4:0]       chsel;      // 5-bits channel selection.

// *******************************************************************************************************
// **   OUTPUTS
// *******************************************************************************************************

    output              clkout_adccore;  // Output clock from the clock divider
    output              eoc;        // signal end of conversion. Data can be latched on the positive edge of clkout_adccore after this signal becomes high.  EOC becomes low at every positive edge of the clkout_adccore signal.
    output [11:0]       dout;       // 12-bits DOUT valid after EOC rise, still valid at falling edge, but not before the next EOC rising edge
    //output  [4:0]       dout_ch;    // Indicator to tell that current dout data is for which channel (clone of EOC)

// *******************************************************************************************************
// **   INTERNAL NET AND REGISTER DATA TYPE (internal signals used within this module)
// *******************************************************************************************************

    //reg     [4:0]       prev1_chsel; // Backup chsel, because the data available at EOC is always for chsel value at previous EOC
    //reg     [4:0]       prev2_chsel; // Data for previous channel is further delay by 1 clkout_adccore due to adcblock_primitive_wrapper have a register clocked by clkout_adccore
    //reg     [4:0]       dout_ch;     // Indicator to tell that current dout data is for which channel

   // chsel_to_hw is the output after port mapping will be wired into chsel of fiftyfivenm_adcblock
    wire    [4:0]       chsel_to_hw;

// *******************************************************************************************************
// **   INITIALIZATION
// *******************************************************************************************************

    //initial begin
        //prev1_chsel = 0;
        //prev2_chsel = 0;
        //dout_ch    = 0;
    //end

// *******************************************************************************************************
// **   MAIN CODE
// *******************************************************************************************************

// -------------------------------------------------------------------------------------------------------
//      1.00: Instantiate Software to Hardware Channel Mapping module
// -------------------------------------------------------------------------------------------------------

    chsel_code_converter_sw_to_hw decoder(
        // The 5-bits chsel input port are wired to chsel_from_sw for conversion
        .chsel_from_sw(chsel),
        // The chsel_code_converter_sw_to_hw output chsel_to_hw which will be wired into fiftyfivenm_adcblock
        .chsel_to_hw(chsel_to_hw)
    );
    defparam           decoder.device_partname_fivechar_prefix = device_partname_fivechar_prefix;
    defparam           decoder.is_this_first_or_second_adc     = is_this_first_or_second_adc;

// -------------------------------------------------------------------------------------------------------
//      2.00: Instantiate ADC Block primitive
// -------------------------------------------------------------------------------------------------------

    fiftyfivenm_adcblock_primitive_wrapper adcblock_instance(
       // .reset            (reset),
        .chsel            (chsel_to_hw),
        .soc              (soc),
        .eoc              (eoc),
        .dout             (dout),
        .usr_pwd          (usr_pwd),
        .tsen             (tsen),
        .clkout_adccore   (clkout_adccore),
        .clkin_from_pll_c0(clkin_from_pll_c0)
    );
    defparam  adcblock_instance.clkdiv                          = clkdiv;
    defparam  adcblock_instance.tsclkdiv                        = tsclkdiv;
    defparam  adcblock_instance.tsclksel                        = tsclksel;
    defparam  adcblock_instance.prescalar                       = prescalar;
    defparam  adcblock_instance.refsel                          = refsel;
    defparam  adcblock_instance.device_partname_fivechar_prefix = device_partname_fivechar_prefix;
    defparam  adcblock_instance.is_this_first_or_second_adc     = is_this_first_or_second_adc;
    defparam  adcblock_instance.analog_input_pin_mask           = analog_input_pin_mask;
    defparam  adcblock_instance.hard_pwd                        = hard_pwd;
    defparam  adcblock_instance.enable_usr_sim                  = enable_usr_sim;
    defparam  adcblock_instance.reference_voltage_sim           = reference_voltage_sim;
    defparam  adcblock_instance.simfilename_ch0                 = simfilename_ch0;
    defparam  adcblock_instance.simfilename_ch1                 = simfilename_ch1;
    defparam  adcblock_instance.simfilename_ch2                 = simfilename_ch2;
    defparam  adcblock_instance.simfilename_ch3                 = simfilename_ch3;
    defparam  adcblock_instance.simfilename_ch4                 = simfilename_ch4;
    defparam  adcblock_instance.simfilename_ch5                 = simfilename_ch5;
    defparam  adcblock_instance.simfilename_ch6                 = simfilename_ch6;
    defparam  adcblock_instance.simfilename_ch7                 = simfilename_ch7;
    defparam  adcblock_instance.simfilename_ch8                 = simfilename_ch8;
    defparam  adcblock_instance.simfilename_ch9                 = simfilename_ch9;
    defparam  adcblock_instance.simfilename_ch10                = simfilename_ch10;
    defparam  adcblock_instance.simfilename_ch11                = simfilename_ch11;
    defparam  adcblock_instance.simfilename_ch12                = simfilename_ch12;
    defparam  adcblock_instance.simfilename_ch13                = simfilename_ch13;
    defparam  adcblock_instance.simfilename_ch14                = simfilename_ch14;
    defparam  adcblock_instance.simfilename_ch15                = simfilename_ch15;
    defparam  adcblock_instance.simfilename_ch16                = simfilename_ch16;

// -------------------------------------------------------------------------------------------------------
//      4.00: Update dout_ch to tell current dout data is for which channel
// -------------------------------------------------------------------------------------------------------

    // DESCRIBE THE ALWAYS BLOCK:
    // output previously backed-up chsel to dout_ch
    // backup existing chsel to later use
   // always @ (posedge clkout_adccore or posedge reset)
   // begin
   //     if (reset)
   //     begin
   //         dout_ch     <= 0;
   //         prev2_chsel <= 0;
   //         prev1_chsel <= 0;
   //     end
   //     else
   //     begin
   //         dout_ch     <= prev2_chsel;
   //         prev2_chsel <= prev1_chsel;
   //         prev1_chsel <= chsel;
   //     end //end-if
   // end

// *******************************************************************************************************
// **   END OF MODULE
// *******************************************************************************************************

endmodule

