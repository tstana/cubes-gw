--==============================================================================
-- IDE3380 (SIPHRA) package
--==============================================================================
--
-- author: Theodor Stana (theodor.stana@gmail.com)
--
-- date of creation: 2017-03-02
--
--==============================================================================
-- GNU LESSER GENERAL PUBLIC LICENSE
--==============================================================================
-- This source file is free software; you can redistribute it and/or modify it
-- under the terms of the GNU Lesser General Public License as published by the
-- Free Software Foundation; either version 2.1 of the License, or (at your
-- option) any later version. This source is distributed in the hope that it
-- will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty
-- of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
-- See the GNU Lesser General Public License for more details. You should have
-- received a copy of the GNU Lesser General Public License along with this
-- source; if not, download it from http://www.gnu.org/licenses/lgpl-2.1.html
--==============================================================================
-- last changes:
--    2017-03-02   Theodor Stana     File created
--==============================================================================
-- TODO: -
--==============================================================================

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;


package siphra_pkg is

  --============================================================================
  -- Constant declarations
  --============================================================================
  ------------------------------------------------------------------------------
  -- Number of bits in packet
  ------------------------------------------------------------------------------
  constant c_siphra_num_packet_bits : natural := 40;
  
  ------------------------------------------------------------------------------
  -- SIPHRA register addresses
  ------------------------------------------------------------------------------
  constant c_ctrl_ch_01         : std_logic_vector(7 downto 0) := x"00";
  constant c_ctrl_ch_02         : std_logic_vector(7 downto 0) := x"01";
  constant c_ctrl_ch_03         : std_logic_vector(7 downto 0) := x"02";
  constant c_ctrl_ch_04         : std_logic_vector(7 downto 0) := x"03";
  constant c_ctrl_ch_05         : std_logic_vector(7 downto 0) := x"04";
  constant c_ctrl_ch_06         : std_logic_vector(7 downto 0) := x"05";
  constant c_ctrl_ch_07         : std_logic_vector(7 downto 0) := x"06";
  constant c_ctrl_ch_08         : std_logic_vector(7 downto 0) := x"07";
  constant c_ctrl_ch_09         : std_logic_vector(7 downto 0) := x"08";
  constant c_ctrl_ch_10         : std_logic_vector(7 downto 0) := x"09";
  constant c_ctrl_ch_11         : std_logic_vector(7 downto 0) := x"0a";
  constant c_ctrl_ch_12         : std_logic_vector(7 downto 0) := x"0b";
  constant c_ctrl_ch_13         : std_logic_vector(7 downto 0) := x"0c";
  constant c_ctrl_ch_14         : std_logic_vector(7 downto 0) := x"0d";
  constant c_ctrl_ch_15         : std_logic_vector(7 downto 0) := x"0e";
  constant c_ctrl_ch_16         : std_logic_vector(7 downto 0) := x"0f";
  constant c_ctrl_ch_sum        : std_logic_vector(7 downto 0) := x"10";
  constant c_channel_config     : std_logic_vector(7 downto 0) := x"11";
  constant c_channel_control    : std_logic_vector(7 downto 0) := x"12";
  constant c_adc_config         : std_logic_vector(7 downto 0) := x"13";
  constant c_cal_dac            : std_logic_vector(7 downto 0) := x"14";
  constant c_power_modules      : std_logic_vector(7 downto 0) := x"15";
  constant c_cal_ctrl           : std_logic_vector(7 downto 0) := x"16";
  constant c_readout_fixed_list : std_logic_vector(7 downto 0) := x"17";
  constant c_readout_mode       : std_logic_vector(7 downto 0) := x"18";
  constant c_amux_ctrl          : std_logic_vector(7 downto 0) := x"19";
  constant c_adc_clk_div_factor : std_logic_vector(7 downto 0) := x"1a";
  constant c_sysclock_ctrl      : std_logic_vector(7 downto 0) := x"1b";
  constant c_cmd_dcal           : std_logic_vector(7 downto 0) := x"1c";
  constant c_cmd_readout        : std_logic_vector(7 downto 0) := x"1d";
  constant c_trigger_latches    : std_logic_vector(7 downto 0) := x"1e";
  constant c_adc_out            : std_logic_vector(7 downto 0) := x"1f";
  constant c_parity_err_reg     : std_logic_vector(7 downto 0) := x"20";

  --============================================================================
  -- Function declarations
  --============================================================================
  -- 7- to 8-bit address
  -- ** input address, try to match to 8-bit definition above
  function f_siphra_addr8bit(inp : std_logic_vector(6 downto 0))
                return std_logic_vector;
  
  -- 8- to 7-bit address
  -- ** input 8-bit definition above, return 7-bit address
  function f_siphra_addr7bit(inp : std_logic_vector(7 downto 0))
                return std_logic_vector;
  
  -- Get number of bits in register based on address
  -- Returns <number of bits - 1>
  function f_siphra_reg_width(addr : std_logic_vector(6 downto 0))
                return natural;
  
end package siphra_pkg;


package body siphra_pkg is

  -- 7- to 8-bit address
  -- ** input address, try to match to 8-bit definition above
  function f_siphra_addr8bit(inp : std_logic_vector(6 downto 0))
                return std_logic_vector is
  begin
    return '0' & inp;
  end function f_siphra_addr8bit;

  -- 8- to 7-bit address
  -- ** input 8-bit definition above, return 7-bit address
  function f_siphra_addr7bit(inp : std_logic_vector(7 downto 0))
                return std_logic_vector is
  begin
    return inp(6 downto 0);
  end function f_siphra_addr7bit;

  -- Get number of bits in register based on address
  -- Returns <number of bits - 1>
  function f_siphra_reg_width(addr : std_logic_vector(6 downto 0))
                return natural is
  
    variable addr8 : std_logic_vector(7 downto 0);
    variable bits  : natural;
    
  begin
  
    addr8 := f_siphra_addr8bit(addr);
    
    case addr8 is
      when c_ctrl_ch_01 =>
        bits := 26;
        
      when c_ctrl_ch_02 =>
        bits := 26;

      when c_ctrl_ch_03 =>
        bits := 26;

      when c_ctrl_ch_04 =>
        bits := 26;

      when c_ctrl_ch_05 =>
        bits := 26;
        
      when c_ctrl_ch_06 =>
        bits := 26;

      when c_ctrl_ch_07 =>
        bits := 26;

      when c_ctrl_ch_08 =>
        bits := 26;

      when c_ctrl_ch_09 =>
        bits := 26;

      when c_ctrl_ch_10 =>
        bits := 26;

      when c_ctrl_ch_11 =>
        bits := 26;

      when c_ctrl_ch_12 =>
        bits := 26;

      when c_ctrl_ch_13 =>
        bits := 26;

      when c_ctrl_ch_14 =>
        bits := 26;

      when c_ctrl_ch_15 =>
        bits := 26;

      when c_ctrl_ch_16 =>
        bits := 26;

      when c_ctrl_ch_sum =>
        bits := 14;
        
      when c_channel_config =>
        bits := 24;
        
      when c_channel_control =>
        bits := 23;
        
      when c_adc_config =>
        bits := 6;
        
      when c_cal_dac =>
        bits := 8;
        
      when c_power_modules =>
        bits := 18;
        
      when c_cal_ctrl =>
        bits := 6;
        
      when c_readout_fixed_list =>
        bits := 19;
        
      when c_readout_mode =>
        bits := 15;
        
      when c_amux_ctrl =>
        bits := 6;
        
      when c_sysclock_ctrl =>
        bits := 2;
        
      when c_cmd_dcal =>
        bits := 1;
        
      when c_cmd_readout =>
        bits := 1;
        
      when c_trigger_latches =>
        bits := 17;
        
      when c_adc_out =>
        bits := 12;
        
      when c_parity_err_reg =>
        bits := 28;
        
      when others =>
        bits := 1;
      
    end case;
    
    return bits;
  end function f_siphra_reg_width;

end package body siphra_pkg;