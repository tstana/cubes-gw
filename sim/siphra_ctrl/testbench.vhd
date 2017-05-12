--==============================================================================
-- Testbench for SPI master
--==============================================================================
--
-- author: Theodor Stana (theodor.stana@gmail.com)
--
-- date of creation: 2017-03-02
--
-- version: 1.0
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

use work.gencores_pkg.all;
use work.wishbone_pkg.all;
use work.siphra_pkg.all;


entity testbench is
end entity testbench;


architecture behav of testbench is

  --============================================================================
  -- Constant declarations
  --============================================================================
  constant c_clk_per : time := 10 ns;

  --============================================================================
  -- Component declarations
  --============================================================================
  component siphra_ctrl is
    port
    (
      ---------------------------------------------------------------------------
      -- Clock, active-low async. reset
      ---------------------------------------------------------------------------
      clk_i             : in  std_logic;
      rst_n_a_i         : in  std_logic;
      
      ---------------------------------------------------------------------------
      -- SIPHRA register ports
      ---------------------------------------------------------------------------
      -- Start register operation
      reg_op_start_p_i  : in  std_logic;

      -- Operation to perform: '0' - read / '1' - write
      reg_op_i          : in  std_logic;
      
      -- Register address and data
      reg_addr_i        : in  std_logic_vector( 6 downto 0);
      reg_data_i        : in  std_logic_vector(31 downto 0);
      reg_data_o        : out std_logic_vector(31 downto 0);
      
      -- Register operation done
      reg_op_ready_o    : out std_logic;
      
      ---------------------------------------------------------------------------
      -- SIPHRA SYSCLK port
      ---------------------------------------------------------------------------
      sysclk_o          : out std_logic;
      
      ---------------------------------------------------------------------------
      -- SIPHRA ADC readout ports
      ---------------------------------------------------------------------------
      txd_i             : in  std_logic;
      adc_value_o       : out std_logic_vector(11 downto 0);
      adc_chan_o        : out std_logic_vector( 4 downto 0);
      adc_trig_type_o   : out std_logic_vector( 1 downto 0);
      adc_valid_o       : out std_logic;
      
      ---------------------------------------------------------------------------
      -- SPI ports
      ---------------------------------------------------------------------------
      spi_cs_n_o        : out std_logic;
      spi_sclk_o        : out std_logic;
      spi_mosi_o        : out std_logic;
      spi_miso_i        : in  std_logic
    );
  end component siphra_ctrl;


  --============================================================================
  -- Signal declarations
  --============================================================================
  signal clk_100meg     : std_logic;
  signal rst_n          : std_logic;

  signal reg_op_start_p : std_logic;
  signal reg_op         : std_logic;
  signal reg_op_ready   : std_logic;
  signal reg_addr       : std_logic_vector( 6 downto 0);
  signal reg_data_in    : std_logic_vector(31 downto 0);
  signal reg_data_out   : std_logic_vector(31 downto 0);
  
  signal spi_cs_n_o     : std_logic;
  signal spi_mosi_o     : std_logic;
  signal spi_miso_i     : std_logic;
  signal spi_sclk_o     : std_logic;
  
  signal siphra_sysclk  : std_logic;
  signal siphra_txd     : std_logic;
  
  signal adc_value      : std_logic_vector(11 downto 0);
  signal adc_chan       : std_logic_vector( 4 downto 0);
  signal adc_trig_type  : std_logic_vector( 1 downto 0);
  signal adc_valid      : std_logic;
  
  signal siphra_adc_delay       : unsigned( 7 downto 0);
  signal siphra_adc_value       : unsigned(11 downto 0);
  signal siphra_adc_chan        : unsigned( 4 downto 0);
  signal siphra_adc_trig_type   : unsigned( 1 downto 0);
  signal siphra_adc_tf          : unsigned( 0 downto 0);
  signal siphra_adc_shifting    : std_logic;
  signal siphra_adc_sreg        : unsigned(21 downto 0);
  signal siphra_adc_sreg_count  : unsigned(4 downto 0);

--==============================================================================
--  architecture begin
--==============================================================================
begin

  --============================================================================
  -- Clock and reset
  --============================================================================
  p_clk : process is
  begin
    clk_100meg <= '0';
    wait for c_clk_per/2;
    clk_100meg <= '1';
    wait for c_clk_per/2;
  end process p_clk;
  
  p_rst : process is
  begin
    rst_n <= '0';
    wait for c_clk_per*5;
    rst_n <= '1';
    wait;
  end process p_rst;
  
  --============================================================================
  -- DUT instantiation
  --============================================================================
  DUT : siphra_ctrl
    generic map
    (
      -- Number of address bits of a SIPHRA register
      g_reg_addr_bits     => c_num_addr_bits,

      -- Max. number of bits existing for a SIPHRA register
      g_reg_data_bits_max => c_num_data_bits
    )
    port map
    (
      ---------------------------------------------------------------------------
      -- Clock, active-low async. reset
      ---------------------------------------------------------------------------
      clk_i             => clk_100meg,
      rst_n_a_i         => rst_n,
      
      ---------------------------------------------------------------------------
      -- SIPHRA register ports
      ---------------------------------------------------------------------------
      -- Start register operation
      reg_op_start_p_i  => reg_op_start_p,

      -- Operation to perform: '0' - read / '1' - write
      reg_op_i          => reg_op,
      
      -- Register address and data
      reg_addr_i        => reg_addr,
      reg_data_i        => reg_data_in,
      reg_data_o        => reg_data_out,
      
      -- Register operation done
      reg_op_ready_o    => reg_op_ready,
      
      ---------------------------------------------------------------------------
      -- SIPHRA SYSCLK port
      ---------------------------------------------------------------------------
      sysclk_o          => siphra_sysclk,
      
      ---------------------------------------------------------------------------
      -- SIPHRA ADC readout ports
      ---------------------------------------------------------------------------
      txd_i             => siphra_txd,
      adc_value_o       => adc_value,
      adc_chan_o        => adc_chan,
      adc_trig_type_o   => adc_trig_type,
      adc_valid_o       => adc_valid,

      ---------------------------------------------------------------------------
      -- SPI ports
      ---------------------------------------------------------------------------
      spi_cs_n_o        => spi_cs_n_o,
      spi_sclk_o        => spi_sclk_o,
      spi_mosi_o        => spi_mosi_o,
      spi_miso_i        => spi_mosi_o
    );


  --============================================================================
  -- Stimuli
  --============================================================================
--  p_stim : process is
--  begin
--    reg_addr    <= (others => '0');
--    reg_data_in <= (others => '0');
--    reg_op      <= '0';
--    reg_op_start_p <= '0';
--
--    wait for 200 ns;
--    wait until rising_edge(clk_100meg);
--    
--    reg_addr    <= f_siphra_addr7bit(c_ctrl_ch_11);
--    reg_data_in <= "00" & x"abcd01";
--    reg_op      <= '1';
--    reg_op_start_p <= '1';
--    
--    wait until rising_edge(clk_100meg);
--    
--    reg_op_start_p <= '0';
--    
--    wait until reg_op_ready = '1';
--    
--    wait for 5 us;
--    wait until rising_edge(clk_100meg);
--    
--    reg_addr    <= f_siphra_addr7bit(c_cal_ctrl);
--    reg_data_in <= "00" & x"000031";
--    reg_op      <= '1';
--    reg_op_start_p <= '1';
--    
--    wait until rising_edge(clk_100meg);
--    
--    reg_op_start_p <= '0';
--    
--    wait until reg_op_ready = '1';
--    
--    report "Stimuli process done!" severity Note;
--  
--    wait;
--  end process p_stim;
  
  -- Simulate SIHPRA ADC sending values
  siphra_adc_tf <= "1";
  siphra_adc_trig_type <= "00";
  
  p_siphra_adc_sreg : process (siphra_sysclk, rst_n) is
  begin
    if (rst_n = '0') then
      siphra_adc_delay <= (others => '0');
      siphra_adc_sreg <= (others => '0');
      siphra_adc_sreg_count <= (others => '0');
      siphra_adc_shifting <= '0';
      siphra_adc_value <= (others => '0');
      siphra_adc_chan <= (others => '0');
    elsif rising_edge(siphra_sysclk) then
      if (siphra_adc_shifting = '0') then
        siphra_adc_delay <= siphra_adc_delay + 1;
        if (siphra_adc_delay = (siphra_adc_delay'range => '1')) then
          -- Prepare shift register and start shifting
          siphra_adc_sreg <= '1' & siphra_adc_tf & siphra_adc_chan &
                             siphra_adc_trig_type & siphra_adc_value & '0';
          siphra_adc_shifting <= '1';

          -- Prepare next ADC value and channel to send on next ADC sending cycle
          siphra_adc_value <= siphra_adc_value + 20;
          siphra_adc_chan <= siphra_adc_chan + 1;
          if (siphra_adc_chan = 18) then
            siphra_adc_chan <= (others => '0');
          end if;
        end if;
      else
        siphra_adc_sreg <= siphra_adc_sreg(20 downto 0) & '0';
        siphra_adc_sreg_count <= siphra_adc_sreg_count + 1;
        if (siphra_adc_sreg_count = 21) then
          siphra_adc_sreg_count <= (others => '0');
          siphra_adc_shifting <= '0';
        end if;
      end if;
      
      -- Finally, assign the TXD signal on each SYSCLK cycle
      siphra_txd <= std_logic(siphra_adc_sreg(21));
    end if;
  end process;

  --============================================================================
  -- Monitor
  --============================================================================
--  p_mon : process is
--    variable bits_in_reg  : natural;
--    variable addr_in      : std_logic_vector(c_num_addr_bits-1 downto 0);
--    variable reg_in       : std_logic_vector(c_num_data_bits-1 downto 0);
--    variable reg_out      : std_logic_vector(c_num_data_bits-1 downto 0);
--  begin
--    -- Wait for stimuli to start an operation
--    wait until reg_op_start_p = '1';
--    bits_in_reg := f_siphra_reg_width(reg_addr);
--    addr_in := reg_addr;
--    reg_in := reg_data_in;
--    
--    -- Wait for siphra_ctrl to finish operation and check appropriate number
--    -- of bits at the siphra_ctrl register output is equal to what we input
--    wait until reg_op_ready = '1';
--    wait until rising_edge(clk_100meg);
--    wait until rising_edge(clk_100meg);
--    if (bits_in_reg = c_num_data_bits) then
--      reg_out := reg_data_out;
--    else
--      reg_out(bits_in_reg-1 downto 0) := reg_data_out(bits_in_reg-1 downto 0);
--      reg_out(c_num_data_bits-1 downto bits_in_reg) := (others => '0');
--    end if;
--
--    report "IN  : " & f_bits2string(reg_in);
--    report "OUT : " & f_bits2string(reg_out);
--    
--  end process p_mon;
  
end architecture behav;
--==============================================================================
--  architecture end
--==============================================================================
