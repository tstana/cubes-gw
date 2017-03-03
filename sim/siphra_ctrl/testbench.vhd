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

  constant c_num_addr_bits : natural :=  7;
  constant c_num_data_bits : natural := 26;

  --============================================================================
  -- Component declarations
  --============================================================================
  component siphra_ctrl is
    generic
    (
      g_num_addr_bits : natural :=  7;
      g_num_data_bits : natural := 26
    );
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
      reg_addr_i        : in  std_logic_vector(g_num_addr_bits-1 downto 0);
      reg_data_i        : in  std_logic_vector(g_num_data_bits-1 downto 0);
      reg_data_o        : out std_logic_vector(g_num_data_bits-1 downto 0);
      
      -- Register operation done
      reg_op_ready_o    : out std_logic;
      
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
  signal reg_addr       : std_logic_vector(c_num_addr_bits-1 downto 0);
  signal reg_data_in    : std_logic_vector(c_num_data_bits-1 downto 0);
  signal reg_data_out   : std_logic_vector(c_num_data_bits-1 downto 0);
  
  signal spi_cs_n_o     : std_logic;
  signal spi_mosi_o     : std_logic;
  signal spi_miso_i     : std_logic;
  signal spi_sclk_o     : std_logic;

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
      g_num_addr_bits => c_num_addr_bits,
      g_num_data_bits => c_num_data_bits
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
  p_stim : process is
  begin
    reg_addr    <= (others => '0');
    reg_data_in <= (others => '0');
    reg_op      <= '0';
    reg_op_start_p <= '0';

    wait for 200 ns;
    wait until rising_edge(clk_100meg);
    
    reg_addr    <= f_siphra_addr7bit(c_ctrl_ch_11);
    reg_data_in <= "00" & x"abcd01";
    reg_op      <= '1';
    reg_op_start_p <= '1';
    
    wait until rising_edge(clk_100meg);
    
    reg_op_start_p <= '0';
    
    wait until reg_op_ready = '1';
    
    wait for 5 us;
    wait until rising_edge(clk_100meg);
    
    reg_addr    <= f_siphra_addr7bit(c_cal_ctrl);
    reg_data_in <= "00" & x"000031";
    reg_op      <= '1';
    reg_op_start_p <= '1';
    
    wait until rising_edge(clk_100meg);
    
    reg_op_start_p <= '0';
    
    wait until reg_op_ready = '1';
    
    report "Stimuli process done!" severity Note;
  
    wait;
  end process p_stim;

  --============================================================================
  -- Monitor
  --============================================================================
  p_mon : process is
    variable bits_in_reg  : natural;
    variable addr_in      : std_logic_vector(c_num_addr_bits-1 downto 0);
    variable reg_in       : std_logic_vector(c_num_data_bits-1 downto 0);
    variable reg_out      : std_logic_vector(c_num_data_bits-1 downto 0);
  begin
    -- Wait for stimuli to start an operation
    wait until reg_op_start_p = '1';
    bits_in_reg := f_siphra_reg_width(reg_addr);
    addr_in := reg_addr;
    reg_in := reg_data_in;
    
    -- Wait for siphra_ctrl to finish operation and check appropriate number
    -- of bits at the siphra_ctrl register output is equal to what we input
    wait until reg_op_ready = '1';
    wait until rising_edge(clk_100meg);
    wait until rising_edge(clk_100meg);
    if (bits_in_reg = c_num_data_bits) then
      reg_out := reg_data_out;
    else
      reg_out(bits_in_reg-1 downto 0) := reg_data_out(bits_in_reg-1 downto 0);
      reg_out(c_num_data_bits-1 downto bits_in_reg) := (others => '0');
    end if;

    report "IN  : " & f_bits2string(reg_in);
    report "OUT : " & f_bits2string(reg_out);
    
  end process p_mon;
  
end architecture behav;
--==============================================================================
--  architecture end
--==============================================================================
