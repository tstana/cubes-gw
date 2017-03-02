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


entity testbench is
end entity testbench;


architecture behav of testbench is

  --============================================================================
  -- Type declarations
  --============================================================================
  constant c_clk_per : time := 10 ns;

  --============================================================================
  -- Constant declarations
  --============================================================================
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
  signal reg_data       : std_logic_vector(c_num_data_bits-1 downto 0);
  
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
      reg_data_i        => reg_data,
      
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
  
    wait;
  end process p_stim;

end architecture behav;
--==============================================================================
--  architecture end
--==============================================================================
