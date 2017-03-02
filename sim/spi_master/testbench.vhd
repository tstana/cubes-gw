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
  constant c_num_data_bits : natural := 32;

  --============================================================================
  -- Component declarations
  --============================================================================
    component spi_master is
      generic(
        -- clock division ratio (SCLK = clk_sys_i / (2 ** g_div_ratio_log2).
        g_div_ratio_log2 : integer := 2;

        -- data bits generic or from port?
        g_data_bits_generic : boolean := true;

        -- number of data bits per transfer; if from port, set to max. value expected on port
        g_num_data_bits  : integer := 2);
      port (
        clk_sys_i : in std_logic;
        rst_n_i   : in std_logic;

        -- state of the Chip select line (1 = CS active). External control
        -- allows for multi-transfer commands (SPI master itself does not
        -- control the state of spi_cs_n_o)
        cs_i : in std_logic;

        -- 1: start next transfer (using CPOL, DATA and SEL from the inputs below)
        start_p_i  : in  std_logic;

        -- Clock polarity: 1: slave clocks in the data on rising SCLK edge, 0: ...
        -- on falling SCLK edge
        cpol_i     : in  std_logic;

        -- Number of bits to send
        data_len_i : in std_logic_vector(log2_ceil(g_num_data_bits)-1 downto 0);

        -- TX Data input 
        data_i     : in  std_logic_vector(g_num_data_bits - 1 downto 0);

        -- 1: data_o contains the result of last read operation. Core is ready to initiate
        -- another transfer.
        ready_o    : out std_logic;

        -- data read from selected slave, valid when ready_o == 1.
        data_o     : out std_logic_vector(g_num_data_bits - 1 downto 0);

        -- these are obvious
        spi_cs_n_o : out std_logic;
        spi_sclk_o : out std_logic;
        spi_mosi_o : out std_logic;
        spi_miso_i : in  std_logic
        );

    end component spi_master;
  

  --============================================================================
  -- Signal declarations
  --============================================================================
  signal clk_100meg     : std_logic;
  signal rst_n          : std_logic;

  signal chip_sel       : std_logic;
  
  signal spi_start_p    : std_logic;
  signal spi_cpol       : std_logic;
  signal bits_to_send   : std_logic_vector(log2_ceil(c_num_data_bits)-1 downto 0);
  signal spi_data_in    : std_logic_vector(c_num_data_bits-1 downto 0);
  signal spi_data_out   : std_logic_vector(c_num_data_bits-1 downto 0);
  signal spi_ready      : std_logic;
  
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
  DUT : spi_master
    generic map
    (
      -- clock division ratio (SCLK = clk_sys_i / (2 ** g_div_ratio_log2).
      g_div_ratio_log2 => 7,

      -- data bits generic or from port?
      g_data_bits_generic => false,

      -- number of data bits per transfer; if from port, set to max. value expected on port
      g_num_data_bits  => c_num_data_bits
    )
    port map
    (
        clk_sys_i   => clk_100meg,
        rst_n_i     => rst_n,

        -- state of the Chip select line (1 = CS active). External control
        -- allows for multi-transfer commands (SPI master itself does not
        -- control the state of spi_cs_n_o)
        cs_i        => chip_sel,

        -- 1: start next transfer (using CPOL, DATA and SEL from the inputs below)
        start_p_i   => spi_start_p,

        -- Clock polarity: 1: slave clocks in the data on rising SCLK edge, 0: ...
        -- on falling SCLK edge
        cpol_i     => spi_cpol,

        -- Number of bits to send
        data_len_i => bits_to_send,

        -- TX Data input 
        data_i     => spi_data_in,

        -- 1: data_o contains the result of last read operation. Core is ready to initiate
        -- another transfer.
        ready_o    => spi_ready,

        -- data read from selected slave, valid when ready_o == 1.
        data_o     => spi_data_out,

        -- these are obvious
        spi_cs_n_o => spi_cs_n_o,
        spi_sclk_o => spi_sclk_o,
        spi_mosi_o => spi_mosi_o,
        spi_miso_i => spi_mosi_o
    );

  --============================================================================
  -- Stimuli
  --============================================================================
  bits_to_send <= std_logic_vector(
                    to_unsigned(32, log2_ceil(c_num_data_bits)));
  
  p_stim : process is
  begin
    chip_sel <= '0';
    spi_cpol <= '0';
    spi_data_in <= (others => '0');
    spi_start_p <= '0';
    
    chip_sel <= '0';
    wait for 200 ns;
    
    wait until rising_edge(clk_100meg);
    
    spi_data_in <= x"00330033";
    chip_sel <= '1';
    
    wait until rising_edge(clk_100meg);
    
    spi_start_p <= '1';
    wait until rising_edge(clk_100meg);
    spi_start_p <= '0';

    wait until spi_ready = '1';
    
    spi_start_p <= '0';
    chip_sel <= '0';
    
    report "Done!" severity note;
    
    wait;
  end process p_stim;

end architecture behav;
--==============================================================================
--  architecture end
--==============================================================================
