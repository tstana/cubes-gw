--==============================================================================
-- Wrapper file for simulating I2C address reception
--==============================================================================
--
-- author: Theodor Stana (theodor.stana@gmail.com)
--
-- date of creation: 2017-02-23
--
-- version: 1.0
--
-- description:
--
-- dependencies:
--
-- references:
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
--    2017-02-23   Theodor Stana     File created
--==============================================================================
-- TODO: -
--==============================================================================

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.genram_pkg.all;


entity mist_uart_wrapper is
  port
  (
    -- Clock, reset
    clk_i           : in  std_logic;
    rst_n_a_i       : in  std_logic;

    -- UART pins
    rxd_i           : in  std_logic;
    txd_o           : out std_logic;

    -- Parallel data I/O to fabric
    tx_data_i       : in  std_logic_vector(7 downto 0);
    rx_data_o       : out std_logic_vector(7 downto 0);

    tx_start_p_i    : in  std_logic;
    
    -- I2C stop condition as detected by external module
    sto_p_i         : in  std_logic;

    -- I2C address input and match pulse output
    addr_i          : in  std_logic_vector(6 downto 0);
    addr_match_p_o  : out std_logic;

    -- I2C operation detected from address byte
    oper_o          : out std_logic;
    
    -- Read, write done pulses
    r_done_p_o      : out std_logic;
    w_done_p_o      : out std_logic
  );
end entity mist_uart_wrapper;


architecture behav of mist_uart_wrapper is

  --============================================================================
  -- Type declarations
  --============================================================================
  type t_i2c_mimic_state is (
    IDLE,
    DATA_RELAY
  );

  --============================================================================
  -- Constant declarations
  --============================================================================
  constant c_baud_div_int : natural := 867;       -- assumes 100 MHz clk_i
  constant c_baud_div     : std_logic_vector := 
      std_logic_vector(to_unsigned(c_baud_div_int, f_log2_size(c_baud_div_int)));

  --============================================================================
  -- Component declarations
  --============================================================================
  component uart is
    generic
    (
      g_baud_div_bits : natural := 16
    );
    port
    (
      -- Clock, reset
      clk_i         : in  std_logic;
      rst_n_a_i     : in  std_logic;

      -- Ports to external world
      rxd_i         : in  std_logic;
      txd_o         : out std_logic;

      -- Ports to other logic
      baud_div_i    : in  std_logic_vector(g_baud_div_bits-1 downto 0);

      tx_data_i     : in  std_logic_vector(7 downto 0);
      tx_start_p_i  : in  std_logic;
      tx_ready_o    : out std_logic;

      rx_ready_o    : out std_logic;
      rx_data_o     : out std_logic_vector(7 downto 0);

      frame_err_o   : out std_logic
    );
  end component uart;

  --============================================================================
  -- Signal declarations
  --============================================================================
  signal rx_data          : std_logic_vector(7 downto 0);
  signal rx_ready         : std_logic;
  signal rx_ready_d0      : std_logic;
  signal r_done_p         : std_logic;
  
  signal tx_ready         : std_logic;
  signal tx_ready_d0      : std_logic;
  
  signal i2c_mimic_state  : t_i2c_mimic_state;
  
  signal addr_match       : std_logic;
  signal addr_match_d0    : std_logic;
  
  signal i2c_op           : std_logic;

--==============================================================================
--  architecture begin
--==============================================================================
begin

  --============================================================================
  -- Instantiate UART component
  --============================================================================
  cmp_uart : uart
    generic map
    (
      g_baud_div_bits => f_log2_size(c_baud_div_int)
    )
    port map
    (
      -- Clock, reset
      clk_i         => clk_i,     -- assumed 100MHz
      rst_n_a_i     => rst_n_a_i,

      -- Ports to external world
      rxd_i         => rxd_i,
      txd_o         => txd_o,

      -- Ports to other logic
      baud_div_i    => c_baud_div,

      tx_data_i     => tx_data_i,
      tx_start_p_i  => tx_start_p_i,
      tx_ready_o    => tx_ready,

      rx_ready_o    => rx_ready,
      rx_data_o     => rx_data,
      
      frame_err_o   => open
    );
    
  --============================================================================
  -- w_done_p_o / r_done_p_o
  --============================================================================
  p_w_done : process (clk_i, rst_n_a_i) is
  begin
    if (rst_n_a_i = '0') then
      tx_ready_d0 <= '0';
      w_done_p_o  <= '0';
    elsif rising_edge(clk_i) then
      tx_ready_d0 <= tx_ready;
      w_done_p_o  <= (not tx_ready_d0) and tx_ready;
    end if;
  end process p_w_done;
    
  p_r_done : process (clk_i, rst_n_a_i) is
  begin
    if (rst_n_a_i = '0') then
      rx_ready_d0 <= '0';
      r_done_p    <= '0';
    elsif rising_edge(clk_i) then
      rx_ready_d0 <= rx_ready;
      r_done_p    <= (not rx_ready_d0) and rx_ready;
    end if;
  end process p_r_done;

  --============================================================================
  -- FSM to mimic I2C bus
  --============================================================================
  p_i2c_mimic : process (clk_i, rst_n_a_i) is
  begin
    if (rst_n_a_i = '0') then
      i2c_mimic_state <= IDLE;
      addr_match      <= '0';
      i2c_op          <= '0';
    elsif rising_edge(clk_i) then
      
      case i2c_mimic_state is
        
        when IDLE =>
          
          addr_match <= '0';
          
          if (r_done_p = '1') then
            if (rx_data(7 downto 1) = addr_i) then
              addr_match <= '1';
              i2c_op <= rx_data(0);
              i2c_mimic_state <= DATA_RELAY;
            end if;
          end if;
          
        when DATA_RELAY =>
          if (sto_p_i = '1') then
            i2c_mimic_state <= IDLE;
          end if;
          
        when others =>
          i2c_mimic_state <= IDLE;
          
      end case;
          
    end if;
  end process p_i2c_mimic;

  --============================================================================
  -- Assign remaining outputs
  --============================================================================
  p_addr_match_p : process (clk_i, rst_n_a_i) is
  begin
    if (rst_n_a_i = '0') then
      addr_match_d0   <= '0';
      addr_match_p_o  <= '0';
    elsif rising_edge(clk_i) then
      addr_match_d0   <= addr_match;
      addr_match_p_o  <= (not addr_match_d0) and addr_match;
    end if;
  end process p_addr_match_p;

  oper_o <= i2c_op;
  
  r_done_p_o <= r_done_p;
  
  rx_data_o <= rx_data;
  
end architecture behav;
--==============================================================================
--  architecture end
--==============================================================================
