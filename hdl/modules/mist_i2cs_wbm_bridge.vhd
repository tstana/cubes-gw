--==============================================================================
-- I2C to Wishbone master bridge using the MIST OBC protocol
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


entity mist_i2cs_wbm_bridge is
  port
  (
    -- Clock, active-low reset
    clk_i       : in  std_logic;
    rst_n_a_i   : in  std_logic;
    
    -- I2C lines
    scl_i       : in  std_logic;
    scl_o       : out std_logic;
    scl_en_o    : out std_logic;
    sda_i       : in  std_logic;
    sda_o       : out std_logic;
    sda_en_o    : out std_logic;

    -- I2C address
    i2c_addr_i  : in  std_logic_vector(6 downto 0);

    -- Status outputs
    -- TIP  : Transfer In Progress
    --        '1' when the I2C slave detects a matching I2C address, thus a
    --            transfer is in progress
    --        '0' when idle
    -- ERR  : Error
    --       '1' when the SysMon attempts to access an invalid WB slave
    --       '0' when idle
    -- WDTO : Watchdog timeout (single clock cycle pulse)
    --        '1' -- timeout of watchdog occured
    --        '0' -- when idle
    tip_o       : out std_logic;
    err_p_o     : out std_logic;
    wdto_p_o    : out std_logic;

    -- Wishbone master signals
    wbm_stb_o   : out std_logic;
    wbm_cyc_o   : out std_logic;
    wbm_sel_o   : out std_logic_vector(3 downto 0);
    wbm_we_o    : out std_logic;
    wbm_dat_i   : in  std_logic_vector(31 downto 0);
    wbm_dat_o   : out std_logic_vector(31 downto 0);
    wbm_adr_o   : out std_logic_vector(31 downto 0);
    wbm_ack_i   : in  std_logic;
    wbm_rty_i   : in  std_logic;
    wbm_err_i   : in  std_logic;
    
    
    -- TEMPORARY: UART RX and TX
    rxd_i       : in  std_logic;
    txd_o       : out std_logic
  );
end entity mist_i2cs_wbm_bridge;


architecture behav of mist_i2cs_wbm_bridge is

  --============================================================================
  -- Type declarations
  --============================================================================
  type t_state is (
    IDLE,
    DECODE_MSG_ID,
    EXEC_CMD
  );

  --============================================================================
  -- Constant declarations
  --============================================================================

  --============================================================================
  -- Component declarations
  --============================================================================
  component mist_uart_wrapper is
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
  end component mist_uart_wrapper;

  --============================================================================
  -- Signal declarations
  --============================================================================
  signal state                  : t_state;
  
  signal i2c_tx_byte            : std_logic_vector(7 downto 0);
  signal i2c_rx_byte            : std_logic_vector(7 downto 0);
  
  signal uart_wrapper_stop      : std_logic;
  
  signal i2c_addr_match_p       : std_logic;
  signal i2c_op                 : std_logic;
  
  signal i2c_r_done_p           : std_logic;
  signal i2c_w_done_p           : std_logic;

--==============================================================================
--  architecture begin
--==============================================================================
begin

  --============================================================================
  -- Instantiate I2C slave module
  --============================================================================
  cmp_i2c_slave : mist_uart_wrapper
    port map
    (
      -- Clock, reset
      clk_i           => clk_i,
      rst_n_a_i       => rst_n_a_i,

      -- UART pins
      rxd_i           => rxd_i,
      txd_o           => txd_o,

      -- Parallel data I/O to fabric
      tx_data_i       => i2c_tx_byte,
      rx_data_o       => i2c_rx_byte,

      -- I2C stop condition as detected by external module
      sto_p_i         => uart_wrapper_stop,

      -- I2C address input and match pulse output
      addr_i          => i2c_addr_i,
      addr_match_p_o  => i2c_addr_match_p,

      -- I2C operation detected from address byte
      oper_o          => i2c_op,
      
      -- Read, write done pulses
      r_done_p_o      => i2c_r_done_p,
      w_done_p_o      => i2c_w_done_p
    );


end architecture behav;
--==============================================================================
--  architecture end
--==============================================================================
