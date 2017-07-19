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

use work.wishbone_pkg.all;
use work.genram_pkg.all;


entity mist_obc_interface is
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

    -- TEMPORARY: UART RX and TX
    rxd_i       : in  std_logic;
    txd_o       : out std_logic
  );
end entity mist_obc_interface;


architecture behav of mist_obc_interface is

  --============================================================================
  -- Type declarations
  --============================================================================
  type t_state is (
    IDLE,
    TRANSACTION_HEADER,

    PREP_F_ACK,
    PREP_T_ACK,
    SEND_HEADER_FRAME,
    
    RECEIVE_DATA_FRAME,
    SEND_DATA_FRAME,
    APPLY_DATA_FRAME,
    
    UART_TX_START,
    WB_CYCLE
  );

  --============================================================================
  -- Function declarations
  --============================================================================
  function to7bits(v : std_logic_vector(7 downto 0)) return std_logic_vector is
  begin
    return v(6 downto 0);
  end function;

  function to3bits(v : std_logic_vector(3 downto 0)) return std_logic_vector is
  begin
    return v(2 downto 0);
  end function;

  --============================================================================
  -- Constant declarations
  --============================================================================
  -- MSP size defines
  constant OBC_MTU            : natural       := 507;
  constant OBC_DL_WIDTH       : natural       :=  32;
  constant OBC_DL_NR_BYTES    : natural       := f_log2_size(OBC_DL_WIDTH);
  constant OBC_FCS_WIDTH      : natural       :=   0;
  constant OBC_FCS_NR_BYTES   : natural       :=   0;  -- f_log2_size(OBC_FCS_WIDTH);

  -- MSP operations
  constant OP_NULL                  : std_logic_vector(6 downto 0) := to7bits(x"00");
  constant OP_DATA_FRAME            : std_logic_vector(6 downto 0) := to7bits(x"01");
  constant OP_F_ACK                 : std_logic_vector(6 downto 0) := to7bits(x"02");
  constant OP_T_ACK                 : std_logic_vector(6 downto 0) := to7bits(x"03");
  constant OP_READ_ALL_REGS         : std_logic_vector(6 downto 0) := to7bits(x"11");
  constant OP_GET_CUBES_ID          : std_logic_vector(6 downto 0) := to7bits(x"40");
  constant OP_SET_LEDS              : std_logic_vector(6 downto 0) := to7bits(x"41");
  constant OP_GET_LEDS              : std_logic_vector(6 downto 0) := to7bits(x"42");
  constant OP_SIPHRA_REG_OP         : std_logic_vector(6 downto 0) := to7bits(x"43");
  constant OP_GET_SIPHRA_DATAR      : std_logic_vector(6 downto 0) := to7bits(x"44");
  constant OP_GET_SIPHRA_ADCR       : std_logic_vector(6 downto 0) := to7bits(x"45");
  constant OP_GET_CH_REG_MASK       : std_logic_vector(6 downto 4) := to3bits(x"5");
  constant OP_NONE                  : std_logic_vector(6 downto 0) := to7bits(x"ff");

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
  end component mist_uart_wrapper;

  --============================================================================
  -- Signal declarations
  --============================================================================
  signal state                  : t_state;
  signal state_d0               : t_state; -- one clock cycle delayed
  signal trans_state            : t_state; -- next state while in transaction
  
  -- I2C signals
  signal i2c_tx_byte            : std_logic_vector(7 downto 0);
  signal i2c_rx_byte            : std_logic_vector(7 downto 0);
  
  signal uart_wrapper_stop_p    : std_logic;
  
  signal i2c_addr_match_p       : std_logic;
  signal i2c_op                 : std_logic;

  signal i2c_r_done_p           : std_logic;
  signal i2c_w_done_p           : std_logic;
  
  -- !!! REMOVE ME !!!
  signal tx_start_p             : std_logic;

  -- OBC protocol signals
  signal fid, fid_prev, tid     : std_logic;
  signal opcode                 : std_logic_vector(6 downto 0);
  signal current_op             : std_logic_vector(6 downto 0);
  signal data_len               : std_logic_vector(OBC_DL_WIDTH-1 downto 0);
  signal data_byte_count        : unsigned(OBC_DL_WIDTH-1 downto 0);
  signal frame_byte_count       : unsigned(8 downto 0);   -- NB: Needs constant!!!
  signal nr_data_bytes          : unsigned(8 downto 0);   -- NB: Needs constant!!!
  
  signal data_buf               : std_logic_vector(7 downto 0);
  signal data_buf_addr          : unsigned(8 downto 0);   -- NB: Needs constant!!!
  signal data_buf_write_p       : std_logic;

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
      
      tx_start_p_i    => tx_start_p,

      -- I2C stop condition as detected by external module
      sto_p_i         => uart_wrapper_stop_p,

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
