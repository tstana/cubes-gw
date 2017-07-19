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
  type t_trans_state is (
    IDLE,
    
    TRANS_HEADER,

    RX_F_ACK,
    RX_T_ACK,
    
    TX_F_ACK,
    TX_T_ACK,

    RX_HEADER_FRAME,
    TX_HEADER_FRAME,

    TX_DATA_FRAME,
    RX_DATA_FRAME,

    APPLY_DATA_FRAME
  );
  
  type t_frame_state is (
    WAIT_I2C_ADDR,
    
    RX_HEADER_BYTES,
    TX_HEADER_BYTES,

    RX_DATA_BYTE,
    TX_DATA_BYTE
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
  constant OP_NULL            : std_logic_vector(6 downto 0) := to7bits(x"00");
  constant OP_DATA_FRAME      : std_logic_vector(6 downto 0) := to7bits(x"01");
  constant OP_F_ACK           : std_logic_vector(6 downto 0) := to7bits(x"02");
  constant OP_T_ACK           : std_logic_vector(6 downto 0) := to7bits(x"03");
  constant OP_READ_ALL_REGS   : std_logic_vector(6 downto 0) := to7bits(x"11");
  constant OP_GET_CUBES_ID    : std_logic_vector(6 downto 0) := to7bits(x"40");
  constant OP_SET_LEDS        : std_logic_vector(6 downto 0) := to7bits(x"41");
  constant OP_GET_LEDS        : std_logic_vector(6 downto 0) := to7bits(x"42");
  constant OP_SIPHRA_REG_OP   : std_logic_vector(6 downto 0) := to7bits(x"43");
  constant OP_GET_SIPHRA_DATAR: std_logic_vector(6 downto 0) := to7bits(x"44");
  constant OP_GET_SIPHRA_ADCR : std_logic_vector(6 downto 0) := to7bits(x"45");
  constant OP_GET_CH_REG_MASK : std_logic_vector(6 downto 4) := to3bits(x"5");

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
  -- State signals for FSMs
  signal frame_state            : t_frame_state;
  signal trans_state            : t_trans_state;
  
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
  signal rx_fid, tx_fid         : std_logic;
  signal fid_prev               : std_logic;
  signal tid                    : std_logic;
  signal rx_opcode, tx_opcode   : std_logic_vector(6 downto 0);
  signal rx_data_len            : std_logic_vector(OBC_DL_WIDTH-1 downto 0);
  signal tx_data_len            : std_logic_vector(OBC_DL_WIDTH-1 downto 0);
  
  signal frame_rxed_p           : std_logic;
  signal frame_txed_p           : std_logic;
  signal frame_byte_count       : unsigned(8 downto 0);   -- NB: Needs constant!!!
  signal trans_byte_count       : unsigned(OBC_DL_WIDTH-1 downto 0);
  
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
  U_HW_COMM : mist_uart_wrapper
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

  --============================================================================
  -- Transaction FSM - handles sending of frames within a transaction
  --============================================================================
  P_TRANS_FSM : process (clk_i, rst_n_a_i) is
  begin
    if (rst_n_a_i = '0') then
      trans_state <= IDLE;
      
    elsif rising_edge(clk_i) then
      
      case trans_state is
        when IDLE =>
          if (i2c_addr_match_p = '1') then
            trans_state <= TRANS_HEADER;
          end if;
          
        when TRANS_HEADER =>
          if (frame_rxed_p = '1') then
            case rx_opcode is
              when OP_SET_LEDS =>
                trans_state <= TX_F_ACK;
              when others =>
                trans_state <= TX_T_ACK;
            end case;
          end if;
          
        when RX_F_ACK =>
          if (frame_rxed_p = '1') then
            if (trans_byte_count /= 0) then
              trans_state <= TX_DATA_FRAME;
            end if;
          end if;
          
        when TX_F_ACK =>
          if (frame_txed_p = '1') then
            if (trans_byte_count /= 0) then
              trans_state <= RX_DATA_FRAME;
            end if;
          end if;
          
        when TX_DATA_FRAME =>
          if (frame_txed_p = '1') then
            if (trans_byte_count = 0) then
              trans_state <= RX_T_ACK;
            else
              trans_state <= RX_F_ACK;
            end if;
          end if;
          
        when RX_T_ACK =>
          if (frame_rxed_p = '1') then
            trans_state <= IDLE;
          end if;
          
        when TX_T_ACK =>
          if (frame_txed_p = '1') then
            trans_state <= IDLE;
          end if;
          
        when others =>
          trans_state <= IDLE;
          
      end case;
      
    end if;
  end process;

  --============================================================================
  -- Frame FSM - handles sending of bytes within a frame
  --============================================================================
  P_FRAME_FSM : process (clk_i, rst_n_a_i)
  begin
    if (rst_n_a_i = '0') then
      frame_state <= WAIT_I2C_ADDR;
      
      frame_rxed_p <= '0';
      frame_txed_p <= '0';
      
      uart_wrapper_stop_p <= '0';
      
      frame_byte_count <= (others => '0');
      trans_byte_count <= (others => '0');
      
      rx_fid <= '0';
      tid <= '0';
      fid_prev <= '0';
      rx_opcode <= (others => '0');
      rx_data_len <= (others => '0');
      
    elsif rising_edge(clk_i) then
    
      frame_rxed_p <= '0';
      frame_txed_p <= '0';
      
      uart_wrapper_stop_p <= '0';
      
      case frame_state is
      
        when WAIT_I2C_ADDR =>
          frame_byte_count <= (others => '0');
          if (i2c_addr_match_p = '1') then
            case trans_state is
              when IDLE =>
                frame_state <= RX_HEADER_BYTES;
              when RX_F_ACK =>
                frame_state <= RX_HEADER_BYTES;
              when TX_F_ACK =>
                frame_state <= TX_HEADER_BYTES;
              when others =>
                null;
            end case;
          end if;
          
        when RX_HEADER_BYTES =>
          if (i2c_r_done_p = '1') then
            frame_byte_count <= frame_byte_count + 1;
            if (frame_byte_count = 0) then
              rx_fid <= i2c_rx_byte(7);
              if (trans_state = TRANS_HEADER) then
                tid <= i2c_rx_byte(7);
              end if;
              rx_opcode <= i2c_rx_byte(6 downto 0);
            elsif (frame_byte_count < 5) then
              rx_data_len <= rx_data_len(OBC_DL_WIDTH-9 downto 0) & i2c_rx_byte;
            else
              trans_byte_count <= unsigned(rx_data_len);
              -- TODO: Check here for correct FID and signal error otherwise.
              fid_prev <= rx_fid;

              frame_rxed_p <= '1';
              
              ------------------------
              -- TODO: Remove for I2C
              ------------------------
              uart_wrapper_stop_p <= '1';
              ------------------------
              frame_state <= WAIT_I2C_ADDR;
            end if;
          end if;
          
        when others =>
          frame_state <= WAIT_I2C_ADDR;   -- TODO: Different state on error?
          
      end case;
      
    end if;
  end process;

end architecture behav;
--==============================================================================
--  architecture end
--==============================================================================
