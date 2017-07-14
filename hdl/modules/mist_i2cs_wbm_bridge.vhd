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

use work.mist_obc_pkg.all;
use work.wishbone_pkg.all;
use work.genram_pkg.all;


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
    wbm_i       : in  t_wishbone_master_in;
    wbm_o       : out t_wishbone_master_out;
    
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
    TRANSACTION_HEADER,
    FRAME_ACK,

    SEND_HEADER_FRAME,
    
    RECEIVE_PAYLOAD_FRAME,
    SEND_PAYLOAD_FRAME,
    UART_TX_START,
    WB_CYCLE,
    UART_WRAPPER_STOP
  );

  --============================================================================
  -- Constant declarations
  --============================================================================
  -- TODO: ----- move these to OBC package !!!
  constant OP_NULL                  : std_logic_vector(6 downto 0) := x"00";
  constant OP_DATA_FRAME            : std_logic_vector(6 downto 0) := x"01";
  constant OP_F_ACK                 : std_logic_vector(6 downto 0) := x"02";
  constant OP_T_ACK                 : std_logic_vector(6 downto 0) := x"03";
  constant OP_READ_ALL_REGS         : std_logic_vector(6 downto 0) := x"11";
  constant OP_GET_CUBES_ID          : std_logic_vector(6 downto 0) := x"40";
  constant OP_SET_LEDS              : std_logic_vector(6 downto 0) := x"41";
  constant OP_GET_LEDS              : std_logic_vector(6 downto 0) := x"42";
  constant OP_SIPHRA_REG_OP         : std_logic_vector(6 downto 0) := x"43";
  constant OP_GET_SIPHRA_DATAR      : std_logic_vector(6 downto 0) := x"44";
  constant OP_GET_SIPHRA_ADCR       : std_logic_vector(6 downto 0) := x"45";
  constant OP_GET_CH_REG_MASK       : std_logic_vector(6 downto 4) := x"5";
  constant OP_NONE                  : std_logic_vector(6 downto 0) := x"ff";

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

  -- Internal Wishbone signals
  signal wb_dat_out             : std_logic_vector(c_wishbone_data_width-1 downto 0);
  signal wb_dat_in              : std_logic_vector(c_wishbone_data_width-1 downto 0);
  signal wb_adr                 : unsigned(c_wishbone_address_width-1 downto 0);
  signal wb_cyc                 : std_logic;
  signal wb_stb                 : std_logic;
  signal wb_we                  : std_logic;
  signal wb_ack                 : std_logic;
  signal wb_err                 : std_logic;
  signal wb_rty                 : std_logic;
  signal wb_stall               : std_logic;
  
  -- OBC protocol signals
  signal fid, fid_prev, tid     : std_logic;
  signal opcode                 : std_logic_vector(6 downto 0);
  signal current_op             : std_logic_vector(6 downto 0);
  signal data_len               : std_logic_vector(OBC_DL_WIDTH-1 downto 0);
  signal data_byte_counter      : unsigned(OBC_DL_WIDTH-1 downto 0);
  signal frame_byte_counter     : unsigned(8 downto 0);   -- NB: Needs constant!!!
  signal nr_data_bytes          : unsigned(8 downto 0);   -- NB: Needs constant!!!

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

  i2c_tx_byte <= wb_dat_in(31 downto 24);

  --============================================================================
  -- Wishbone master
  --============================================================================
  -- Inputs from Wishbone bus
  wb_ack    <= wbm_i.ack;
  wb_err    <= wbm_i.err;
  wb_rty    <= wbm_i.rty;
  
  -- Outputs to Wishbone bus
  wbm_o.dat <= wb_dat_out;
  wbm_o.adr <= std_logic_vector(wb_adr);
  wbm_o.cyc <= wb_cyc;
  wbm_o.stb <= wb_stb;
  wbm_o.we  <= wb_we;
  wbm_o.sel <= (others => '1');

  -- FSM process
  p_wb_fsm : process (clk_i, rst_n_a_i) is
  begin
    if (rst_n_a_i = '0') then
      state <= IDLE;
      state_d0 <= IDLE;
      wb_cyc <= '0';
      wb_stb <= '0';
      wb_we  <= '0';
      wb_dat_in  <= (others => '0');
      wb_dat_out <= (others => '0');
      tx_start_p <= '0';
      data_len <= (others => '0');
      data_byte_counter <= (others => '0');
      frame_byte_counter <= (others => '0');
      uart_wrapper_stop_p <= '0';
      
    elsif rising_edge(clk_i) then
    
      -- Keep previous state for switching back to it
      state_d0 <= state;
      
      case state is
        
        -----------------------------------------------------------------------
        -- IDLE, waiting for frame
        -----------------------------------------------------------------------
        when IDLE =>
          wb_dat_out <= (others => '0');
          uart_wrapper_stop_p <= '0';
          frame_byte_counter <= (others => '0');
          
          if (i2c_addr_match_p = '1') and (i2c_op = '1') then
          
              case current_op is
              
                when OP_NONE =>
                  state <= TRANSACTION_HEADER;
                  state_after <= IDLE;
              
                when OP_SET_LEDS =>
                  state <= SEND_F_ACK;
                  wb_adr <= x"00000014";
                  data_byte_counter <= unsigned(data_len);
                  state_after <= RECEIVE_DATA_FRAME;
                
                when OP_GET_LEDS =>
                  wb_adr <= x"00000014";
                  data_byte_counter <= to_unsigned(3, bytes_left'length);
                  state <= WB_CYCLE;
                
                when OP_SIPHRA_REG_OP =>
                  wb_adr <= x"00000300";
                  data_byte_counter <= to_unsigned(7, bytes_left'length);
                  state <= RECEIVE_DATA;
                
                when OP_GET_SIPHRA_DATAR =>
                  wb_adr <= x"00000300";
                  data_byte_counter <= to_unsigned(3, bytes_left'length);
                  state <= WB_CYCLE;
                
                when OP_GET_SIPHRA_ADCR =>
                  wb_adr <= x"00000308";
                  data_byte_counter <= to_unsigned(3, bytes_left'length);
                  state <= WB_CYCLE;
                
                when others =>
                  if (i2c_rx_byte(7 downto 4) = CMD_GET_CH_REG_MASK) then
                    wb_adr(31 downto 12) <= (others => '0');
                    wb_adr(11 downto  8) <= x"2";
                    wb_adr( 7 downto  4) <= unsigned(i2c_rx_byte(3 downto 0));
                    wb_adr( 3 downto  0) <= (others => '0');
                    data_byte_counter <= to_unsigned(3, bytes_left'length);
                    state <= WB_CYCLE;
                  else
                    state <= UART_WRAPPER_STOP;
                  end if;

                end case;

          end if;
          
        -----------------------------------------------------------------------
        -- Transaction header received
        -----------------------------------------------------------------------
        when TRANSACTION_HEADER =>
          if (i2c_r_done_p = '1') then
          
            frame_byte_counter <= frame_byte_counter + 1;
            
            ----------------------
            -- FID + OPCODE field
            ----------------------
            if (frame_byte_counter = 0) then
              tid <= i2c_rx_byte(7);
              fid <= i2c_rx_byte(7);
              opcode <= i2c_rx_byte(6 downto 0);
              
            ------------
            -- DL field
            ------------
            else if (frame_byte_counter < 5) then
              data_len <= data_len(c_obc_dlc_width-9 downto 0) & i2c_rx_byte;
              
            ----------
            -- CRC !!!
            ----------
            
            --------------------------------------------------
            -- done shifting in header - store current OPCODE
            --------------------------------------------------
            else
              current_op <= opcode;
              state <= IDLE;
            
            end if;
          
          end if;

        
        -----------------------------------------------------------------------
        -- Prepare F_ACK and T_ACK frames
        -----------------------------------------------------------------------
        when SEND_F_ACK =>
          opcode <= OP_F_ACK;
          data_len <= (others => '0');
          if (i2c_addr_match_p = '1') and (i2c_op = '0') then
            i2c_rx_byte <= fid & opcode;
            fid_prev <= fid;
            state <= SEND_HEADER_FRAME;
          end if;
          
        -----------------------------------------------------------------------
        -- Sending and receiving header frames
        -----------------------------------------------------------------------
        when SEND_HEADER_FRAME =>
          if (i2c_w_done_p = '1') then
            frame_byte_count <= frame_byte_count + 1;
            ---------------------------
            -- TODO: Remove me for I2C
            ---------------------------
            tx_start_p <= '1';
            state <= UART_TX_START;
            ---------------------------
            if (frame_byte_count < 4) then
              i2c_tx_byte <= data_len(OBC_DL_WIDTH-1 downto OBC_DL_WIDTH-8);
              data_len <= data_len(OBC_DL_WIDTH-9 downto 0) & x"00";
            else
              state <= state_after;
            end if;
          end if;
          
        -----------------------------------------------------------------------
        -- Sending and receiving data frames
        -----------------------------------------------------------------------
        when RECEIVE_DATA_FRAME =>
          data_buf_write_p <= '0';
          if (i2c_r_done_p = '1') then
            frame_byte_counter <= frame_byte_counter + 1;
            
            -- Shift in FID+OPCODE
            if (frame_byte_counter = 0) then
              fid <= i2c_rx_byte(7);
              opcode <= i2c_rx_byte(6 downto 0);
              if (data_byte_counter > OBC_MTU)
                nr_data_bytes <= data_byte_counter;
              else
                nr_data_bytes <= OBC_MTU;
              end if;
              
            -- Shift in DATA bytes
            else if (frame_byte_counter < nr_data_bytes) then
              data_buf <= i2c_rx_byte;
              data_buf_write_p <= '1';
              data_buf_addr <= data_buf_addr + 1;
              data_byte_counter <= data_byte_counter - 1;
              
            -- Done shifting in bytes - start applying if correct frame
            else
              if (fid = not fid_prev) then
                state <= APPLY_DATA_FRAME;
              else
                state <= IDLE;
              end if;
            
            end if;
          end if;
        
        ---------------------------
        -- TODO: Remove me for I2C
        ---------------------------
        when UART_TX_START =>
          tx_start_p <= '0';
          state <= state_d0;
        ---------------------------
          
        when WB_CYCLE =>
          wb_cyc <= '1';
          wb_stb <= '1';
          wb_we  <= not i2c_op;
          if (wb_ack = '1') then
            wb_cyc <= '0';
            wb_stb <= '0';
            wb_we  <= '0';
            
            -- If last byte was sent, bytes_left wraps around to 0xf..ff
            -- in RX/TX state; go back to IDLE if so.
            --
            -- Otherwise, if we are to receive (WB to write), go back
            -- to RX state, incrementing the WB address.
            --
            -- Finally, if we are to send (WB to read), get the data
            -- and go back to TX state, incrementing the WB address.
            -- The TX state handles shifting of WB data.
            if (bytes_left = (bytes_left'range => '1')) then
              state <= UART_WRAPPER_STOP;
            elsif (wb_we = '1') then
              wb_adr <= wb_adr + 4;
              state <= RECEIVE_DATA;
            else
              wb_dat_in <= wbm_i.dat;
              wb_adr <= wb_adr + 4;
              -- TODO: Remove/change me for I2C
              state <= UART_TX_START;
            end if;
          end if;
          
          if (wb_err = '1') then
            wb_cyc <= '0';
            wb_stb <= '0';
            wb_we  <= '0';
            
            state <= UART_WRAPPER_STOP;
          end if;
          
        when UART_WRAPPER_STOP =>
          uart_wrapper_stop_p <= '1';
          state <= IDLE;
        
        when others =>
          state <= IDLE;
          
      end case;
    end if;
  end process p_wb_fsm;


end architecture behav;
--==============================================================================
--  architecture end
--==============================================================================
