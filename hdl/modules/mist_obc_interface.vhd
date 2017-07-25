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

use work.genram_pkg.all;
use work.msp_pkg.all;

entity mist_obc_interface is
  generic
  (
    g_num_periphs : natural
  );
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
    
    -- Peripheral module signals
    periph_sel_o        : out std_logic_vector(f_log2_size(g_num_periphs)-1 downto 0);
    periph_buf_data_i   : in  std_logic_vector(7 downto 0);
    periph_buf_data_o   : out std_logic_vector(7 downto 0);
    periph_buf_addr_i   : in  std_logic_vector(f_log2_size(c_msp_mtu)-1 downto 0);
    periph_buf_we_p_i   : in  std_logic;
    periph_data_rdy_p_o : out std_logic;

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

    RX_DATA_BYTES,
    TX_DATA_BYTES
  );

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
  signal rx_data_len            : std_logic_vector(c_msp_dl_width-1 downto 0);
  signal tx_data_len            : std_logic_vector(c_msp_dl_width-1 downto 0);
  
  -- Implementation-specific signals
  signal frame_rxed_p           : std_logic;
  signal frame_txed_p           : std_logic;
  signal frame_byte_count       : unsigned(f_log2_size(c_msp_mtu)-1 downto 0);
  signal frame_data_bytes       : unsigned(c_msp_dl_width-1 downto 0);
  signal trans_data_bytes       : unsigned(c_msp_dl_width-1 downto 0);
  signal trans_done_p           : std_logic;
  
  signal buf_data_in            : std_logic_vector(7 downto 0);
  signal buf_data_out           : std_logic_vector(7 downto 0);
  signal buf_addr               : unsigned(f_log2_size(c_msp_mtu)-1 downto 0);
  signal buf_we_p               : std_logic;

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
      trans_done_p <= '0';
      periph_sel_o <= (others => '0');
      periph_data_rdy_p_o <= '0';
      
    elsif rising_edge(clk_i) then
      
      trans_done_p <= '0';
      
      periph_data_rdy_p_o <= '0';
      
      case trans_state is
        when IDLE =>
          if (i2c_addr_match_p = '1') then
            periph_sel_o <= (others => '0');
            trans_state <= TRANS_HEADER;
          end if;
          
        when TRANS_HEADER =>
          if (frame_rxed_p = '1') then
            case rx_opcode is
              when c_msp_op_set_leds =>
                trans_state <= TX_F_ACK;
                periph_sel_o <= "1";
              when others =>
                trans_state <= TX_T_ACK;
            end case;
          end if;
          
        when RX_F_ACK =>
          if (frame_rxed_p = '1') then
            if (trans_data_bytes /= 0) then
              trans_state <= TX_DATA_FRAME;
            end if;
          end if;
          
        when TX_F_ACK =>
          if (frame_txed_p = '1') then
            if (trans_data_bytes /= 0) then
              trans_state <= RX_DATA_FRAME;
            end if;
          end if;
          
        when RX_DATA_FRAME =>
          if (frame_rxed_p = '1') then
            periph_data_rdy_p_o <= '1';
            if (trans_data_bytes = 0) then
              trans_state <= TX_T_ACK;
            else
              trans_state <= TX_F_ACK;
            end if;
          end if;

        when TX_DATA_FRAME =>
          if (frame_txed_p = '1') then
            if (trans_data_bytes = 0) then
              trans_state <= RX_T_ACK;
            else
              trans_state <= RX_F_ACK;
            end if;
          end if;
          
        when RX_T_ACK =>
          if (frame_rxed_p = '1') then
            trans_done_p <= '1';
            trans_state <= IDLE;
          end if;
          
        when TX_T_ACK =>
          if (frame_txed_p = '1') then
            trans_done_p <= '1';
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
  -- Process for the FSM
  P_FRAME_FSM : process (clk_i, rst_n_a_i)
  begin
    if (rst_n_a_i = '0') then
      frame_state <= WAIT_I2C_ADDR;
      
      frame_rxed_p <= '0';
      frame_txed_p <= '0';
      
      uart_wrapper_stop_p <= '0';
      tx_start_p <= '0';
      i2c_tx_byte <= (others => '0');
      
      frame_byte_count <= (others => '0');
      frame_data_bytes <= (others => '0');
      trans_data_bytes <= (others => '0');
      
      rx_fid <= '0';
      tid <= '0';
      fid_prev <= '0';
      rx_opcode <= (others => '0');
      rx_data_len <= (others => '0');
      
      buf_data_in <= (others => '0');
      buf_addr <= (others => '0');
      buf_we_p <= '0';
      
    elsif rising_edge(clk_i) then
   
      frame_rxed_p <= '0';
      frame_txed_p <= '0';
      
      uart_wrapper_stop_p <= '0';
      tx_start_p <= '0';
      
      buf_we_p <= '0';
      
      -- Buffer address increment after write; new transactions should write
      -- starting from buffer address 0.
      if (buf_we_p = '1') then
        buf_addr <= buf_addr + 1;
      elsif (trans_done_p = '1') then
        buf_addr <= (others => '0');
      end if;
      
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
                tx_data_len <= (others => '0');
                i2c_tx_byte <= fid_prev & c_msp_op_f_ack;
                tx_start_p <= '1';
              when TX_T_ACK =>
                frame_state <= TX_HEADER_BYTES;
                tx_data_len <= (others => '0');
                i2c_tx_byte <= tid & c_msp_op_t_ack;
                tx_start_p <= '1';
              when RX_DATA_FRAME =>
                if (trans_data_bytes >= c_msp_mtu) then
                  frame_data_bytes <= to_unsigned(c_msp_mtu, frame_data_bytes'length);
                else
                  frame_data_bytes <= trans_data_bytes;
                end if;
                frame_state <= RX_DATA_BYTES;
              when others =>
                null;
            end case;
          end if;
          
        when RX_HEADER_BYTES =>
          -- shift in bytes
          if (frame_byte_count < 5) then
            if (i2c_r_done_p = '1') then
              frame_byte_count <= frame_byte_count + 1;
              -- FID+OPCODE byte
              if (frame_byte_count = 0) then
                rx_fid <= i2c_rx_byte(7);
                if (trans_state = TRANS_HEADER) then
                  tid <= i2c_rx_byte(7);
                end if;
                rx_opcode <= i2c_rx_byte(6 downto 0);
              -- DL field bytes
              else
                rx_data_len <= rx_data_len(c_msp_dl_width-9 downto 0) & i2c_rx_byte;
              end if;
            end if;
              
          -- done shifting; apply received fields, signal transaction FSM and
          -- go back to waiting
          else
            trans_data_bytes <= unsigned(rx_data_len);
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
          
        when TX_HEADER_BYTES =>
          -- shift out bytes
          if (frame_byte_count < 5) then
            if (i2c_w_done_p = '1') then
              frame_byte_count <= frame_byte_count + 1;
              
              -- FID+OPCODE byte
              if (frame_byte_count = 0) then
                tx_fid <= i2c_tx_byte(7);
                tx_opcode <= i2c_tx_byte(6 downto 0);
              end if;
              
              -- DL bytes
              if (frame_byte_count < 4) then
                tx_data_len <= tx_data_len(c_msp_dl_width-9 downto 0) & x"00";
                i2c_tx_byte <= tx_data_len(c_msp_dl_width-1 downto c_msp_dl_width-8);
                tx_start_p <= '1';
                
              -- FCS bytes
              -- else
              
              end if;
              
            end if;
            
          -- done shifting; signal transaction FSM and go back to waiting
          else
            -- trans_data_bytes <= unsigned(tx_data_len);
            -- TODO: Check here for correct FID and signal error otherwise.
            fid_prev <= tx_fid;

            frame_txed_p <= '1';
            
            ------------------------
            -- TODO: Remove for I2C
            ------------------------
            uart_wrapper_stop_p <= '1';
            ------------------------
            frame_state <= WAIT_I2C_ADDR;
          end if;
          
        when RX_DATA_BYTES =>
          if (frame_byte_count < 1 + frame_data_bytes) then
            if (i2c_r_done_p = '1') then
              frame_byte_count <= frame_byte_count + 1;
              
              -- FID+OPCODE byte
              if (frame_byte_count = 0) then
                rx_fid <= i2c_rx_byte(7);
                rx_opcode <= i2c_rx_byte(6 downto 0);
              -- DATA bytes
              else
                buf_data_in <= i2c_rx_byte;
                buf_we_p <= '1';
                trans_data_bytes <= trans_data_bytes - 1;
              -- FCS bytes here
              end if;
            end if;
          else
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

        when others =>
          frame_state <= WAIT_I2C_ADDR;   -- TODO: Different state on error?
          
      end case;
      
    end if;
  end process;
  
  --============================================================================
  -- Dual-port RAM for the data buffer
  --============================================================================
  U_BUFFER_RAM : generic_dpram
    generic map
    (
      g_data_width                => 8,
      g_size                      => c_msp_mtu,
      g_addr_conflict_resolution  => "read_first",
      g_dual_clock                => false
    )
    port map
    (
      rst_n_i => rst_n_a_i,

      -- Port A
      clka_i => clk_i,
      wea_i  => buf_we_p,
      aa_i   => std_logic_vector(buf_addr),
      da_i   => buf_data_in,
      qa_o   => buf_data_out,
      
      -- Port B
      clkb_i => '0',
      web_i  => periph_buf_we_p_i,
      ab_i   => periph_buf_addr_i,
      db_i   => periph_buf_data_i,
      qb_o   => periph_buf_data_o
    );

end architecture behav;
--==============================================================================
--  architecture end
--==============================================================================
