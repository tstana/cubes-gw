--==============================================================================
-- Testbench for MIST OBC interface module
--==============================================================================
--
-- author: Theodor Stana (theodor.stana@gmail.com)
--
-- date of creation: 2017-07-17
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
--    2017-07-17   Theodor Stana     File created
--==============================================================================
-- TODO: -
--==============================================================================

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.genram_pkg.all;
use work.wishbone_pkg.all;


entity testbench is
end entity testbench;


architecture arch of testbench is

  --============================================================================
  -- Type declarations
  --============================================================================
  type t_master_state is (
    IDLE,
    
    I2C_ADDR_BYTE,

    TRANSACTION_HEADER,

    SEND_F_ACK,
    SEND_T_ACK,
    SEND_HEADER_FRAME,
    PREP_NEXT_HEADER_BYTE,
    
    RECEIVE_F_ACK,
    RECEIVE_T_ACK,
    
    RECEIVE_DATA_FRAME,
    DATA_FRAME_RX,
    SEND_DATA_FRAME,
    DATA_FRAME_TX,
    
    APPLY_DATA_FRAME
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
  constant CLK_PERIOD           : time := 10 ns;
  constant RESET_PERIOD         : time := 45 ns;
  
  constant BAUD_DIV_INT         : natural := 867;
  constant BAUD_DIV             : std_logic_vector :=
      std_logic_vector(to_unsigned(BAUD_DIV_INT, f_log2_size(BAUD_DIV_INT)));
      
  constant INTER_FRAME_DELAY    : natural := 999;

  -- I2C address of slave
  constant CUBES_I2C_ADDR       : std_logic_vector(6 downto 0) := to7bits(x"70");

  -- MSP size defines
  constant OBC_MTU              : natural       := 507;
  constant OBC_DL_WIDTH         : natural       :=  32;
  constant OBC_DL_NR_BYTES      : natural       := f_log2_size(OBC_DL_WIDTH);
  constant OBC_FCS_WIDTH        : natural       :=   0;
  constant OBC_FCS_NR_BYTES     : natural       :=   0;  -- f_log2_size(OBC_FCS_WIDTH);

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
  ---------------------------------
  -- UART component for the master
  ---------------------------------
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

  -------
  -- DUT
  -------
  component mist_obc_interface is
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
  end component mist_obc_interface;

  --============================================================================
  -- Signal declarations
  --============================================================================
  -- Clock, reset signals
  signal clk_100meg, rst_n          : std_logic;
  
  -- Error signal
  signal ERROR                      : std_logic;
  
  -- Master-side UART signals
  signal master_txd, master_rxd     : std_logic;
  signal master_tx_data             : std_logic_vector(7 downto 0);
  signal master_rx_data             : std_logic_vector(7 downto 0);
  signal master_tx_start_p          : std_logic;
  signal master_tx_ready            : std_logic;
  signal master_tx_ready_d0         : std_logic;
  signal master_tx_ready_p          : std_logic;
  signal master_rx_ready            : std_logic;
  signal master_rx_ready_d0         : std_logic;
  signal master_rx_ready_p          : std_logic;
  
  -- MSP signals
  signal master_state               : t_master_state;
  signal trans_state                : t_master_state;
  signal trans_active               : std_logic;
  
  signal opcode, opcode_ext         : std_logic_vector( 6 downto 0);
  signal fid, fid_prev, fid_ext, tid: std_logic;
  signal dl, dl_ext                 : std_logic_vector(31 downto 0);
  
  signal frame_byte_count           : unsigned(31 downto 0);
  signal data_byte_count            : unsigned(31 downto 0);
  signal nr_data_bytes              : unsigned(31 downto 0);
  
  signal header_buf                 : std_logic_vector(39 downto 0);
  signal data_buf                   : std_logic_vector(39 downto 0);
  
  -- 1us counter between transactions
  signal delay_count                : natural;
  signal delay_count_done_p         : std_logic;
  signal first_run                  : boolean := true;
  
  -- Stimuli signals
  signal leds_setting               : std_logic_vector(7 downto 0);
  
--==============================================================================
--  architecture begin
--==============================================================================
begin

  --============================================================================
  -- Clock and reset signals
  --============================================================================
  P_CLK : process
  begin
    clk_100meg <= '1';
    wait for CLK_PERIOD/2;
    clk_100meg <= '0';
    wait for CLK_PERIOD/2;
  end process P_CLK;
  
  P_RST : process
  begin
    rst_n <= '0';
    wait for RESET_PERIOD;
    rst_n <= '1';
    wait;
  end process P_RST;

  --============================================================================
  -- Implement "I2C master" over UART
  --============================================================================
  -- Instantiate component
  U_UART : uart
    generic map
    (
      g_baud_div_bits => f_log2_size(BAUD_DIV_INT)
    )
    port map
    (
      -- Clock, reset
      clk_i         => clk_100meg,
      rst_n_a_i     => rst_n,

      -- Ports to external world
      rxd_i         => master_rxd,
      txd_o         => master_txd,

      -- Ports to other logic
      baud_div_i    => BAUD_DIV,

      tx_data_i     => master_tx_data,
      tx_start_p_i  => master_tx_start_p,
      tx_ready_o    => master_tx_ready,

      rx_ready_o    => master_rx_ready,
      rx_data_o     => master_rx_data,

      frame_err_o   => open
    );
    
  -- MSP master FSM
  P_FSM : process (clk_100meg, rst_n)
  begin
    if (rst_n = '0') then
      ERROR <= '0';
    
      master_state <= IDLE;
      trans_state <= IDLE;
      trans_active <= '0';
      
      delay_count <= 0;
      delay_count_done_p <= '0';
      
      frame_byte_count <= (others => '0');
      header_buf <= (others => '0');
      data_buf <= (others => '0');
      data_byte_count <= (others => '0');
      nr_data_bytes <= (others => '0');
      
      master_tx_data <= (others => '0');
      master_tx_start_p <= '0';
      master_tx_ready_d0 <= '0';
      master_tx_ready_p <= '0';
      master_rx_ready_d0 <= '0';
      master_rx_ready_p <= '0';
      
      opcode <= (others => '0');
      fid <= '0';
      fid_prev <= '0';
      tid <= '0';
      dl <= (others => '0');
      
    elsif rising_edge(clk_100meg) then
    
      delay_count_done_p <= '0';
      
      master_tx_start_p <= '0';
      
      master_tx_ready_d0 <= master_tx_ready;
      master_tx_ready_p  <= master_tx_ready and (not master_tx_ready_d0);
      master_rx_ready_d0 <= master_rx_ready;
      master_rx_ready_p  <= master_rx_ready and (not master_rx_ready_d0);
      
      case master_state is
        when IDLE =>
          master_tx_start_p <= '0';
          frame_byte_count <= (others => '0');

          delay_count <= delay_count + 1;
          if (delay_count = INTER_FRAME_DELAY) then
            delay_count_done_p <= '1';
            delay_count <= 0;
            
            master_tx_data <= CUBES_I2C_ADDR & '0';
            master_tx_start_p <= '1';
            master_state <= I2C_ADDR_BYTE;
          end if;
          
        when I2C_ADDR_BYTE =>
          if (trans_state = SEND_DATA_FRAME) then
            data_buf(39 downto 32) <= (not fid_prev) & OP_DATA_FRAME;
            data_buf(31 downto  8) <= (others => '0');
            data_buf( 7 downto  0) <= leds_setting;    -- TODO: Change me!!!              
          end if;

          if (master_tx_ready_p = '1') then
            master_state <= trans_state;
            
            if (trans_active = '0') then
              master_state <= TRANSACTION_HEADER;   -- NB: Hack!
              trans_state <= TRANSACTION_HEADER;
            end if;
          end if;
          
        ------------------------------------------------------------------------
        -- Dedicated frame states
        ------------------------------------------------------------------------
        when TRANSACTION_HEADER =>
          header_buf(39 downto 32) <= fid_ext & opcode_ext;
          header_buf(31 downto  0) <= dl_ext;
          opcode <= opcode_ext;
          fid <= fid_ext;
          tid <= fid_ext;
          fid_prev <= fid_ext;
          dl <= dl_ext;
          case opcode_ext is
            when OP_SET_LEDS =>
              data_byte_count <= unsigned(dl_ext);
            when others =>
              null;
          end case;
          master_state <= PREP_NEXT_HEADER_BYTE;
          trans_active <= '1';
          
        when RECEIVE_F_ACK =>
          if (master_rx_ready_p = '1') then
            frame_byte_count <= frame_byte_count + 1;
            if (frame_byte_count = 0) then
              fid <= master_rx_data(7);
              opcode <= master_rx_data(6 downto 0);
            elsif (frame_byte_count < 4) then
              dl <= dl(23 downto 0) & master_rx_data;
            else
              if (opcode = OP_F_ACK) and (fid = fid_prev) then
                master_state <= IDLE;
                trans_state <= SEND_DATA_FRAME;
              else
                ERROR <= '1';
              end if;
            end if;
          end if;

        when RECEIVE_T_ACK =>
          if (master_rx_ready_p = '1') then
            frame_byte_count <= frame_byte_count + 1;
            if (frame_byte_count = 0) then
              fid <= master_rx_data(7);
              opcode <= master_rx_data(6 downto 0);
            elsif (frame_byte_count < 4) then
              dl <= dl(23 downto 0) & master_rx_data;
            else
              if (opcode = OP_T_ACK) and (fid = tid) then
                master_state <= IDLE;
                data_byte_count <= unsigned(dl);
                trans_state <= IDLE;
                trans_active <= '0';
              else
                ERROR <= '1';
              end if;
            end if;
          end if;

        ------------------------------------------------------------------------
        -- Common states shared between frames
        ------------------------------------------------------------------------
        when SEND_HEADER_FRAME =>
          if (master_tx_ready_p = '1') then
            header_buf <= header_buf(31 downto 0) & x"00";
            frame_byte_count <= frame_byte_count + 1;
            master_state <= PREP_NEXT_HEADER_BYTE;
            if (frame_byte_count = 4) then
              master_state <= IDLE;
              case opcode is
                when OP_SET_LEDS =>
                  trans_state <= RECEIVE_F_ACK;
                when others =>
                  trans_state <= IDLE;
              end case;
            end if;
          end if;
        
        when PREP_NEXT_HEADER_BYTE =>
          master_tx_data <= header_buf(39 downto 32);
          master_tx_start_p <= '1';
          master_state <= SEND_HEADER_FRAME;
        
        when SEND_DATA_FRAME =>
          nr_data_bytes <= to_unsigned(4, nr_data_bytes'length);
          master_state <= DATA_FRAME_TX;
          master_tx_data <= data_buf(39 downto 32);
          master_tx_start_p <= '1';
          data_buf <= data_buf(31 downto 0) & x"00";

        when DATA_FRAME_TX =>
          if (master_tx_ready_p = '1') then
            frame_byte_count <= frame_byte_count + 1;
            data_buf <= data_buf(31 downto 0) & x"00";
            master_tx_data <= data_buf(39 downto 32);
            master_tx_start_p <= '1';
            data_byte_count <= data_byte_count - 1;
            if (frame_byte_count > 0) then
              if (frame_byte_count = nr_data_bytes) then
                trans_state <= RECEIVE_T_ACK;
                master_state <= IDLE;
                master_tx_start_p <= '0';   -- HACK: Avoid redundant byte...
              end if;
            end if;
          end if;
          
        ------------------------------------------------------------------------
        -- Catch-all
        ------------------------------------------------------------------------
        when others =>
          master_state <= IDLE;
          
      end case;
      
    end if;
  end process P_FSM;

  --============================================================================
  -- Stimuli
  --============================================================================
  P_STIM : process
  begin
    wait until rst_n = '0';
    opcode_ext <= OP_SET_LEDS;
    fid_ext <= '0';
    dl_ext <= x"00000004";
    leds_setting <= x"ff";
    wait;
  end process P_STIM;

  --============================================================================
  -- DUT
  --============================================================================
  U_DUT : mist_obc_interface
    port map
    (
    -- Clock, active-low reset
    clk_i       => clk_100meg,
    rst_n_a_i   => rst_n,
    
    -- I2C lines
    scl_i       => '0',
    scl_o       => open,
    scl_en_o    => open,
    sda_i       => '0',
    sda_o       => open,
    sda_en_o    => open,

    -- I2C address
    i2c_addr_i  => CUBES_I2C_ADDR,

    -- Unused
    tip_o       => open,
    err_p_o     => open,
    wdto_p_o    => open,

    -- TEMPORARY: UART RX and TX
    rxd_i       => master_txd,
    txd_o       => master_rxd
    );
    
end architecture arch;
--==============================================================================
--  architecture end
--==============================================================================
