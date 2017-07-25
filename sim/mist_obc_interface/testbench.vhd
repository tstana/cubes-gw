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


entity testbench is
end entity testbench;


architecture arch of testbench is

  --============================================================================
  -- Type declarations
  --============================================================================
  type t_frame_state is (
    WAITING,
    
    I2C_ADDR,

    TX_HEADER_BYTES,
    RX_HEADER_BYTES,

    TX_DATA_BYTES,
    RX_DATA_BYTES
  );
  
  type t_trans_state is (
    IDLE,
    
    TRANS_HEADER,
    
    RX_F_ACK,
    TX_F_ACK,
    RX_T_ACK,
    TX_T_ACK,

    RX_HEADER_FRAME,
    TX_HEADER_FRAME,

    TX_DATA_FRAME,
    RX_DATA_FRAME
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
  constant RESET_PERIOD         : time := 45 us;
  
  constant BAUD_DIV_INT         : natural := 867;
  constant BAUD_DIV             : std_logic_vector :=
      std_logic_vector(to_unsigned(BAUD_DIV_INT, f_log2_size(BAUD_DIV_INT)));
      
  constant INTER_FRAME_DELAY    : time := 1 us;

  -- I2C address of slave
  constant CUBES_I2C_ADDR       : std_logic_vector(6 downto 0) := to7bits(x"70");

  -- MSP size defines
  constant OBC_MTU              : natural       := 507;
  constant OBC_DL_WIDTH         : natural       :=  32;
  constant OBC_DL_NR_BYTES      : natural       := f_log2_size(OBC_DL_WIDTH)-1;
  constant OBC_FCS_WIDTH        : natural       :=   0;
  constant OBC_FCS_NR_BYTES     : natural       :=   0;  -- f_log2_size(OBC_FCS_WIDTH);

  type t_data_buf_ram is array (0 to OBC_MTU-1) of std_logic_vector(7 downto 0);

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
  
  -- Number of peripherals in testbench
  constant c_num_periphs            : natural := 1;

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

      -- External module enable
      periph_sel_o        : out std_logic_vector(f_log2_size(g_num_periphs)-1 downto 0);
      periph_buf_data_i   : in  std_logic_vector(7 downto 0);
      periph_buf_data_o   : out std_logic_vector(7 downto 0);
      periph_buf_addr_i   : in  std_logic_vector(8 downto 0);   -- NB: Possibly needs constant!
      periph_buf_we_p_i   : in  std_logic;
      periph_data_rdy_p_o : out std_logic;

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
  
  -- Current state of master
  signal frame_state               : t_frame_state;
  signal trans_state               : t_trans_state;
  
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
  
  -- MSP specific signals
  signal fid, tid                   : std_logic;
  signal opcode                     : std_logic_vector( 6 downto 0);
  signal dl                         : std_logic_vector(31 downto 0);
  
  signal frame_byte_count           : natural;
  signal frame_data_bytes           : natural;
  signal trans_data_bytes           : natural;
  
  signal header_buf                 : std_logic_vector(39 downto 0);
  signal data_buf                   : t_data_buf_ram;
  signal data_buf_addr              : natural;
  
  -- Other signals to/from the DUT
  signal periph_sel                 : std_logic_vector(f_log2_size(c_num_periphs)-1 downto 0);
  signal obc_data_rdy_p             : std_logic;
  signal data_from_obc              : std_logic_vector(7 downto 0);
  
  -- 1us counter between transactions
  signal delay_count                : natural;
  signal delay_count_done_p         : std_logic;
  signal first_run                  : boolean := true;
  
  -- DUT-specific signals (mimicking logic in synthesizable gateware)
  signal led                        : std_logic_vector(7 downto 0);
    
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
    
  --============================================================================
  -- Stimuli and monitor processes
  --============================================================================
  P_STIM : process is
    
    procedure pulse(signal sig_p : out std_logic) is
    begin
      sig_p <= '1';
      wait until rising_edge(clk_100meg);
      sig_p <= '0';
    end procedure;
    
    procedure send_i2c_addr is
    begin
      frame_state <= I2C_ADDR;
      master_tx_data <= CUBES_I2C_ADDR & '0';
      pulse(master_tx_start_p);
      wait until master_tx_ready = '1';
    end procedure;
    
    procedure send_header(
      opcode  : in std_logic_vector;
      fid     : in std_logic;
      dl      : in std_logic_vector
    ) is
    begin
      frame_state <= TX_HEADER_BYTES;
      
      header_buf(39 downto 32) <= fid & opcode;
      header_buf(31 downto  0) <= dl;
      wait until rising_edge(clk_100meg);
      
      while (frame_byte_count < OBC_DL_NR_BYTES) loop
        master_tx_data <= header_buf(39 downto 32);
        pulse(master_tx_start_p);
        header_buf <= header_buf(31 downto 0) & x"00";
        wait until master_tx_ready = '1';
        frame_byte_count <= frame_byte_count + 1;
      end loop;
      
      frame_byte_count <= 0;
      frame_state <= WAITING;
    end procedure;
    
    procedure receive_header is
    begin
      frame_state <= RX_HEADER_BYTES;
      
      while (frame_byte_count < OBC_DL_NR_BYTES) loop
        wait until master_rx_ready = '1';
        frame_byte_count <= frame_byte_count + 1;
        if (frame_byte_count = 0) then
          fid <= master_rx_data(7);
          opcode <= master_rx_data(6 downto 0);
          -- TODO: Wait one clock cycle and check FID + OPCODE byte?
        else
          dl <= dl(23 downto 0) & master_rx_data;
        end if;
      end loop;
      
      frame_byte_count <= 0;
      frame_state <= WAITING;
    end procedure;
    
    procedure send_data (
      signal fid                : in  std_logic;
      signal frame_data_bytes   : in  natural
    ) is
    begin
      frame_state <= TX_DATA_BYTES;

      master_tx_data <= fid & OP_DATA_FRAME;
      pulse(master_tx_start_p);
      wait until master_tx_ready = '1';
      frame_byte_count <= 1;
      
      while (frame_byte_count < frame_data_bytes) loop
        master_tx_data <= data_buf(data_buf_addr);
        pulse(master_tx_start_p);
        data_buf_addr <= data_buf_addr + 1;
        trans_data_bytes <= trans_data_bytes - 1;
        wait until master_tx_ready = '1';
        frame_byte_count <= frame_byte_count + 1;
      end loop;
      
      frame_state <= WAITING;
      frame_byte_count <= 0;
    end procedure;
    
    procedure end_transaction is
    begin
      trans_state <= IDLE;
      data_buf_addr <= 0;
    end procedure;

  ------------------------------------------------------------------------------
  -- Stimuli process start
  ------------------------------------------------------------------------------
  begin
  
    ----------------------------------------------------------------------------
    -- Reset
    ----------------------------------------------------------------------------
    trans_state <= IDLE;
    frame_state <= WAITING;
    
    opcode <= (others => '0');
    fid <= '0';
    dl <= (others => '0');
    
    master_tx_data <= (others => '0');
    master_tx_start_p <= '0';
    header_buf <= (others => '0');
    data_buf <= (others => (others => '0'));
    data_buf_addr <= 0;
    
    frame_byte_count <= 0;
    trans_data_bytes <= 0;
    frame_data_bytes <= 0;
    
    wait until rst_n = '1';
    
    ----------------------------------------------------------------------------
    -- Prepare SET_LEDS command
    ----------------------------------------------------------------------------
    opcode <= OP_SET_LEDS;
    fid <= '0';
    dl <= x"00000001";
    frame_data_bytes <= 1;
    trans_data_bytes <= 1;
    data_buf(0) <= x"ff";
    
    wait for INTER_FRAME_DELAY;
    
    -- Send transaction header
    trans_state <= TRANS_HEADER;
    send_i2c_addr;
    send_header(opcode, fid, dl);
    fid <= not fid;
    wait for INTER_FRAME_DELAY;
    
    -- Receive F_ACK
    trans_state <= RX_F_ACK;
    send_i2c_addr;
    receive_header;
    wait for INTER_FRAME_DELAY;
    
    -- Send DATA_FRAME
    trans_state <= TX_DATA_FRAME;
    send_i2c_addr;
    send_data(fid, frame_data_bytes);
    wait for INTER_FRAME_DELAY;
    
    -- Receive T_ACK
    trans_state <= RX_T_ACK;
    send_i2c_addr;
    receive_header;
    wait for INTER_FRAME_DELAY;
    
    end_transaction;
    ----------------------------------------------------------------------------

     ----------------------------------------------------------------------------
    -- Prepare SET_LEDS command
    ----------------------------------------------------------------------------
    opcode <= OP_SET_LEDS;
    fid <= '0';
    dl <= x"00000001";
    frame_data_bytes <= 1;
    trans_data_bytes <= 1;
    data_buf(0) <= x"12";
    
    wait for INTER_FRAME_DELAY;
    
    -- Send transaction header
    trans_state <= TRANS_HEADER;
    send_i2c_addr;
    send_header(opcode, fid, dl);
    fid <= not fid;
    wait for INTER_FRAME_DELAY;
    
    -- Receive F_ACK
    trans_state <= RX_F_ACK;
    send_i2c_addr;
    receive_header;
    wait for INTER_FRAME_DELAY;
    
    -- Send DATA_FRAME
    trans_state <= TX_DATA_FRAME;
    send_i2c_addr;
    send_data(fid, frame_data_bytes);
    wait for INTER_FRAME_DELAY;
    
    -- Receive T_ACK
    trans_state <= RX_T_ACK;
    send_i2c_addr;
    receive_header;
    wait for INTER_FRAME_DELAY;
    
    end_transaction;
    ----------------------------------------------------------------------------

   wait;
  end process;
  ------------------------------------------------------------------------------
  
  --============================================================================
  -- DUT
  --============================================================================
  U_DUT : mist_obc_interface
    generic map
    (
      g_num_periphs => c_num_periphs
    )
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

      -- External module enable
      periph_sel_o        => periph_sel,
      periph_buf_data_i   => (others => '0'),
      periph_buf_data_o   => data_from_obc,
      periph_buf_addr_i   => (others => '0'),
      periph_buf_we_p_i   => '0',
      periph_data_rdy_p_o => obc_data_rdy_p,

      -- TEMPORARY: UART RX and TX
      rxd_i       => master_txd,
      txd_o       => master_rxd
    );
    
  -- Debug LEDs
  P_DEBUG_LEDS : process (clk_100meg, rst_n) is
  begin
    if (rst_n = '0') then
      led <= (others => '0');
    elsif rising_edge(clk_100meg) then
      if (periph_sel = "1") and (obc_data_rdy_p = '1') then
        led <= data_from_obc;
      end if;
    end if;
  end process;

end architecture arch;
--==============================================================================
--  architecture end
--==============================================================================
