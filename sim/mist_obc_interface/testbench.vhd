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

library altera_mf;
use altera_mf.all;

library work;
use work.genram_pkg.all;
use work.msp_pkg.all;



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
  
  type t_data_buf_ram is array(0 to c_msp_mtu-1) of std_logic_vector(7 downto 0);
  
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
  -- Clock and reset periods
  constant c_clk_per           : time := 20 ns;
  constant c_reset_per         : natural := 5; -- * c_clk_per
  
  -- UART baud divider
  constant c_baud_div_int         : natural := 5;
  constant c_baud_div             : std_logic_vector :=
      std_logic_vector(to_unsigned(c_baud_div_int, f_log2_size(c_baud_div_int)));
      
  constant c_inter_frame_delay    : time := 100 ns;

  -- I2C address of slave
  constant c_cubes_i2c_addr       : std_logic_vector(6 downto 0) := to7bits(x"70");

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
  component bemicro_cubes_btm is
    generic
    (
      g_nr_buttons      : natural := 1;
      
      -- Internal period in 50 MHz clock ticks; this is in addition to the period
      -- of any external reset the user employs:
      --
      -- reset_time = (20 ns * g_reset_period) + <external_reset_period>
      g_reset_period    : natural := 5000;
      
      -- UART baud divider ratio:
      --    g_baud_div = [f(clk_i) / f(baud)]-1
      --    Default: 115200 bps with 100 MHz clk_i
      g_baud_div        : natural := 867
    );
    port
    (
      clk_50meg_i : in  std_logic;
      btn_n_i     : in  std_logic_vector(g_nr_buttons-1 downto 0);

      led_n_o     : out std_logic_vector(7 downto 0);
      
      rxd_i       : in  std_logic;
      txd_o       : out std_logic;
      
      spi_cs_n_o        : out std_logic;
      spi_sclk_o        : out std_logic;
      spi_mosi_o        : out std_logic;
      spi_miso_i        : in  std_logic;
      
      siphra_sysclk_o   : out std_logic;
      siphra_txd_i      : in  std_logic
    );
  end component bemicro_cubes_btm;

  --============================================================================
  -- Signal declarations
  --============================================================================
  -- Clock, reset signals
  signal clk_50meg, rst_n          : std_logic;
  signal rst_count                 : natural;
  
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
  signal fid, rx_fid, tid           : std_logic;
  signal opcode                     : std_logic_vector( 6 downto 0);
  signal dl                         : std_logic_vector(31 downto 0);
  
  signal frame_byte_count           : natural;
  signal frame_data_bytes           : natural;
  signal trans_data_bytes           : natural;
  
  signal frame_end_p                : std_logic;
  
  signal header_buf                 : std_logic_vector(39 downto 0);
  signal data_buf                   : t_data_buf_ram;
  signal data_buf_addr              : natural;
  
  -- 1us counter between transactions
  signal delay_count                : natural;
  signal delay_count_done_p         : std_logic;
  signal first_run                  : boolean := true;
  
  -- DUT-specific signals
  signal led_n, led                 : std_logic_vector(7 downto 0);
    
  --============================================================================
  -- Alias declarations
  --============================================================================
  alias dut_plls_locked is <<signal .testbench.cmp_dut.plls_locked : std_logic>>;

--==============================================================================
--  architecture begin
--==============================================================================
begin

  --============================================================================
  -- Clock and reset signals
  --============================================================================
  p_clk : process
  begin
    clk_50meg <= '1';
    wait for c_clk_per/2;
    clk_50meg <= '0';
    wait for c_clk_per/2;
  end process p_clk;
  
  p_rst : process
  begin
    rst_n <= '0';
    while (rst_count < c_reset_per) loop
      wait until clk_50meg = '1';
      rst_count <= rst_count + 1;
    end loop;
    rst_n <= '1';
    wait;
  end process p_rst;

  --============================================================================
  -- Implement "I2C master" over UART
  --============================================================================
  -- Instantiate component
  cmp_uart : uart
    generic map
    (
      g_baud_div_bits => f_log2_size(c_baud_div_int)
    )
    port map
    (
      -- Clock, reset
      clk_i         => clk_50meg,
      rst_n_a_i     => rst_n,

      -- Ports to external world
      rxd_i         => master_rxd,
      txd_o         => master_txd,

      -- Ports to other logic
      baud_div_i    => c_baud_div,

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
  p_stim : process is
    
    ----------------------------------------------------------------------------
    -- Sub-procedures used within transaction
    ----------------------------------------------------------------------------
    procedure pulse(signal sig_p : out std_logic) is
    begin
      sig_p <= '1';
      wait until rising_edge(clk_50meg);
      sig_p <= '0';
    end procedure;
    ----------------------------------------------------------------------------
    
    ----------------------------------------------------------------------------
    procedure send_i2c_addr is
    begin
      frame_state <= I2C_ADDR;
      master_tx_data <= c_cubes_i2c_addr & '0';
      pulse(master_tx_start_p);
      wait until master_tx_ready = '1';
    end procedure;
    ----------------------------------------------------------------------------
    
    ----------------------------------------------------------------------------
    procedure send_header (
      signal opcode_in  : in std_logic_vector;
      signal fid_in     : in std_logic;
      signal dl_in      : in std_logic_vector
    ) is
    begin
      frame_state <= TX_HEADER_BYTES;
      
      header_buf(39 downto 32) <= fid_in & opcode_in;
      header_buf(31 downto  0) <= dl_in;
      wait until rising_edge(clk_50meg);
      
      while (frame_byte_count < c_msp_dl_num_bytes) loop
        master_tx_data <= header_buf(39 downto 32);
        pulse(master_tx_start_p);
        header_buf <= header_buf(31 downto 0) & x"00";
        wait until master_tx_ready = '1';
        frame_byte_count <= frame_byte_count + 1;
      end loop;
      
      frame_byte_count <= 0;
      frame_state <= WAITING;
    end procedure;
    ----------------------------------------------------------------------------
    
    ----------------------------------------------------------------------------
    procedure receive_header (
      signal opcode_out   : out std_logic_vector;
      signal fid_out      : out std_logic;
      signal dl_out       : out std_logic_vector
    ) is
    
      variable dl_int : std_logic_vector(31 downto 0) := (others => '0');
      
    begin
      frame_state <= RX_HEADER_BYTES;
      
      while (frame_byte_count < c_msp_dl_num_bytes) loop
        wait until master_rx_ready = '1';
        frame_byte_count <= frame_byte_count + 1;
        if (frame_byte_count = 0) then
          fid_out <= master_rx_data(7);
          opcode_out <= master_rx_data(6 downto 0);
          -- TODO: Wait one clock cycle and check FID + OPCODE byte?
        else
          dl_int := dl_int(23 downto 0) & master_rx_data;
        end if;
      end loop;
      
      dl_out <= dl_int;
      
      frame_byte_count <= 0;
      frame_state <= WAITING;
    end procedure;
    ----------------------------------------------------------------------------
    
    ----------------------------------------------------------------------------
    procedure send_data (
      signal fid_in                 : in  std_logic;
      signal frame_data_bytes_in    : in  natural
    ) is
    begin
      frame_state <= TX_DATA_BYTES;

      master_tx_data <= fid_in & c_msp_op_data_frame;
      pulse(master_tx_start_p);
      wait until master_tx_ready = '1';
      frame_byte_count <= 1;
      
      while (frame_byte_count < frame_data_bytes_in) loop
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
    ----------------------------------------------------------------------------
    
    ----------------------------------------------------------------------------
    procedure end_transaction is
    begin
      trans_state <= IDLE;
      data_buf_addr <= 0;
    end procedure;
    ----------------------------------------------------------------------------
    
    ----------------------------------------------------------------------------
    -- Procedure to send a transaction
    ----------------------------------------------------------------------------
    procedure run_transaction (
      cmd_in : in  std_logic_vector
    ) is
    begin
      opcode <= cmd_in;
      dl <= x"00000001";
      frame_data_bytes <= 1;
      trans_data_bytes <= 1;
      
      wait for c_inter_frame_delay;
      
      -- Send transaction header
      trans_state <= TRANS_HEADER;
      fid <= not fid;
      tid <= not fid;
      send_i2c_addr;
      send_header(opcode, fid, dl);
      pulse(frame_end_p);
      wait for c_inter_frame_delay;
      
      -- Receive F_ACK
      trans_state <= RX_F_ACK;
      send_i2c_addr;
      receive_header(opcode, rx_fid, dl);
      pulse(frame_end_p);
      wait for c_inter_frame_delay;
      
      -- Send DATA_FRAME
      trans_state <= TX_DATA_FRAME;
      fid <= not fid;
      send_i2c_addr;
      send_data(fid, frame_data_bytes);
      pulse(frame_end_p);
      wait for c_inter_frame_delay;
      
      -- Receive T_ACK
      trans_state <= RX_T_ACK;
      send_i2c_addr;
      receive_header(opcode, rx_fid, dl);
      pulse(frame_end_p);
      wait for c_inter_frame_delay;
      
      end_transaction;
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
    tid <= '0';
    fid <= '0';
    rx_fid <= '0';
    dl <= (others => '0');
    
    master_tx_data <= (others => '0');
    master_tx_start_p <= '0';
    header_buf <= (others => '0');
    data_buf <= (others => (others => '0'));
    data_buf_addr <= 0;
    
    frame_byte_count <= 0;
    trans_data_bytes <= 0;
    frame_data_bytes <= 0;
    frame_end_p <= '0';
    
    wait until rst_n = '1';
    
    wait until dut_plls_locked = '1';
    
    ----------------------------------------------------------------------------
    -- Transactions
    ----------------------------------------------------------------------------
    data_buf(0) <= x"ff";
    run_transaction(c_msp_op_set_leds);

    data_buf(0) <= x"12";
    run_transaction(c_msp_op_set_leds);

    ----------------------------------------------------------------------------
    -- End stimuli
    ----------------------------------------------------------------------------
    wait;
    
  end process p_stim;
  
  ------------------------------------------------------------------------------
  
  p_monitor : process is
  begin
    ERROR <= '0';
    wait until rst_n = '1';
    
    while true loop
      wait until frame_end_p = '1';
      
      case trans_state is
        when TRANS_HEADER =>
          null;
        when RX_F_ACK =>
          if (rx_fid /= fid) or (opcode /= c_msp_op_f_ack) then
            ERROR <= '1';
          end if;
        when RX_T_ACK =>
          if (rx_fid /= tid) or (opcode /= c_msp_op_t_ack) then
            ERROR <= '1';
          end if;
        when others =>
          null;
      end case;
    end loop;
    
    wait;
  end process p_monitor;
    
  --============================================================================
  -- DUT
  --============================================================================
  cmp_dut : bemicro_cubes_btm
    generic map
    (
      g_nr_buttons      => 1,
      
      -- Internal period in 50 MHz clock ticks; this is in addition to the period
      -- of any external reset the user employs:
      --
      -- reset_time = (20 ns * g_reset_period) + <external_reset_period>
      g_reset_period    => c_reset_per,
      
      -- UART baud divider ratio:
      --    g_baud_div = [f(clk_i) / f(baud)]-1
      --    Default: 115200 bps with 100 MHz clk_i
      g_baud_div        => 2*c_baud_div_int + 1
    )
    port map
    (
      clk_50meg_i       => clk_50meg,
      btn_n_i           => (others => '1'),

      led_n_o           => led_n,
      
      rxd_i             => master_txd,
      txd_o             => master_rxd,
      
      spi_cs_n_o        => open,
      spi_sclk_o        => open,
      spi_mosi_o        => open,
      spi_miso_i        => '0',
      
      siphra_sysclk_o   => open,
      siphra_txd_i      => '0'
    );
    
  led <= not led_n;

end architecture arch;
--==============================================================================
--  architecture end
--==============================================================================
