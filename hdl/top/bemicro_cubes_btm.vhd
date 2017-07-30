--==============================================================================
-- CUBES Bench Test Model implementation for the MAX10 BeMicro kit
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


entity bemicro_cubes_btm is
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
    --    Default: 115200 bps with 100 MHz clock at UART module port "clk_i"
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
end entity bemicro_cubes_btm;


architecture arch of bemicro_cubes_btm is

  --============================================================================
  -- Constant declarations
  --============================================================================
  ------------------------------------------------------------------------------
  -- Gateware version number
  ------------------------------------------------------------------------------
  -- Hex-value minor and major version
  constant c_gw_vers_maj : std_logic_vector( 7 downto 0) := x"30";
  constant c_gw_vers_min : std_logic_vector( 7 downto 0) := x"31";
  
  -- Concatenate the two together
  constant c_gw_version : std_logic_vector(15 downto 0) := c_gw_vers_maj & c_gw_vers_min;
  
  ------------------------------------------------------------------------------
  -- I2C address
  ------------------------------------------------------------------------------
  constant c_i2c_address : std_logic_vector(6 downto 0) := "1110000";
  
  ------------------------------------------------------------------------------
  -- Peripherals to OBC interface
  ------------------------------------------------------------------------------
  constant c_num_obc_periphs  : natural := 2;
  
  constant c_periph_hk_regs   : natural := 0;
  constant c_periph_leds      : natural := 1;

  --============================================================================
  -- Component declarations
  --============================================================================
  -- MAX10 PLLs
  component plls is
    port
    (
      rst_a_i           : in  std_logic;
      clk_50meg_i       : in  std_logic;

      clk_adc_o         : out std_logic;
      clk_100meg_o      : out std_logic;

      main_pll_locked_o : out std_logic
    );
  end component plls;

  -- Button debouncer
  component debouncer is
    generic
    (
      g_nr_buttons      : natural := 4;
      g_debounce_cycles : natural := 10_000_000
    );
    port
    (
      clk_i     : in  std_logic;
      btn_n_i   : in  std_logic_vector(g_nr_buttons-1 downto 0);
      btn_o     : out std_logic_vector(g_nr_buttons-1 downto 0)
    );
  end component debouncer;
  
  -- I2C slave to Wishbone master following MIST OBC protocol
  component mist_obc_interface is
    generic
    (
      -- Number of peripherals to OBC interface component
      g_num_periphs : natural;

      -- Baud divider ratio:
      --    g_baud_div = [f(clk_i) / f(baud)]-1
      --    Default: 115200 bps with 100 MHz clk_i
      g_baud_div : natural := 867
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
      periph_sel_o              : out std_logic_vector(f_log2_size(g_num_periphs)-1 downto 0);
      
      periph_num_data_bytes_i   : in  std_logic_vector(c_msp_dl_width-1 downto 0);
      periph_buf_we_i           : in  std_logic;
      periph_buf_addr_i         : in  std_logic_vector(f_log2_size(c_msp_mtu)-1 downto 0);
      periph_buf_data_i         : in  std_logic_vector(7 downto 0);
      periph_data_rdy_p_i       : in  std_logic;
      
      periph_buf_data_o         : out std_logic_vector(7 downto 0);
      periph_data_rdy_p_o       : out std_logic;

      -- TEMPORARY: UART RX and TX
      rxd_i       : in  std_logic;
      txd_o       : out std_logic
    );
  end component mist_obc_interface;

  -- Housekeeping registers
  component hk_regs is
    port
    (
      clk_i               : in  std_logic;
      rst_n_a_i           : in  std_logic;
      
      -- Number of bytes available on data request
      num_bytes_o         : out std_logic_vector(c_msp_dl_width-1 downto 0);
      
      -- Interface to MSP data buffer
      data_ld_p_i         : in  std_logic;
      we_o                : out std_logic;
      addr_o              : out std_logic_vector(f_log2_size(c_msp_mtu)-1 downto 0);
      data_o              : out std_logic_vector(7 downto 0);
      data_rdy_p_o        : out std_logic;
      
      -- Interface to modules providing housekeeping
      gw_vers_i           : in  std_logic_vector(15 downto 0);
      leds_i              : in  std_logic_vector( 7 downto 0)
    );
  end component hk_regs;

  --============================================================================
  -- Signal declarations
  --============================================================================
  signal clk_100meg             : std_logic;
  signal clk_adc                : std_logic;

  signal rst                    : std_logic := '1';
  signal rst_count              : unsigned(f_log2_size(g_reset_period)-1 downto 0) := (others => '0');
  signal rst_ext                : std_logic;
  signal rst_n                  : std_logic;
  
  signal plls_locked            : std_logic;
  signal plls_locked_d0         : std_logic;
  signal plls_unlocked_p        : std_logic;
  
  signal btn                    : std_logic_vector(g_nr_buttons-1 downto 0);

  signal led                    : std_logic_vector( 7 downto 0);
  signal led_sequenced          : std_logic;
  signal led_sequence_div       : unsigned(20 downto 0);
  signal led_sequence_count     : unsigned( 3 downto 0);
  
  signal obc_periph_sel         : std_logic_vector(f_log2_size(c_num_obc_periphs)-1 downto 0);
  signal obc_periph_en          : std_logic_vector(c_num_obc_periphs-1 downto 0);
  signal obc_num_bytes_in       : std_logic_vector(c_msp_dl_width-1 downto 0);
  signal obc_buf_we_in          : std_logic;
  signal obc_buf_addr_in        : std_logic_vector(f_log2_size(c_msp_mtu)-1 downto 0);
  signal obc_buf_data_in        : std_logic_vector(7 downto 0);
  signal obc_buf_data_in_rdy_p  : std_logic;
  signal obc_buf_data_out       : std_logic_vector(7 downto 0);
  signal obc_buf_data_out_rdy_p : std_logic;
  
  -- Temporary SPI signals
  -- TODO: Remove!
  signal spi_cs_n : std_logic;
  signal spi_sclk : std_logic;
  signal spi_mosi : std_logic;
  signal spi_miso : std_logic;

--==============================================================================
--  architecture begin
--==============================================================================
begin

  --============================================================================
  -- Button debouncer for use in e.g., reset
  --============================================================================
  cmp_btn_debounce : debouncer
    generic map
    (
      g_nr_buttons      => g_nr_buttons,
      g_debounce_cycles => 1_000_000
    )
    port map
    (
      clk_i   => clk_50meg_i,
      btn_n_i => btn_n_i,
      btn_o   => btn
    );

  --============================================================================
  -- Reset signal generation
  --============================================================================
  rst_ext <= btn(0);
  
  p_reset : process (clk_50meg_i, rst_ext, plls_unlocked_p) is
  begin
    if (rst_ext = '1') or (plls_unlocked_p = '1') then
      rst <= '1';
      rst_count <= (others => '0');
    elsif rising_edge(clk_50meg_i) then
      if (rst = '1') then
        rst_count <= rst_count + 1;
        if (rst_count = g_reset_period-1) then
          rst <= '0';
        end if;
      end if;
    end if;
  end process;

  rst_n <= not rst;

  --============================================================================
  -- Instantiate PLLs
  --============================================================================
  cmp_plls : plls
    port map
    (
      rst_a_i           => rst,
      clk_50meg_i       => clk_50meg_i,
  
      clk_adc_o         => clk_adc,
      clk_100meg_o      => clk_100meg,
  
      main_pll_locked_o => plls_locked
    );
    
  p_plls_unlocked : process (clk_50meg_i) is
  begin
    if rising_edge(clk_50meg_i) then
      plls_locked_d0 <= plls_locked;
      plls_unlocked_p <= plls_locked_d0 and (not plls_locked);
    end if;
  end process p_plls_unlocked;
    
  --============================================================================
  -- MIST OBC I2C slave and on-FPGA bus logic
  --============================================================================
  -- Peripheral enable based on selection
gen_obc_en : for i in 0 to c_num_obc_periphs-1 generate
  obc_periph_en(i) <= '1' when obc_periph_sel =
                            std_logic_vector(to_unsigned(i, obc_periph_sel'length)) else
                      '0';
end generate gen_obc_en;
  
  -- MUX peripheral module outputs to OBC interface inputs
  
  -- Instantiate OBC interface component
  cmp_obc_interface : mist_obc_interface
    generic map
    (
      g_num_periphs => 1,
      g_baud_div    => g_baud_div
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
      i2c_addr_i  => c_i2c_address,

      tip_o       => open,
      err_p_o     => open,
      wdto_p_o    => open,

      -- Peripheral module signals
      periph_sel_o              => obc_periph_sel,
      
      periph_num_data_bytes_i   => obc_num_bytes_in,
      periph_buf_we_i           => obc_buf_we_in,
      periph_buf_addr_i         => obc_buf_addr_in,
      periph_buf_data_i         => obc_buf_data_in,
      periph_data_rdy_p_i       => obc_buf_data_in_rdy_p,
      
      periph_buf_data_o         => obc_buf_data_out,
      periph_data_rdy_p_o       => obc_buf_data_out_rdy_p,

      -- TEMPORARY: UART RX and TX
      rxd_i       => rxd_i,
      txd_o       => txd_o
    );

  --============================================================================
  -- Housekeeping registers
  --============================================================================
  cmp_hk_regs : hk_regs
    port map
    (
      clk_i               => clk_100meg,
      rst_n_a_i           => rst_n,
      
      -- Number of bytes available on data request
      num_bytes_o         => obc_num_bytes_in,
      
      -- Interface to MSP data buffer
      data_ld_p_i         => '0',
      we_o                => obc_buf_we_in,
      addr_o              => obc_buf_addr_in,
      data_o              => obc_buf_data_in,
      data_rdy_p_o        => obc_buf_data_in_rdy_p,
      
      -- Interface to modules providing housekeeping
      gw_vers_i           => c_gw_version,
      leds_i              => led
    );

  --============================================================================
  -- Debug LEDs
  --============================================================================
  p_debug_leds : process (clk_100meg, rst_n) is
  begin
    if (rst_n = '0') then
      led <= (others => '0');
      led_sequenced <= '0';
      led_sequence_div <= (others => '0');
      led_sequence_count <= (others => '0');
    elsif rising_edge(clk_100meg) then
      -- Sequence the LEDs on reset
      if (led_sequenced = '0') then
        led_sequence_div <= led_sequence_div + 1;
        if (led_sequence_div = 1_249_999) then
          led_sequence_div <= (others => '0');
          led_sequence_count <= led_sequence_count + 1;
          if (led_sequence_count = 0) then
            led <= x"01";
          elsif (led_sequence_count < 8) then
            led <= led(6 downto 0) & '0';
          elsif (led_sequence_count < 15) then
            led <= '0' & led(7 downto 1);
          else
            led <= (others => '0');
            led_sequenced <= '1';
          end if;
        end if;
        
      -- After reset, use setting obtained from OBC
      else
        if (obc_periph_en(c_periph_leds) = '1') and (obc_buf_data_out_rdy_p = '1') then
          led <= obc_buf_data_out;
        end if;
      end if;
    end if;
  end process p_debug_leds;
  
  led_n_o <= not led;
  --============================================================================
  
  -- Temporary
  siphra_sysclk_o <= '0';
  spi_cs_n <= '1';
  spi_sclk <= '0';
  spi_mosi <= '0';
  
  -- Actual SPI outputs
  spi_cs_n_o <= '1';
  spi_sclk_o <= '0';
  spi_mosi_o <= '0';
  
end architecture arch;
--==============================================================================
--  architecture end
--==============================================================================
