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
use work.wishbone_pkg.all;


entity bemicro_cubes_btm is
  generic
  (
    g_nr_buttons : natural := 1
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
    spi_miso_i        : in  std_logic
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
  -- Wishbone slaves
  ------------------------------------------------------------------------------
  -- Number of slaves
  constant c_num_wb_slaves    : natural := 3;
  
  -- Slave indices
  constant c_id_regs_idx      : natural := 0;
  constant c_led_ctrl_idx     : natural := 1;
  constant c_siphra_ctrl_idx  : natural := 2;
  
  -- Slave base addresses
  constant c_id_regs_addr     : t_wishbone_address := x"00000000";
  constant c_led_ctrl_addr    : t_wishbone_address := x"00000010";
  constant c_siphra_ctrl_addr : t_wishbone_address := x"00000020";
  
  -- Slave xwb_crossbar masks
  constant c_id_regs_mask     : t_wishbone_address := x"fffffff0";
  constant c_led_ctrl_mask    : t_wishbone_address := x"fffffff0";
  constant c_siphra_ctrl_mask : t_wishbone_address := x"fffffff0";
  
  ------------------------------------------------------------------------------
  -- Wishbone address layout
  ------------------------------------------------------------------------------
  constant c_wb_layout : t_wishbone_address_array(0 to c_num_wb_slaves-1) := (
    c_id_regs_idx     => c_id_regs_addr,
    c_led_ctrl_idx    => c_led_ctrl_addr,
    c_siphra_ctrl_idx => c_siphra_ctrl_addr
  );
  
  
  constant c_wb_address_mask : t_wishbone_address_array(0 to c_num_wb_slaves-1) := (
    c_id_regs_idx     => c_id_regs_mask,
    c_led_ctrl_idx    => c_led_ctrl_mask,
    c_siphra_ctrl_idx => c_siphra_ctrl_mask
  );
  
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
  component mist_i2cs_wbm_bridge is
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
  end component mist_i2cs_wbm_bridge;

  -- ID registers slave
  component wb_id_regs is
    port
    (
      clk_i             : in  std_logic;
      rst_n_a_i         : in  std_logic;
      
      id_version_i      : in  std_logic_vector(15 downto 0);
      
      wbs_i             : in  t_wishbone_slave_in;
      wbs_o             : out t_wishbone_slave_out
    );
  end component wb_id_regs;

  -- Wishbone LED control slave
  component wb_led_ctrl is
    port
    (
      clk_i             : in  std_logic;
      rst_n_a_i         : in  std_logic;
      
      wbs_i             : in  t_wishbone_slave_in;
      wbs_o             : out t_wishbone_slave_out;
      
      led_o             : out std_logic_vector(7 downto 0)
    );
  end component wb_led_ctrl;

  -- SIPHRA controller slave
  component wb_siphra_ctrl is
    port
    (
      clk_i      : in  std_logic;
      rst_n_a_i  : in  std_logic;
      
      spi_cs_n_o : out std_logic;
      spi_sclk_o : out std_logic;
      spi_mosi_o : out std_logic;
      spi_miso_i : in  std_logic;
      
      wbs_i      : in  t_wishbone_slave_in;
      wbs_o      : out t_wishbone_slave_out
    );
  end component wb_siphra_ctrl;

  --============================================================================
  -- Signal declarations
  --============================================================================
  signal clk_100meg             : std_logic;
  signal clk_adc                : std_logic;

  signal rst_n                  : std_logic;
  signal rst                    : std_logic;
  
  signal btn                    : std_logic_vector(g_nr_buttons-1 downto 0);

  signal led                    : std_logic_vector( 7 downto 0);
  
  -- Wishbone signals
  signal xwb_slave_in           : t_wishbone_slave_in_array(0 to 0);
  signal xwb_slave_out          : t_wishbone_slave_out_array(0 to 0);
  signal xwb_masters_in         : t_wishbone_master_in_array(0 to c_num_wb_slaves-1);
  signal xwb_masters_out        : t_wishbone_master_out_array(0 to c_num_wb_slaves-1);

--==============================================================================
--  architecture begin
--==============================================================================
begin

  --============================================================================
  -- Reset signal generation from debounced button
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
  
  rst   <= btn(0);
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
  
      main_pll_locked_o => open
    );
    
  --============================================================================
  -- MIST OBC I2C slave to Wishbone master
  --============================================================================
  cmp_wb_master : mist_i2cs_wbm_bridge
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
      tip_o       => open,
      err_p_o     => open,
      wdto_p_o    => open,

      -- Wishbone master signals
      wbm_i       => xwb_slave_out(0),
      wbm_o       => xwb_slave_in(0),
      
      -- TEMPORARY: UART RX and TX
      rxd_i       => rxd_i,
      txd_o       => txd_o
    );

  --============================================================================
  -- Wishbone crossbar
  --============================================================================
  cmp_crossbar : xwb_crossbar
    generic map
    (
      g_num_masters   => 1,
      g_num_slaves    => c_num_wb_slaves,
      g_registered    => false,
      g_address       => c_wb_layout,
      g_mask          => c_wb_address_mask
    )
    port map
    (
      clk_sys_i       => clk_100meg,
      rst_n_i         => rst_n,
      slave_i         => xwb_slave_in,
      slave_o         => xwb_slave_out,
      master_i        => xwb_masters_in,
      master_o        => xwb_masters_out
    );

  -- ID Registers
  cmp_wb_id_regs : wb_id_regs
    port map
    (
      clk_i             => clk_100meg,
      rst_n_a_i         => rst_n,
      
      id_version_i      => c_gw_version,
      
      wbs_i             => xwb_masters_out(c_id_regs_idx),
      wbs_o             => xwb_masters_in(c_id_regs_idx)
    );
  
  --============================================================================
  -- Light some LEDs from Wishbone
  --============================================================================
  cmp_wb_led_ctrl : wb_led_ctrl
    port map
    (
      clk_i       => clk_100meg,
      rst_n_a_i   => rst_n,
      
      wbs_i       => xwb_masters_out(c_led_ctrl_idx),
      wbs_o       => xwb_masters_in(c_led_ctrl_idx),
      
      led_o       => led
    );

  led_n_o <= not led;
  
  --============================================================================
  -- SIPHRA controller slave
  --============================================================================
  cmp_wb_siphra_ctrl : wb_siphra_ctrl
    port map
    (
      clk_i      => clk_100meg,
      rst_n_a_i  => rst_n,
      
      spi_cs_n_o => spi_cs_n_o,
      spi_sclk_o => spi_sclk_o,
      spi_mosi_o => spi_mosi_o,
      spi_miso_i => spi_miso_i,
      
      wbs_i      => xwb_masters_out(c_siphra_ctrl_idx),
      wbs_o      => xwb_masters_in(c_siphra_ctrl_idx)
    );

    
end architecture arch;
--==============================================================================
--  architecture end
--==============================================================================
