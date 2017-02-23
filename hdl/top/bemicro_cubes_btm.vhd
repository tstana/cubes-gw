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


entity bemicro_cubes_btm is
  generic
  (
    g_nr_buttons : natural := 1
  );
  port
  (
    clk_50meg_i : in  std_logic;
    btn_n_i     : in  std_logic_vector(g_nr_buttons-1 downto 0);

    led_n_o     : out std_logic_vector(7 downto 0)
  );
end entity bemicro_cubes_btm;


architecture arch of bemicro_cubes_btm is

  --============================================================================
  -- Constant declarations
  --============================================================================
  constant c_baud_div_int : natural := 867;
  constant c_baud_div     : std_logic_vector := 
      std_logic_vector(to_unsigned(c_baud_div_int, f_log2_size(c_baud_div_int)));

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

  --============================================================================
  -- Signal declarations
  --============================================================================
  signal clk_100meg             : std_logic;
  signal clk_adc                : std_logic;

  signal rst_n                  : std_logic;
  signal rst                    : std_logic;
  
  signal btn                    : std_logic_vector(g_nr_buttons-1 downto 0);

  signal led_div                : unsigned(26 downto 0);
  signal led                    : unsigned( 7 downto 0);

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
  -- Blink some LEDs
  --============================================================================
  p_blinky : process (clk_100meg, rst_n) is
  begin
    if (rst_n = '0') then
      led_div <= (others => '0');
      led     <= (others => '0');
    elsif rising_edge(clk_100meg) then
      led_div <= led_div + 1;
      if (led_div = 99_999_999) then
        led_div <= (others => '0');
        led     <= led + 1;
      end if;
    end if;
  end process p_blinky;
  
  led_n_o <= not std_logic_vector(led);

end architecture arch;
--==============================================================================
--  architecture end
--==============================================================================
