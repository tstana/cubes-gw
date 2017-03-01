--==============================================================================
-- CUBES ID Registers
--==============================================================================
--
-- author: Theodor Stana (theodor.stana@gmail.com)
--
-- date of creation: 2017-02-28
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
--    2017-02-28   Theodor Stana     File created
--==============================================================================
-- TODO: -
--==============================================================================

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.wishbone_pkg.all;


entity wb_id_regs is
  port
  (
    clk_i             : in  std_logic;
    rst_n_a_i         : in  std_logic;
    
    id_version_i      : in  std_logic_vector(15 downto 0);
    
    wbs_i             : in  t_wishbone_slave_in;
    wbs_o             : out t_wishbone_slave_out
  );
end entity wb_id_regs;


architecture arch of wb_id_regs is

  --============================================================================
  -- Constant declarations
  --============================================================================
  -- Addresses
  constant c_id_nlr_ofs   : std_logic_vector(1 downto 0) := "00";
  constant c_id_nhvr_ofs  : std_logic_vector(1 downto 0) := "01";

  -- "CUBESv" string to combine with id_version_i, yielding:
  -- "CUBESv<maj><min>"
  constant c_id_name    : std_logic_vector(47 downto 0) := x"435542455376";

  --============================================================================
  -- Signal declarations
  --============================================================================
  signal wb_dat_in    : std_logic_vector(c_wishbone_data_width-1 downto 0);
  signal wb_dat_out   : std_logic_vector(c_wishbone_data_width-1 downto 0);
  signal wb_adr       : std_logic_vector(c_wishbone_address_width-1 downto 0);
  signal wb_cyc       : std_logic;
  signal wb_stb       : std_logic;  
  signal wb_we        : std_logic;  
  signal wb_ack       : std_logic;


--==============================================================================
--  architecture begin
--==============================================================================
begin

  --============================================================================
  -- I/O logic
  --============================================================================
  wb_adr <= wbs_i.adr;
  wb_cyc <= wbs_i.cyc;
  wb_stb <= wbs_i.stb;
  
  wbs_o.ack <= wb_ack;
  wbs_o.dat <= wb_dat_out;
  wbs_o.err <= '0';
  wbs_o.rty <= '0';
  wbs_o.stall <= '0';
  wbs_o.int <= '0';

  --============================================================================
  -- Wishbone regs
  --============================================================================
  p_wb_regs : process (clk_i, rst_n_a_i) is
  begin
    if (rst_n_a_i = '0') then
      wb_ack <= '0';
      wb_dat_out <= (others => '0');
    elsif rising_edge(clk_i) then
    elsif rising_edge(clk_i) then
      -- Read-only register
      if (wb_cyc = '1') and (wb_stb = '1') then
        wb_ack <= '1';
        wb_dat_out <= (others => '0');
        if (wb_adr(3 downto 2) = c_id_nlr_ofs) then
          wb_dat_out <= c_id_name(47 downto 16);
        elsif (wb_adr(3 downto 2) = c_id_nhvr_ofs) then
          wb_dat_out(31 downto 16) <= c_id_name(15 downto 0);
          wb_dat_out(15 downto  0) <= id_version_i;
        end if;
      end if;
      
      if (wb_ack = '1') then
        wb_ack <= '0';
      end if;
    end if;
  end process p_wb_regs;

end architecture arch;
--==============================================================================
--  architecture end
--==============================================================================
