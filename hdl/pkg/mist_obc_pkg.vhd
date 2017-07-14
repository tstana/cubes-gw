--==============================================================================
-- MIST OBC VHDL package
--==============================================================================
--
-- author: Theodor Stana (theodor.stana@gmail.com)
--
-- date of creation: 2017-02-24
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
--    2017-02-24   Theodor Stana     File created
--==============================================================================
-- TODO: -
--==============================================================================

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.genram_pkg.all;
use work.wishbone_pkg.all;


package mist_obc_pkg is
  --============================================================================
  -- OBC-specific defines
  --============================================================================
  -- DLC field width constants
  constant OBC_MTU            : natural       := 507;
  constant OBC_DL_WIDTH       : natural       :=  32;
  constant OBC_DL_NR_BYTES    : natural       := f_log2_size(OBC_DL_WIDTH);
  constant OBC_FCS_WIDTH      : natural       :=   0;
  constant OBC_FCS_NR_BYTES   : natural       :=   0;  -- f_log2_size(OBC_FCS_WIDTH);

  -- OBC message ID type
  type t_obc_msg is record
    msg_id : std_logic_vector(7 downto 0);
    wb_adr : t_wishbone_address;
  end record t_obc_msg;

  --============================================================================
  -- Commands
  --============================================================================
  type t_commands is array (natural range <>) of t_obc_msg;
  
  constant c_commands : t_commands := (
    ( x"90",  x"00000004" ),
    ( x"91",  x"00000008" )
  );
  
end package mist_obc_pkg;
