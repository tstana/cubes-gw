library ieee;
use ieee.std_logic_1164.all;

use work.genram_pkg.all;

package msp_pkg is
  --============================================================================
  -- Function declarations
  --============================================================================
  function to7bits(v : std_logic_vector(7 downto 0)) return std_logic_vector;
  function to3bits(v : std_logic_vector(3 downto 0)) return std_logic_vector;
  
  --============================================================================
  -- Constant declarations
  --============================================================================
  -- MSP size defines
  constant c_obc_mtu            : natural;
  constant c_obc_dl_width       : natural;
  constant c_obc_dl_num_bytes   : natural;
  constant c_obc_fcs_width      : natural;
  constant c_obc_fcs_num_bytes  : natural;

  -- MSP operations
  constant c_op_null            : std_logic_vector(6 downto 0);
  constant c_op_data_frame      : std_logic_vector(6 downto 0);
  constant c_op_f_ack           : std_logic_vector(6 downto 0);
  constant c_op_t_ack           : std_logic_vector(6 downto 0);
  constant c_op_read_all_regs   : std_logic_vector(6 downto 0);
  constant c_op_get_cubes_id    : std_logic_vector(6 downto 0);
  constant c_op_set_leds        : std_logic_vector(6 downto 0);
  constant c_op_get_leds        : std_logic_vector(6 downto 0);
  constant c_op_siphra_reg_op   : std_logic_vector(6 downto 0);

end package;


package body msp_pkg is
  --============================================================================
  -- Function definitions
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
  -- Constant definitions
  --============================================================================
  -- MSP size defines
  constant c_obc_mtu            : natural       := 507;
  constant c_obc_dl_width       : natural       :=  32;
  constant c_obc_dl_num_bytes   : natural       := f_log2_size(c_obc_dl_width)-1;
  constant c_obc_fcs_width      : natural       :=   0;
  constant c_obc_fcs_num_bytes  : natural       :=   0;  -- f_log2_size(c_obc_fcs_width);

  -- MSP operations
  constant c_op_null            : std_logic_vector(6 downto 0) := to7bits(x"00");
  constant c_op_data_frame      : std_logic_vector(6 downto 0) := to7bits(x"01");
  constant c_op_f_ack           : std_logic_vector(6 downto 0) := to7bits(x"02");
  constant c_op_t_ack           : std_logic_vector(6 downto 0) := to7bits(x"03");
  constant c_op_read_all_regs   : std_logic_vector(6 downto 0) := to7bits(x"11");
  constant c_op_get_cubes_id    : std_logic_vector(6 downto 0) := to7bits(x"40");
  constant c_op_set_leds        : std_logic_vector(6 downto 0) := to7bits(x"41");
  constant c_op_get_leds        : std_logic_vector(6 downto 0) := to7bits(x"42");
  constant c_op_siphra_reg_op   : std_logic_vector(6 downto 0) := to7bits(x"43");

end package body;