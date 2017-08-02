library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.genram_pkg.all;

package msp_pkg is
  --============================================================================
  -- Function declarations
  --============================================================================
  function to7bits(v : std_logic_vector(7 downto 0)) return std_logic_vector;
  function to3bits(v : std_logic_vector(3 downto 0)) return std_logic_vector;
  function f_obc_sel(periph : natural) return std_logic_vector;
  
  --============================================================================
  -- Constant declarations
  --============================================================================
  -- MSP size defines
  constant c_msp_mtu            : natural;
  constant c_msp_dl_width       : natural;
  constant c_msp_dl_num_bytes   : natural;
  constant c_msp_fcs_width      : natural;
  constant c_msp_fcs_num_bytes  : natural;

  -- MSP operations
  constant c_msp_op_null          : std_logic_vector(6 downto 0);
  constant c_msp_op_data_frame    : std_logic_vector(6 downto 0);
  constant c_msp_op_f_ack         : std_logic_vector(6 downto 0);
  constant c_msp_op_t_ack         : std_logic_vector(6 downto 0);
  constant c_msp_op_req_payload   : std_logic_vector(6 downto 0);
  constant c_msp_op_req_hk        : std_logic_vector(6 downto 0);
  constant c_msp_op_exp_send      : std_logic_vector(6 downto 0);
  constant c_msp_op_get_cubes_id  : std_logic_vector(6 downto 0);
  constant c_msp_op_set_leds      : std_logic_vector(6 downto 0);
  constant c_msp_op_siphra_reg_op : std_logic_vector(6 downto 0);
  
  -- Peripherals to OBC interface
  constant c_num_obc_periphs            : natural;
  
  constant c_periph_hk_regs             : natural;
  constant c_periph_leds                : natural;
  
  constant c_periph_siphra_base         : natural;
  constant c_periph_siphra_reg_op       : natural;
  constant c_periph_siphra_reg_val      : natural;

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
  constant c_msp_mtu              : natural := 507;
  constant c_msp_dl_width         : natural :=  32;
  constant c_msp_dl_num_bytes     : natural := f_log2_size(c_msp_dl_width)-1;
  constant c_msp_fcs_width        : natural :=   0;
  constant c_msp_fcs_num_bytes    : natural :=   0;  -- f_log2_size(c_msp_fcs_width);

  -- MSP operations
  constant c_msp_op_null          : std_logic_vector(6 downto 0) := to7bits(x"00");
  constant c_msp_op_data_frame    : std_logic_vector(6 downto 0) := to7bits(x"01");
  constant c_msp_op_f_ack         : std_logic_vector(6 downto 0) := to7bits(x"02");
  constant c_msp_op_t_ack         : std_logic_vector(6 downto 0) := to7bits(x"03");
  constant c_msp_op_req_payload   : std_logic_vector(6 downto 0) := to7bits(x"10");
  constant c_msp_op_req_hk        : std_logic_vector(6 downto 0) := to7bits(x"11");
  constant c_msp_op_exp_send      : std_logic_vector(6 downto 0) := to7bits(x"20");
  constant c_msp_op_get_cubes_id  : std_logic_vector(6 downto 0) := to7bits(x"40");
  constant c_msp_op_set_leds      : std_logic_vector(6 downto 0) := to7bits(x"41");
  constant c_msp_op_siphra_reg_op : std_logic_vector(6 downto 0) := to7bits(x"42");

  -- Peripherals to OBC interface
  constant c_num_obc_periphs            : natural := 4;
  
  constant c_periph_hk_regs             : natural := 0;
  constant c_periph_leds                : natural := 1;
  
  constant c_periph_siphra_base         : natural := 2;
  constant c_periph_siphra_reg_op       : natural := 2;
  constant c_periph_siphra_reg_val      : natural := 3;

  -- Shorthand for nasty std_logic_vector(to_unsigned())...
  function f_obc_sel(periph : natural) return std_logic_vector is
  begin
    return std_logic_vector(to_unsigned(periph, f_log2_size(c_num_obc_periphs)));
  end function;
 
end package body;