library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;


entity adc_ctrl is
  port
  (
    clk_i               : in std_logic;
    
    cmd_ready_i         : in  std_logic;
    cmd_valid_o         : out std_logic;
    cmd_channel_o       : out std_logic_vector(4 downto 0)
  );
end entity adc_ctrl;


architecture arch of adc_ctrl is

begin

  cmd_valid_o <= '1';
  cmd_channel_o <= "00001";

end architecture;