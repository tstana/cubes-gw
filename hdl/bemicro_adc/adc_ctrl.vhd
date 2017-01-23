library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;


entity adc_ctrl is
  port
  (
    clk_i               : in  std_logic;
    
    rst_n_a_i           : in  std_logic;
    
    channel_sel_i       : in  std_logic;
    
    cmd_ready_i         : in  std_logic;
    cmd_valid_o         : out std_logic;
    cmd_channel_o       : out std_logic_vector(4 downto 0)
  );
end entity adc_ctrl;


architecture arch of adc_ctrl is

  signal chan   : std_logic_vector(4 downto 0);
  signal valid  : std_logic;
  
begin


  valid <= '1';
  chan  <= "00001" when channel_sel_i = '0' else
           "01001";

  cmd_valid_o   <= valid;
  cmd_channel_o <= chan;

end architecture;