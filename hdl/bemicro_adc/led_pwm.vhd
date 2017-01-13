library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;


entity led_pwm is
  port
  (
    clk_i               : in  std_logic;

    rst_a_i             : in  std_logic;

    adc_result_valid_i  : in  std_logic;
    adc_result_i        : in  std_logic_vector(11 downto 0);

    led_o               : out std_logic_vector( 7 downto 0)
  );
end entity led_pwm;


architecture behav of led_pwm is

  type t_led_adc_val is array (0 to 7) of unsigned(8 downto 0);
  
  signal led_adc_val    : t_led_adc_val;
  
  -- Each LED counts up to 512 (4096 / 8 -- ADC range / nr. LEDs)
  signal pwm_counter      : unsigned( 8 downto 0);
  signal adc_result_int   : unsigned(11 downto 0);

begin

  adc_result_int  <= unsigned(adc_result_i);

  p_pwm_counter: process (clk_i, rst_a_i) is
  begin
    if (rst_a_i = '1') then
      pwm_counter <= (others => '0');
    elsif rising_edge(clk_i) then
      pwm_counter <= pwm_counter + 1;
    end if;
  end process;
  
  p_led_control : process (clk_i, rst_a_i) is
    type t_diff_array is array (0 to 7) of unsigned(11 downto 0);
    variable adc_result_diff : t_diff_array;
  begin
    if (rst_a_i = '1') then
      led_o <= (others => '0');
    elsif rising_edge(clk_i) then
      for i in 0 to 7 loop
        adc_result_diff(i) := adc_result_int - 512*i;
        
        if (adc_result_valid_i = '1') then
          if (adc_result_int < 512*i) then
            led_adc_val(i) <= (others => '0');
          elsif (adc_result_int > 512*(i+1)) then
            led_adc_val(i) <= (others => '1');
          else
            led_adc_val(i) <= adc_result_diff(i)(8 downto 0);
          end if;
        end if;
        
        if (pwm_counter < led_adc_val(i)) then
          led_o(i) <= '1';
        else
          led_o(i) <= '0';
        end if;
        
      end loop;
    end if;
  end process;

end architecture behav;