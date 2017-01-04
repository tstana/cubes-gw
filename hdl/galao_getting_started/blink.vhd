library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity blink is
    Port ( clk_i : in STD_LOGIC;
           led_o : out STD_LOGIC_VECTOR (1 downto 0));
end blink;

architecture Behavioral of blink is

    signal div_count : unsigned(27 downto 0);
    signal led       : unsigned( 1 downto 0);
    
begin

    process (clk_i) is
    begin
        if rising_edge(clk_i) then
            div_count <= div_count + 1;
            if (div_count = 99_999_999) then
                div_count <= (others => '0');
                led <= led + 1;
            end if;
        end if;
    end process;
    
    led_o <= std_logic_vector(led);

end Behavioral;
