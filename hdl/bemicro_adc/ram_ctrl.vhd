library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity ram_ctrl is
  generic
  (
    g_adc_nr_bits : natural := 12;
    
    g_ram_width   : natural := 32;
    g_ram_depth   : natural := 1024
  );
  port
  (
    clk_i             : in  std_logic;
    rst_i             : in  std_logic;

    adc_data_valid_i  : in  std_logic;
    adc_channel_i     : in  std_logic_vector(4 downto 0);
    adc_result_i      : in  std_logic_vector(g_adc_nr_bits-1 downto 0);

    ram_addr_o        : out std_logic_vector(log2(g_ram_depth)-1 downto 0);

    ram_wr_o          : out std_logic;
    ram_data_o        : out std_logic_vector(g_ram_width-1 downto 0);

    ram_rd_o          : out std_logic;
    ram_data_i        : in  std_logic_vector(g_ram_width-1 downto 0)
  );
end entity ram_ctrl;


architecture arch of ram_ctrl is

  signal wr_ptr   : unsigned(log2(g_ram_depth)-1 downto 0);
  signal rd_ptr   : unsigned(log2(g_ram_depth)-1 downto 0);
  
  signal wr_data  : std_logic_vector(g_ram_width-1 downto 0);
  signal rd_data  : std_logic_vector(g_ram_width-1 downto 0);

begin

  p_ram_ctrl : process (clk_i) is
  begin
    if rising_edge(clk_i) then
      if (rst_i = '1') then
        wr_ptr  <= (others => '0');
        rd_ptr  <= (others => '0');
        wr_data <= (others => '0');
        rd_data <= (others => '0');
      else
        if (adc_data_valid_i = '1') then
          wr_ptr <= wr_ptr + 1;
          wr_data(g_adc_nr_bits-1 downto 0) <= adc_result_i;
        end if;
      end if;
    end if;
  end process p_ram_ctrl;

  ram_addr_o <= std_logic_vector(wr_ptr);
  ram_data_o <= std_logic_vector(wr_data);

end architecture arch;
