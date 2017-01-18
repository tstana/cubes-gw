library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.genram_pkg.all;


entity ram_ctrl is
  generic
  (
    g_adc_nr_bits : natural := 12;
    
    g_ram_width   : natural := 16;
    g_ram_depth   : natural := 1024
  );
  port
  (
  
  
    dbg_o : out std_logic;
    
    
    
    clk_i               : in  std_logic;
      
    rst_n_a_i           : in  std_logic;

    adc_result_valid_i  : in  std_logic;
    adc_channel_i       : in  std_logic_vector(4 downto 0);
    adc_result_i        : in  std_logic_vector(g_adc_nr_bits-1 downto 0);

    ram_addr_o          : out std_logic_vector(f_log2_size(g_ram_depth)-1 downto 0);

    ram_we_o            : out std_logic;
    ram_data_o          : out std_logic_vector(g_ram_width-1 downto 0);

    ram_data_i          : in  std_logic_vector(g_ram_width-1 downto 0)
  );
end entity ram_ctrl;


architecture arch of ram_ctrl is

  signal wr_ptr   : unsigned(f_log2_size(g_ram_depth)-1 downto 0);
  signal rd_ptr   : unsigned(f_log2_size(g_ram_depth)-1 downto 0);
  
  signal wr_data  : std_logic_vector(g_ram_width-1 downto 0);
  signal rd_data  : std_logic_vector(g_ram_width-1 downto 0);
  signal we       : std_logic;
  
  signal sample_count : unsigned(9 downto 0);
  signal tmp_storage,dbg : std_logic_vector(15 downto 0);
  signal write_done, d0 : std_logic := '0';
  attribute keep : boolean;
  attribute keep of tmp_storage, write_done : signal is true;
  signal delay : unsigned(29 downto 0);
  signal delay_Done : std_logic := '0';
  

begin

  p_ram_ctrl : process (clk_i, rst_n_a_i) is
  begin
    if (rst_n_a_i = '0') then
      wr_ptr   <= (others => '0');
--      rd_ptr   <= (others => '0');
      wr_data  <= (others => '0');
      rd_data  <= (others => '0');
      we       <= '0';
    elsif rising_edge(clk_i) then


      wr_data(15 downto 12) <= (others => '0');

  
      we <= '0';
      if (adc_result_valid_i = '1') and (adc_channel_i = "00001") -- then
                  and write_done = '0' then
                  
                  
        wr_ptr <= wr_ptr + 1;
        wr_data(g_adc_nr_bits-1 downto 0) <= adc_result_i;
        we <= '1';
      end if;
    end if;
  end process p_ram_ctrl;

  sample_count <= wr_ptr;
  process (clk_i) is
  begin
    if rising_edge(clk_i) then
      if (delay < 1_000_000_000) then
        delay <= delay + 1;
        delay_done <= '0';
      else
        delay_done <= '1';
      end if;
      d0 <= write_done;
      if delay_done = '1' and  (write_done = '0') and (d0 = '0') and (sample_count = (sample_count'range => '1')) then
        write_done <= '1';
      end if;
    end if;
  end process;

  p_tmp_storage : process (clk_i) is
  begin
    if rising_edge(clk_i) then
      if (write_done = '1') then
        rd_ptr <= rd_ptr + 1;
        tmp_storage <= ram_data_i;
        dbg <= tmp_storage(tmp_storage'high downto 1) & tmp_storage(tmp_storage'high);
      end if;
    end if;
  end process;
      dbg_o <= dbg(tmp_storage'high);
      
      
  ram_addr_o <= std_logic_vector(wr_ptr) when write_done = '0' else
                std_logic_vector(rd_ptr);
  ram_data_o <= wr_data;
  ram_we_o   <= we;

end architecture arch;
