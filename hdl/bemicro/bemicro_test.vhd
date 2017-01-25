library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;


entity bemicro_test is
  generic
  (
    g_nr_buttons : natural := 2
  );
  port
  (
    clk_50meg_i   : in  std_logic;
    
    btn_n_i       : in  std_logic_vector(g_nr_buttons-1 downto 0);
    led_n_o       : out std_logic_vector(7 downto 0)
  );
end entity;


architecture behav of bemicro_test is

  
  --===========================================================================
  -- Component declarations
  --===========================================================================
  component debouncer is
    generic
    (
      g_nr_buttons      : natural := 4;
      g_debounce_cycles : natural := 10_000_000
    );
    port
    (
      clk_i     : in  std_logic;
      btn_n_i   : in  std_logic_vector(g_nr_buttons-1 downto 0);
      btn_o     : out std_logic_vector(g_nr_buttons-1 downto 0)
    );
  end component debouncer;

  --===========================================================================
  -- Signal declarations
  --===========================================================================
  signal rst_n          : std_logic;
  signal btn            : std_logic_vector(g_nr_buttons-1 downto 0);
  
  signal led_sel_btn    : std_logic;
  signal led_sel_btn_d0 : std_logic;
  signal led_sel        : std_logic;
  
  signal halfsec_count  : unsigned(24 downto 0);
  signal qsec_count     : unsigned(23 downto 0);
  
  signal led_halfsec    : unsigned(7 downto 0);
  signal led_qsec       : unsigned(7 downto 0);

begin

  cmp_debouncer : debouncer
    generic map
    (
      g_nr_buttons      => g_nr_buttons,
      g_debounce_cycles => 500_000
    )
    port map
    (
      clk_i   => clk_50meg_i,
      btn_n_i => btn_n_i,
      btn_o   => btn
    );

  rst_n       <= not btn(0);
  led_sel_btn <= btn(1);
  
  process (clk_50meg_i, rst_n) is
  begin
    if (rst_n = '0') then
      led_sel_btn_d0 <= '0';
      led_sel        <= '0';
    elsif rising_edge(clk_50meg_i) then
      led_sel_btn_d0 <= led_sel_btn;
      if (led_sel_btn = '1') and (led_sel_btn_d0 = '0') then
        led_sel <= not led_sel;
      end if;
    end if;
  end process;
  
  process (clk_50meg_i, rst_n) is
  begin
    if (rst_n = '0') then
      halfsec_count <= (others => '0');
      qsec_count    <= (others => '0');
      led_halfsec   <= (others => '0');
      led_qsec      <= (others => '0');
    elsif rising_edge(clk_50meg_i) then
      halfsec_count <= halfsec_count + 1;
      if (halfsec_count = 24_999_999) then
        halfsec_count <= (others => '0');
        led_halfsec   <= led_halfsec + 1;
      end if;
      
      qsec_count <= qsec_count + 1;
      if (qsec_count = 12_499_999) then
        qsec_count <= (others => '0');
        led_qsec   <= led_qsec + 1;
      end if;
    end if;
  end process;

  led_n_o <=  not std_logic_vector(led_halfsec) when led_sel = '0' else
              not std_logic_vector(led_qsec);

end architecture;