library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;


entity bemicro_uart is
  generic
  (
    g_nr_buttons : natural := 1
  );
  port
  (
    clk_50meg_i : in  std_logic;
    btn_n_i     : in  std_logic_vector(g_nr_buttons-1 downto 0);

    led_n_o     : out std_logic_vector(7 downto 0);

    txd_o       : out std_logic
  );
end entity bemicro_uart;


architecture behav of bemicro_uart is

  --===============================================================================================
  -- Component declarations
  --===============================================================================================
  -- MAX10 PLLs
  component plls is
    port
    (
      rst_a_i           : in  std_logic;
      clk_50meg_i       : in  std_logic;

      clk_adc_o         : out std_logic;
      clk_100meg_o      : out std_logic;

      main_pll_locked_o : out std_logic
    );
  end component plls;

  -- UART
  component uart is
    generic
    (
    g_baud_div_bits : natural := 16
    );
    port
    (
      -- Clock, reset
      clk_i       : in  std_logic;
      rst_n_a_i   : in  std_logic;
      
      -- Ports to external world
      rxd_i       : in  std_logic;
      txd_o       : out std_logic;
      
      -- Ports to other logic
      baud_div_i  : in  std_logic_vector(g_baud_div_bits-1 downto 0);
      rx_data_o   : out std_logic_vector(7 downto 0);
      tx_data_i   : in  std_logic_vector(7 downto 0);
      
      rx_ready_o  : out std_logic;
      tx_ready_o  : out std_logic;
      frame_err_o : out std_logic
    );
  end component uart;

  -- Button debouncer
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

  --===============================================================================================
  -- Signal declarations
  --===============================================================================================
  signal clk_100meg             : std_logic;
  signal clk_adc                : std_logic;

  signal rst_n                  : std_logic;
  signal rst                    : std_logic;
  
  signal btn                    : std_logic_vector(g_nr_buttons-1 downto 0);
  
  signal led_div                : unsigned(19 downto 0);
  signal led                    : unsigned( 7 downto 0);
  
begin

  cmp_btn_debounce : debouncer
    generic map
    (
      g_nr_buttons      => g_nr_buttons,
      g_debounce_cycles => 1_000_000
    )
    port map
    (
      clk_i   => clk_50meg_i,
      btn_n_i => btn_n_i,
      btn_o   => btn
    );
  
  rst   <= btn(0);
  rst_n <= not rst;

  cmp_plls : plls
    port map
    (
      rst_a_i           => rst,
      clk_50meg_i       => clk_50meg_i,
  
      clk_adc_o         => clk_adc,
      clk_100meg_o      => clk_100meg,
  
      main_pll_locked_o => open
    );

  p_led_div : process (clk_adc, rst_n) is
  begin
    if (rst_n ='0') then
      led_div <= (others => '0');
      led     <= (others => '0');
    elsif rising_edge(clk_adc) then
      led_div <= led_div + 1;
      if (led_div = 999_999) then
        led_div <= (others => '0');
        led     <= led + 1;
      end if;
    end if;
  end process p_led_div;
  
  led_n_o <= not std_logic_vector(led);

  cmp_uart : uart
    generic map
    (
      g_baud_div_bits => 10
    )
    port map
    (
      -- Clock, reset
      clk_i       => clk_100meg,
      rst_n_a_i   => rst_n,

      -- Ports to external world
      rxd_i       => '0',
      txd_o       => txd_o,

      -- Ports to other logic
      baud_div_i  => "1101100100",
      rx_data_o   => open,
      tx_data_i   => (others => '0'),

      rx_ready_o  => open,
      tx_ready_o  => open,
      frame_err_o => open
    );

end architecture behav;