library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.genram_pkg.all;


entity bemicro_uart is
  generic
  (
    g_nr_buttons : natural := 1
  );
  port
  (
  
  
  dbg_o : out std_logic_vector(1 downto 0);
  
    clk_50meg_i : in  std_logic;
    btn_n_i     : in  std_logic_vector(g_nr_buttons-1 downto 0);

    led_n_o     : out std_logic_vector(7 downto 0);

    rxd_i       : in  std_logic;
    txd_o       : out std_logic
  );
end entity bemicro_uart;


architecture behav of bemicro_uart is

  --===============================================================================================
  -- Constant declarations
  --===============================================================================================
  constant c_baud_div_int : natural := 867;
  constant c_baud_div     : std_logic_vector := 
                  std_logic_vector(to_unsigned(c_baud_div_int, f_log2_size(c_baud_div_int)));

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
    
    
    
    stx: out std_logic_vector(1 downto 0);
    srx : out std_logic_vector(1 downto 0);
    dbg:out std_logic;
    
    
      -- Clock, reset
      clk_i         : in  std_logic;
      rst_n_a_i     : in  std_logic;

      -- Ports to external world
      rxd_i         : in  std_logic;
      txd_o         : out std_logic;

      -- Ports to other logic
      baud_div_i    : in  std_logic_vector(g_baud_div_bits-1 downto 0);

      tx_data_i     : in  std_logic_vector(7 downto 0);
      tx_start_p_i  : in  std_logic;
      tx_ready_o    : out std_logic;

      rx_ready_o    : out std_logic;
      rx_data_o     : out std_logic_vector(7 downto 0);

      frame_err_o   : out std_logic
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

  signal tx_start       : std_logic;
  signal tx_data        : unsigned(7 downto 0) := x"30";
  signal tx_ready       : std_logic;
  signal start_delay    : unsigned(26 downto 0);
  
  signal rx_data        : std_logic_vector(7 downto 0);
  signal rx_ready       : std_logic;
  signal rx_ready_d0    : std_logic;
  signal rx_ready_p     : std_logic;
  
  signal frame_err      : std_logic;
  
  signal div        : natural range 0 to 109;
  signal clk_debug  : std_logic;
  signal c : natural range 0 to 461000;
  signal blink      : std_logic;

  signal srx, stx : std_logic_vector(1 downto 0);
  signal dbg : std_logic;
  
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

  cmp_uart : uart
    generic map
    (
      g_baud_div_bits => f_log2_size(c_baud_div_int)
    )
    port map
    (
    
    
    stx => stx,
    srx => srx,
    dbg => dbg,
    
      -- Clock, reset
      clk_i         => clk_100meg,
      rst_n_a_i     => rst_n,

      -- Ports to external world
      rxd_i         => rxd_i,
      txd_o         => txd_o,

      -- Ports to other logic
      baud_div_i    => c_baud_div,

      tx_data_i     => rx_data,
      tx_start_p_i  => tx_start,
      tx_ready_o    => tx_ready,

      rx_ready_o    => rx_ready,
      rx_data_o     => rx_data,
      
      frame_err_o   => frame_err
    );

    dbg_o(0) <= '1' when stx = "111" and srx = "111" else '0';
    dbg_o(1) <= dbg;
    
    
  p_rx_ready_pulse : process (clk_100meg, rst_n) is
  begin
    if rst_n = '0' then
      rx_ready_d0 <= '0';
      rx_ready_p  <= '0';
    elsif rising_edge(clk_100meg) then
      rx_ready_d0 <= rx_ready;
      rx_ready_p <= rx_ready and (not rx_ready_d0);
    end if;
  end process;
  
  
  process (clk_100meg, rst_n) is
  begin
    if rst_n = '0' then
      div <= 0;
      clk_debug <= '0';
    elsif rising_edge(clk_100meg) then
      div <= div + 1;
      if (div = 109) then
        div <= 0;
        clk_debug <= not clk_debug;
      end if;
    end if;
  end process;
  
  process (clk_debug) is
  begin
  if rising_edge(clk_debug) then
  c <= c+1;
  if (c=460999) then
  c <= 0;
  blink <= not blink;
  end if;
  end if;
  end process;
  
  tx_start <= rx_ready_p and (not frame_err);

  led_n_o(7) <= blink;
  led_n_o(6 downto 0) <= not rx_data(6 downto 0);

end architecture behav;