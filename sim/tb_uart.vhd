library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;


entity tb_uart is
end entity;


architecture behav of tb_uart is

  --===============================================================================================
  -- Constant declarations
  --===============================================================================================
  constant c_clk_per : time := 10ns;
  
  --===============================================================================================
  -- Component declarations
  --===============================================================================================
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

  --===============================================================================================
  -- Signal declarations
  --===============================================================================================
  signal clk_100meg     : std_logic;
  signal rst_n          : std_logic;
  
  signal txd_o          : std_logic;

  
begin

  p_clk : process is
  begin
    clk_100meg <= '0';
    wait for c_clk_per/2;
    clk_100meg <= '1';
    wait for c_clk_per/2;
  end process p_clk;
  
  p_rst : process is
  begin
    rst_n <= '0';
    wait for c_clk_per*5;
    rst_n <= '1';
    wait;
  end process p_rst;

  DUT : uart
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

end architecture;