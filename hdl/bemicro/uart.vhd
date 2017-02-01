library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.gencores_pkg.all;


entity uart is
  generic
  (
    g_baud_div_bits : natural := 16
  );
  port
  (
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
end entity uart;

architecture behav of uart is
  
  --===============================================================================================
  -- Signal declarations
  --===============================================================================================
  signal baud_div           : unsigned(g_baud_div_bits-1 downto 0);
  signal baud_tick          : std_logic;
  signal baud_div_halfbit   : unsigned(g_baud_div_bits-2 downto 0);
  signal baud_halfbit_tick  : std_logic;
  
  signal tmp : unsigned(7 downto 0);
  signal delay : std_logic;
  
begin
  
  --===========================================================================
  -- Baud rate generator
  --===========================================================================
  -- Divider at half-bit
  baud_div_halfbit <= unsigned(baud_div_i(baud_div'high downto 1));
  
  -- Baud rate generation process
  p_baud_div : process (clk_i, rst_n_a_i) is
  begin
    if (rst_n_a_i = '0') then
      baud_div          <= (others => '0');
      baud_tick         <= '0';
      baud_halfbit_tick <= '0';
    elsif rising_edge(clk_i) then
      baud_tick <= '0';
      baud_div  <= baud_div + 1;
      if (baud_div = unsigned(baud_div_i)) then
        baud_div  <= (others => '0');
        baud_tick <= '1';
      end if;

      baud_halfbit_tick <= '0';
      if (baud_div(baud_div'high-1 downto 0) = baud_div_halfbit) then
        baud_halfbit_tick <= '1';
      end if;
    end if;
  end process p_baud_div;

  process (clk_i, rst_n_a_i) is
  begin
    if (rst_n_a_i = '0') then
      tmp <= (others => '0');
      delay <= '0';
    elsif rising_edge(clk_i) then
      if (delay = '0') then
        if baud_tick = '1' then
          delay <= '1';
        end if;
      else
        tmp <= tmp + 1;
        if (tmp = 11) then 
          tmp <= (others => '0');
          delay <= '0';
        end if;
      end if;
    end if;
  end process;

  txd_o <= delay;
  
end architecture behav;