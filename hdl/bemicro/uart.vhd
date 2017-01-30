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
end entity uart;

architecture behav of uart is
  
  --===============================================================================================
  -- Signal declarations
  --===============================================================================================
  signal baud_x8_count  : unsigned(g_baud_div_bits-4 downto 0);
  signal baud_x8_div    : unsigned(g_baud_div_bits-4 downto 0);
  signal baud_x8_tick   : std_logic;
  signal baud_sreg      : std_logic_vector(7 downto 0);
  signal baud_tick      : std_logic;
  
  signal tmp : unsigned(7 downto 0);
  signal delay : std_logic;
  
  signal rxd           : std_logic;
  
begin
  
  --===========================================================================
  -- Baud rate generator
  --===========================================================================
  -- Create x8 baud-rate clock by dividing input baud rate divider by eight
  baud_x8_div <= unsigned(baud_div_i(g_baud_div_bits-1 downto 3));
  
  -- Synchronize RXD port to clock domain
  cmp_sync_rx : gc_sync_ffs
    port map
    (
      clk_i     => clk_i,
      rst_n_i   => rst_n_a_i,
      data_i    => rxd_i,
      synced_o  => rxd
    );
    
  -- Divide clk_i to baud rate x8
  p_baud_div : process (clk_i, rst_n_a_i) is
  begin
    if (rst_n_a_i = '0') then
      baud_x8_count <= to_unsigned(1, g_baud_div_bits-3);
      baud_x8_tick  <= '0';
    elsif rising_edge(clk_i) then
      baud_x8_count <= baud_x8_count - 1;
      baud_x8_tick  <= '0';
      if (baud_x8_count = 1) then
        baud_x8_count <= baud_x8_div;
        baud_x8_tick  <= '1';
      end if;
    end if;
  end process p_baud_div;
  
  -- Generate baud rate tick
  p_baud_sreg : process (clk_i, rst_n_a_i) is
  begin
    if (rst_n_a_i = '0') then
      baud_sreg <= "00000001";
    elsif rising_edge(clk_i) then
      if (baud_x8_tick = '1') then
        baud_sreg <= baud_sreg(0) & baud_sreg(7 downto 1);
      end if;
    end if;
  end process p_baud_sreg;

  baud_tick <= baud_sreg(0) and baud_x8_tick;

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