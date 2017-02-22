library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.genram_pkg.all;


entity tb_uart is
  generic (
    g_baud_div_bits : natural := 10
  );
end entity;


architecture behav of tb_uart is

  --===============================================================================================
  -- Constant declarations
  --===============================================================================================
  constant c_clk_per : time := 10 ns;
  constant c_baud_div_int : natural := 867;
  constant c_baud_div     : std_logic_vector := 
                  std_logic_vector(to_unsigned(c_baud_div_int, f_log2_size(c_baud_div_int)));
  
  --===============================================================================================
  -- Function declarations
  --===============================================================================================
  procedure sample(
    signal clk : in  std_logic;
    signal sig : out std_logic
  ) is
  begin
    sig <= '1';
    wait until rising_edge(clk);
    sig <= '0';
  end procedure;
  
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


  --===============================================================================================
  -- Signal declarations
  --===============================================================================================
  signal clk_100meg     : std_logic;
  signal rst_n          : std_logic;
  
  signal txd, rxd       : std_logic;

  signal tx_start       : std_logic;
  signal tx_data        : unsigned(7 downto 0) := x"30";
  signal tx_data_d0     : unsigned(7 downto 0) := x"30";
  signal tx_ready       : std_logic;
  signal start_delay    : unsigned(7 downto 0);
  
  signal rx_sreg        : std_logic_vector(7 downto 0) := (others => '0');
  signal rx_count       : unsigned(2 downto 0) := (others => '0');
  signal rx_data        : std_logic_vector(7 downto 0);
  signal rx_ready       : std_logic;
  signal rx_ready_d0    : std_logic;
  
  signal baud_x8_count  : unsigned(g_baud_div_bits-4 downto 0);
  signal baud_x8_div    : unsigned(g_baud_div_bits-4 downto 0);
  signal baud_x8_tick   : std_logic;
  signal baud_en        : std_logic;
  signal baud_rx_en     : std_logic;
  signal baud_tx_en     : std_logic;
  signal baud_sreg      : std_logic_vector(7 downto 0);
  signal baud_div       : unsigned(2 downto 0);
  signal baud_tick      : std_logic;
  
  signal mon_sample     : std_logic := '0';
  
begin

  --===============================================================================================
  -- Clock and reset
  --===============================================================================================
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
  
  --===============================================================================================
  -- Stimuli process
  --===============================================================================================
  p_stim : process (clk_100meg, rst_n) is
  begin
    if (rst_n = '0') then
      tx_start    <= '0';
      start_delay <= (others => '0');
      tx_data     <= x"30";
      tx_data_d0  <= x"30";
    elsif rising_edge(clk_100meg) then
      tx_start    <= '0';
      tx_data_d0  <= tx_data;
      if (tx_ready = '1') then
        start_delay <= start_delay + 1;
        if (start_delay = (start_delay'range => '1')) then
          tx_data  <= tx_data + 1;
          tx_start <= '1';
          if (tx_data = x"39") then
            tx_data <= x"13";
          elsif (tx_data = x"13") then
            tx_data <= x"30";
          end if;
        end if;
      end if;
    end if;
  end process p_stim;

  --===============================================================================================
  -- Instantiate DUT
  --===============================================================================================
  DUT : uart
    generic map
    (
      g_baud_div_bits => f_log2_size(c_baud_div_int)
    )
    port map
    (
      -- Clock, reset
      clk_i         => clk_100meg,
      rst_n_a_i     => rst_n,

      -- Ports to external world
      rxd_i         => txd,
      txd_o         => txd,

      -- Ports to other logic
      baud_div_i    => c_baud_div,
      tx_start_p_i  => tx_start,
      tx_data_i     => std_logic_vector(tx_data_d0),
      tx_ready_o    => tx_ready,

      rx_data_o     => rx_data,
      rx_ready_o    => rx_ready,

      frame_err_o   => open
    );

  --===============================================================================================
  -- DUT monitor process
  --===============================================================================================
  p_mon : process is
  begin
    baud_en <= '0';
    wait until tx_start = '1';
    baud_en <= '1';
    wait until baud_div = to_unsigned(4, baud_div'length);
    sample(clk_100meg, mon_sample);
    assert (txd = '0') report "Start bit wrong!" severity error;
    while rx_count /= (rx_count'range => '1') loop
      wait until baud_div = to_unsigned(4, baud_div'length);
      sample(clk_100meg, mon_sample);
      rx_sreg <= txd & rx_sreg(7 downto 1);
      rx_count <= rx_count + 1;
    end loop;
    wait until baud_div = to_unsigned(4, baud_div'length);
    sample(clk_100meg, mon_sample);
    assert (txd = '1') report "Stop bit wrong!" severity error;
  end process;
  
  --===============================================================================================
  -- Baud rate generator
  --===============================================================================================  
  -- Create x8 baud-rate clock by dividing input baud rate divider by eight
  baud_x8_div <= unsigned(c_baud_div(g_baud_div_bits-1 downto 3));
  
  -- Divide clk_i to baud rate x8
  p_baud_x8_div : process (clk_100meg, rst_n) is
  begin
    if (rst_n = '0') then
      baud_x8_count <= to_unsigned(1, g_baud_div_bits-3);
      baud_x8_tick  <= '0';
    elsif rising_edge(clk_100meg) then
      baud_x8_count <= (others => '0');
      if (baud_en = '1') then
        baud_x8_count <= baud_x8_count - 1;
        baud_x8_tick  <= '0';
        if (baud_x8_count = 1) then
          baud_x8_count <= baud_x8_div;
          baud_x8_tick  <= '1';
        end if;
      end if;
    end if;
  end process p_baud_x8_div;
  
  -- Divide baud x8 clock
  p_baud_div : process (clk_100meg, rst_n) is
  begin
    if (rst_n = '0') then
      baud_div  <= (others => '0');
      baud_tick <= '0';
    elsif rising_edge(clk_100meg) then
      if (baud_en = '1') then
        baud_tick <= '0';
        if (baud_x8_tick = '1') then
          baud_div <= baud_div - 1;
          if (baud_div = (baud_div'range => '0')) then
            baud_tick <= '1';
          end if;
        end if;
      else
        baud_div <= (others => '0');
      end if;
    end if;
  end process p_baud_div;

end architecture;