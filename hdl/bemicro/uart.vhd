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
  -- Type declarations
  --===============================================================================================
  type t_state_tx is (
    TX_IDLE,
    TX_START,
    TX_DATA,
    TX_STOP
  );

  --===============================================================================================
  -- Signal declarations
  --===============================================================================================
  signal baud_en            : std_logic;
  signal baud_div           : unsigned(g_baud_div_bits-1 downto 0);
  signal baud_tick          : std_logic;
  signal baud_div_halfbit   : unsigned(g_baud_div_bits-2 downto 0);
  signal baud_halfbit_tick  : std_logic;
  
  signal state_tx           : t_state_tx;
  signal tx_baud_en         : std_logic;
  signal tx_sreg            : std_logic_vector(7 downto 0);
  signal tx_data_count      : unsigned(2 downto 0);
  
  signal rxd                : std_logic;
  signal rxd_d0             : std_logic;
  signal rx_baud_en         : std_logic;

begin
  
  --===============================================================================================
  -- Baud rate generator
  --===============================================================================================
  -- Baud divider enable
  baud_en <= tx_baud_en or rx_baud_en;
  
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
      if (baud_en = '1') then
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
    end if;
  end process p_baud_div;

  --===========================================================================
  -- TX
  --===========================================================================
  p_tx : process (clk_i, rst_n_a_i) is
  begin
    if (rst_n_a_i = '0') then
      state_tx      <= TX_IDLE;
      txd_o         <= '1';
      tx_ready_o    <= '1';
      tx_baud_en    <= '0';
      tx_sreg       <= (others => '0');
      tx_data_count <= (others => '0');
    elsif rising_edge(clk_i) then

      case state_tx is

        when TX_IDLE =>
          txd_o         <= '1';
          tx_ready_o    <= '1';
          tx_data_count <= (others => '0');
          tx_baud_en    <= '0';
          if (tx_start_p_i = '1') then
            tx_ready_o  <= '0';
            tx_sreg     <= tx_data_i;
            txd_o       <= '0';
            tx_baud_en  <= '1';
            state_tx    <= TX_START;
          end if;
          
        when TX_START =>
          if (baud_tick = '1') then
            state_tx  <= TX_DATA;
          end if;
          
        when TX_DATA =>
          txd_o <= tx_sreg(0);
          if (baud_tick = '1') then
            tx_sreg       <= '0' & tx_sreg(7 downto 1);
            tx_data_count <= tx_data_count + 1;
            if (tx_data_count = ((tx_data_count'range) => '1')) then
              txd_o    <= '1';
              state_tx <= TX_STOP;
            end if;
          end if;
          
        when TX_STOP =>
          if (baud_tick = '1') then
            state_tx  <= TX_IDLE;
          end if;
          
        when others =>
          state_tx      <= TX_IDLE;
          txd_o         <= '1';
          tx_data_count <= (others => '0');
          tx_baud_en    <= '0';
          
      end case;

    end if;
  end process p_tx;
  
  --===========================================================================
  -- RX
  --===========================================================================
  -- Synchronize RXD port to clk_i domain
  cmp_sync_rx : gc_sync_ffs
    port map
    (
      clk_i     => clk_i,
      rst_n_i   => rst_n_a_i,
      data_i    => rxd_i,
      synced_o  => rxd
    );
    
  rx_baud_en <= '0';

end architecture behav;