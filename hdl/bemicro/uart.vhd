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

  type t_state_rx is (
    RX_IDLE,
    RX_START,
    RX_DATA,
    RX_STOP
  );

  --===============================================================================================
  -- Signal declarations
  --===============================================================================================
  signal baud_en            : std_logic;
  signal baud_div           : unsigned(g_baud_div_bits-1 downto 0);
  signal baud_tick          : std_logic;
  signal baud_div_halfbit   : unsigned(g_baud_div_bits-1 downto 0);
  signal baud_halfbit_tick  : std_logic;

  signal state_tx           : t_state_tx;
  signal txd                : std_logic;
  signal tx_baud_en         : std_logic;
  signal tx_sreg            : std_logic_vector(7 downto 0);
  signal tx_data_count      : unsigned(2 downto 0);
  signal tx_ready           : std_logic;

  signal state_rx           : t_state_rx;
  signal rxd                : std_logic;
  signal rxd_d0             : std_logic;
  signal rxd_fedge_p        : std_logic;
  signal rx_sta             : std_logic;
  signal rx_sta_en          : std_logic;
  signal rx_sta_rst         : std_logic;
  signal rx_baud_en         : std_logic;
  signal rx_sreg            : std_logic_vector(7 downto 0);
  signal rx_data_count      : unsigned(2 downto 0);
  signal rx_ready           : std_logic;
  signal frame_err          : std_logic;

begin

  --===============================================================================================
  -- Baud rate generator
  --===============================================================================================
  -- Baud divider enable
  baud_en <= tx_baud_en or rx_baud_en;

  -- Divide baud_div_i by two to generate baud_halfbit_tick
  baud_div_halfbit <= '0' & unsigned(baud_div_i(baud_div'high downto 1));

  -- Baud rate generation process
  p_baud_div : process (clk_i, rst_n_a_i) is
  begin
    if (rst_n_a_i = '0') then
      baud_div          <= (others => '0');
      baud_tick         <= '0';
      baud_halfbit_tick <= '0';
    elsif rising_edge(clk_i) then
      baud_tick <= '0';
      baud_halfbit_tick <= '0';

      if (baud_en = '0') then
        baud_div <= (others => '0');
      else
        baud_div  <= baud_div + 1;
        if (baud_div = unsigned(baud_div_i)) or
              ((rx_sta = '1') and (state_rx = RX_STOP)) then
          baud_div  <= (others => '0');
          baud_tick <= '1';
        end if;

        if (baud_div = baud_div_halfbit) then
          baud_halfbit_tick <= '1';
        end if;
      end if;
    end if;
  end process p_baud_div;

  --===========================================================================
  -- TX
  --===========================================================================
  -- FSM
  p_tx : process (clk_i, rst_n_a_i) is
  begin
    if (rst_n_a_i = '0') then
      state_tx      <= TX_IDLE;
      txd           <= '1';
      tx_ready      <= '1';
      tx_baud_en    <= '0';
      tx_sreg       <= (others => '0');
      tx_data_count <= (others => '0');
    elsif rising_edge(clk_i) then

      case state_tx is

        when TX_IDLE =>
          txd           <= '1';
          tx_ready      <= '1';
          tx_data_count <= (others => '0');
          tx_baud_en    <= '0';
          if (tx_start_p_i = '1') then
            tx_ready    <= '0';
            tx_sreg     <= tx_data_i;
            txd         <= '0';
            tx_baud_en  <= '1';
            state_tx    <= TX_START;
          end if;

        when TX_START =>
        if (baud_tick = '1') then
            state_tx  <= TX_DATA;
          end if;

        when TX_DATA =>
          txd <= tx_sreg(0);
          if (baud_tick = '1') then
            tx_sreg       <= '0' & tx_sreg(7 downto 1);
            tx_data_count <= tx_data_count + 1;
            if (tx_data_count = ((tx_data_count'range) => '1')) then
              state_tx <= TX_STOP;
            end if;
          end if;

        when TX_STOP =>
          txd <= '1';
          if (baud_tick = '1') then
            state_tx  <= TX_IDLE;
          end if;

        when others =>
          state_tx      <= TX_IDLE;

      end case;

    end if;
  end process p_tx;

  -- Assign TX outputs
  txd_o      <= txd;
  tx_ready_o <= tx_ready;

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

  -- Detect a start bit while FSM is in RX_IDLE or RX_STOP state. This is used
  -- to start the RX FSM and, more importantly, synchronize the baud rate ticker
  -- above to the baud rate of the sending machine.
  p_rx_start_bit : process (clk_i, rst_n_a_i) is
  begin
    if (rst_n_a_i = '0') then
      rxd_d0      <= '0';
      rxd_fedge_p <= '0';
      rx_sta      <= '0';
      rx_sta_en   <= '1';
      rx_sta_rst  <= '0';
    elsif rising_edge(clk_i) then
      -- 1. Detect a falling edge on RXD pin.
      rxd_d0 <= rxd;
      rxd_fedge_p <= rxd_d0 and (not rxd);

      -- 2. Use falling edge to signal start bit.
      if (rx_sta_en = '1') and (rxd_fedge_p = '1') then
        rx_sta <= '1';
      elsif (rx_sta_rst = '1') then
        rx_sta <= '0';
      end if;

      -- 3. Disable start bit outside of RX_IDLE and RX_STOP.
      --    * RX_IDLE is interesting for obvious reasons;
      --    * RX_STOP is interesting because due to clock drift, the start bit
      --    may arrive before the FSM makes it to RX_IDLE;
      --    * other falling edges on RXD pin belong to bits, so rx_sta should
      --    be disabled.
      if (rx_sta = '1') then
        rx_sta_en <= '0';
      elsif (state_rx = RX_IDLE) or (state_rx = RX_STOP) then
        rx_sta_en <= '1';
      end if;

      -- 4. Reset start bit signal while at least one cycle of RX_IDLE has
      -- passed.
      rx_sta_rst <= '0';
      if (rx_sta = '1') and (rx_ready = '1') then
        rx_sta_rst <= '1';
      end if;
    end if;
  end process p_rx_start_bit;

  -- FSM
  p_rx : process (clk_i, rst_n_a_i) is
  begin
    if (rst_n_a_i = '0') then
      state_rx      <= RX_IDLE;
      rx_ready      <= '1';
      rx_baud_en    <= '0';
      frame_err     <= '0';
      rx_sreg       <= (others => '0');
      rx_data_count <= (others => '0');
      rx_data_o     <= (others => '0');
      
    elsif rising_edge(clk_i) then

      case state_rx is

        when RX_IDLE =>
          rx_data_count <= (others => '0');
          rx_baud_en    <= '0';
          rx_ready      <= '1';
          rx_data_o     <= rx_sreg;
          if (rx_ready = '1') and (rx_sta = '1') then
            rx_baud_en    <= '1';
            rx_ready      <= '0';
            frame_err     <= '0';
            state_rx      <= RX_START;
          end if;

        when RX_START =>
          if (baud_halfbit_tick = '1') then
            if (rxd = '1') then
              state_rx <= RX_IDLE;
            end if;
          end if;

          if (baud_tick = '1') then
            state_rx <= RX_DATA;
          end if;

        when RX_DATA =>
          if (baud_halfbit_tick = '1') then
            rx_sreg <= rxd & rx_sreg(7 downto 1);
          end if;

          if (baud_tick = '1') then
            rx_data_count <= rx_data_count + 1;
            if (rx_data_count = (rx_data_count'range => '1')) then
              state_rx <= RX_STOP;
            end if;
          end if;

        when RX_STOP =>
          if (baud_halfbit_tick = '1') then
            if (rxd = '0') then
              frame_err <= '1';
            end if;
          end if;

          if (baud_tick = '1') then
            state_rx <= RX_IDLE;
          end if;

        when others =>
          state_rx      <= RX_IDLE;

      end case;

    end if;
  end process p_rx;

  -- Assign RX fabric-side outputs
  rx_ready_o  <= rx_ready;
  frame_err_o <= frame_err;

end architecture behav;
