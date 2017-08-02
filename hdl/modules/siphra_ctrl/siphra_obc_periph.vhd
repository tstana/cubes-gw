library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.msp_pkg.all;
use work.genram_pkg.all;


entity siphra_obc_periph is
  port
  (
    clk_i             : in  std_logic;
    rst_n_a_i         : in  std_logic;

    ----------------------------------------------------------------------------
    -- Interface to MSP data buffer
    --    Sub-peripheral offsets:
    --      (0) : SIPHRA register operation (OBC-S)
    --      (1) : SIPHRA current register value (OBC-R)
    ----------------------------------------------------------------------------
    -- Sub-peripheral enable
    en_i              : in  std_logic_vector(1 downto 0);
    
    -- Data sink interface
    data_rdy_p_i      : in  std_logic;
    num_bytes_i       : in  std_logic_vector(c_msp_dl_width-1 downto 0);
    data_i            : in  std_logic_vector(7 downto 0);

    -- Data source interface
    data_ld_p_i       : in  std_logic;
    num_bytes_o       : out std_logic_vector(c_msp_dl_width-1 downto 0);
    we_o              : out std_logic;
    addr_o            : out std_logic_vector(f_log2_size(c_msp_mtu)-1 downto 0);
    data_o            : out std_logic_vector(7 downto 0);
    data_rdy_p_o      : out std_logic;

    ----------------------------------------------------------------------------
    -- SIPHRA interface
    ----------------------------------------------------------------------------
    -- SIPHRA pins
    siphra_sysclk_o   : out std_logic;
    siphra_txd_i      : in  std_logic;

    -- SPI interface
    spi_cs_n_o        : out std_logic;
    spi_sclk_o        : out std_logic;
    spi_mosi_o        : out std_logic;
    spi_miso_i        : in  std_logic
  );
end entity siphra_obc_periph;


architecture behav of siphra_obc_periph is

  --============================================================================
  -- Type declarations
  --============================================================================
  type t_state is (
    IDLE,
    RECEIVE_DATA,
    SEND_DATA
  );

  --============================================================================
  -- Component declarations
  --============================================================================
  component siphra_ctrl is
    port
    (
      ---------------------------------------------------------------------------
      -- Clock, active-low async. reset
      ---------------------------------------------------------------------------
      clk_i             : in  std_logic;
      rst_n_a_i         : in  std_logic;
      
      ---------------------------------------------------------------------------
      -- SIPHRA register ports
      ---------------------------------------------------------------------------
      -- Start register operation
      reg_op_start_p_i  : in  std_logic;

      -- Operation to perform: '0' - read / '1' - write
      reg_op_i          : in  std_logic;
      
      -- Register address and data
      reg_addr_i        : in  std_logic_vector( 6 downto 0);
      reg_data_i        : in  std_logic_vector(31 downto 0);
      reg_data_o        : out std_logic_vector(31 downto 0);
      
      -- Register operation done
      reg_op_ready_o    : out std_logic;
      
      ---------------------------------------------------------------------------
      -- SIPHRA SYSCLK port
      ---------------------------------------------------------------------------
      sysclk_o          : out std_logic;

      ---------------------------------------------------------------------------
      -- SIPHRA ADC readout ports
      ---------------------------------------------------------------------------
      txd_i             : in  std_logic;
      adc_value_o       : out std_logic_vector(11 downto 0);
      adc_chan_o        : out std_logic_vector( 4 downto 0);
      adc_trig_type_o   : out std_logic_vector( 1 downto 0);
      adc_trig_flag_o   : out std_logic;
      adc_valid_o       : out std_logic;

      ---------------------------------------------------------------------------
      -- SPI ports
      ---------------------------------------------------------------------------
      spi_cs_n_o        : out std_logic;
      spi_sclk_o        : out std_logic;
      spi_mosi_o        : out std_logic;
      spi_miso_i        : in  std_logic    
    );
  end component siphra_ctrl;

  --============================================================================
  -- Signal declarations
  --============================================================================
  signal state                : t_state;
  
  signal reg_op               : std_logic;
  signal reg_op_start_p       : std_logic;
  signal reg_op_ready         : std_logic;
  signal reg_op_ready_d0      : std_logic;
  signal reg_op_ready_rise_p  : std_logic;
  signal reg_addr             : std_logic_vector( 6 downto 0);
  signal reg_data_in          : std_logic_vector(31 downto 0);
  signal reg_data_out         : std_logic_vector(31 downto 0);
  signal reg_val              : std_logic_vector(31 downto 0);

  signal adc_value            : std_logic_vector(11 downto 0);
  signal adc_trig_type        : std_logic_vector( 1 downto 0);
  signal adc_trig_flag        : std_logic;
  signal adc_chan             : std_logic_vector( 4 downto 0);
  signal adc_valid            : std_logic;
  
  signal addr       : unsigned(f_log2_size(c_msp_mtu)-1 downto 0);
  signal data       : std_logic_vector( 7 downto 0);

begin -- behav

  --============================================================================
  -- FSM for OBC interface
  --============================================================================
  p_fsm : process (clk_i, rst_n_a_i) is
  begin
    if (rst_n_a_i = '0') then
      state <= IDLE;
      addr <= (others => '0');
      data <= (others => '0');
      we_o <= '0';
      data_rdy_p_o <= '0';
      reg_op_start_p <= '0';
      reg_val <= (others => '0');
      reg_op_ready_d0 <= '0';
      reg_op_ready_rise_p <= '0';
      num_bytes_o <= (others => '0');
    elsif rising_edge(clk_i) then
      -- Default states for pulse signals
      data_rdy_p_o <= '0';
      reg_op_start_p <= '0';
      
      -- Current SIPHRA register value
      reg_op_ready_d0 <= reg_op_ready;
      reg_op_ready_rise_p <= (not reg_op_ready_d0) and reg_op_ready;
      if (reg_op_ready_rise_p = '1') then
        reg_val <= reg_data_out;
      end if;
      
      -- Actual FSM logic
      case state is
        when IDLE =>
          addr <= (others => '0');
          we_o <= '0';
          if (en_i /= (en_i'range => '0')) then
            if (data_rdy_p_i = '1') then
              if (en_i = "01") then
                state <= RECEIVE_DATA;
              end if;
            elsif (data_ld_p_i = '1') then
              state <= SEND_DATA;
              if (en_i = "10") and (reg_op_ready = '1') then
                num_bytes_o <= std_logic_vector(to_unsigned(4, num_bytes_o'length));
                data <= reg_val(31 downto 24);
              else
                num_bytes_o <= (others => '0');
              end if;
            end if;
          end if;
        when RECEIVE_DATA =>
          addr <= addr + 1;
          if (addr < unsigned(num_bytes_i)) then
            -- TODO: Implement RECEIVE_DATA for other commands (?)
            if (addr < 4) then
              reg_data_in <= reg_data_in(23 downto 0) & data_i;
            else
              reg_addr <= data_i(7 downto 1);
              reg_op <= data_i(0);
            end if;
          else
            reg_op_start_p <= '1';
            state <= IDLE;
          end if;
        when SEND_DATA =>
          addr <= addr + 1;
          if (addr < 3) then
            data <= reg_val(31 downto 24);
            reg_val <= reg_val(31 downto 8) & x"00";
          else
            data_rdy_p_o <=  '1';
            state <= IDLE;
          end if;
        when others =>
          state <= IDLE;
      end case;
    end if;
  end process p_fsm;
  
  -- Assign data source outputs
  data_o <= data;
  addr_o <= std_logic_vector(addr);
  
  --============================================================================
  -- SIPHRA control component
  --============================================================================
  cmp_siphra_ctrl : siphra_ctrl
    port map
    (
      --------------------------------------------------------------------------
      -- Clock, active-low async. reset
      --------------------------------------------------------------------------
      clk_i             => clk_i,
      rst_n_a_i         => rst_n_a_i,
      
      --------------------------------------------------------------------------
      -- SIPHRA register ports
      --------------------------------------------------------------------------
      -- Start register operation
      reg_op_start_p_i  => reg_op_start_p,

      -- Operation to perform: '0' - read / '1' - write
      reg_op_i          => reg_op,
      
      -- Register address and data
      reg_addr_i        => reg_addr,
      reg_data_i        => reg_data_in,
      reg_data_o        => reg_data_out,
      
      -- Register operation done
      reg_op_ready_o    => reg_op_ready,
      
      --------------------------------------------------------------------------
      -- SIPHRA SYSCLK port
      --------------------------------------------------------------------------
      sysclk_o          => siphra_sysclk_o,

      --------------------------------------------------------------------------
      -- SIPHRA ADC readout ports
      --------------------------------------------------------------------------
      txd_i             => siphra_txd_i,
      adc_value_o       => adc_value,
      adc_chan_o        => adc_chan,
      adc_trig_type_o   => adc_trig_type,
      adc_trig_flag_o   => adc_trig_flag,
      adc_valid_o       => adc_valid,

      --------------------------------------------------------------------------
      -- SPI ports
      --------------------------------------------------------------------------
      spi_cs_n_o        => spi_cs_n_o,
      spi_sclk_o        => spi_sclk_o,
      spi_mosi_o        => spi_mosi_o,
      spi_miso_i        => spi_miso_i
    );

end architecture behav;
