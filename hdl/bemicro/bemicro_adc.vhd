library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.gencores_pkg.all;
use work.genram_pkg.all;


entity bemicro_adc is
  generic
  (
    g_nr_buttons : natural := 2
  );
  port
  (
  
  
    dbg_o : out std_logic;
    
    
    
    clk_50meg_i : in  std_logic;
    btn_n_i     : in  std_logic_vector(g_nr_buttons-1 downto 0);
    led_n_o     : out std_logic_vector(7 downto 0)
  );
end entity bemicro_adc;
  
  
architecture struct of bemicro_adc is

  --===============================================================================================
  -- Type declarations
  --===============================================================================================

  --===============================================================================================
  -- Constant declarations
  --===============================================================================================
  constant c_ram_width : natural := 16;
  constant c_ram_depth : natural := 1024;
  
  --===============================================================================================
  -- Component declarations
  --===============================================================================================
  -- Altera Modular ADC Control
  component altera_adc_control is
	port (
		adc_pll_clock_clk      : in  std_logic                     := '0';             --  adc_pll_clock.clk
		adc_pll_locked_export  : in  std_logic                     := '0';             -- main_pll_locked.export
		clock_clk              : in  std_logic                     := '0';             --          clock.clk
		command_valid          : in  std_logic                     := '0';             --        command.valid
		command_channel        : in  std_logic_vector(4 downto 0)  := (others => '0'); --               .channel
		command_startofpacket  : in  std_logic                     := '0';             --               .startofpacket
		command_endofpacket    : in  std_logic                     := '0';             --               .endofpacket
		command_ready          : out std_logic;                                        --               .ready
		reset_sink_reset_n     : in  std_logic                     := '0';             --     reset_sink.reset_n
		response_valid         : out std_logic;                                        --       response.valid
		response_channel       : out std_logic_vector(4 downto 0);                     --               .channel
		response_data          : out std_logic_vector(11 downto 0);                    --               .data
		response_startofpacket : out std_logic;                                        --               .startofpacket
		response_endofpacket   : out std_logic                                         --               .endofpacket
	);
  end component altera_adc_control;
  
  -- Altera PLLs
  component altera_cascade_pll
    PORT
    (
      areset		: IN STD_LOGIC  := '0';
      inclk0		: IN STD_LOGIC  := '0';
      c0		: OUT STD_LOGIC ;
      locked		: OUT STD_LOGIC 
    );
  end component;

  component altera_main_pll
    PORT
    (
      areset		: IN STD_LOGIC  := '0';
      inclk0		: IN STD_LOGIC  := '0';
      c0		: OUT STD_LOGIC ;
      c1		: OUT STD_LOGIC ;
      locked		: OUT STD_LOGIC 
    );
  end component;

  -- ADC control
  component adc_ctrl is
  port
  (
    clk_i               : in  std_logic;
    
    rst_n_a_i           : in  std_logic;
    
    channel_sel_i       : in  std_logic;
    
    cmd_ready_i         : in  std_logic;
    cmd_valid_o         : out std_logic;
    cmd_channel_o       : out std_logic_vector(4 downto 0)
  );
  end component adc_ctrl;

  -- RAM control
  component ram_ctrl is
    generic
    (
      g_adc_nr_bits : natural := 12;
      
      g_ram_width   : natural := 32;
      g_ram_depth   : natural := 1024
    );
    port
    (
    
    dbg_o : out std_logic;
    
    
    
      clk_i               : in  std_logic;

      rst_n_a_i           : in  std_logic;

      adc_result_valid_i  : in  std_logic;
      adc_channel_i       : in  std_logic_vector(4 downto 0);
      adc_result_i        : in  std_logic_vector(g_adc_nr_bits-1 downto 0);

      ram_addr_o          : out std_logic_vector(f_log2_size(g_ram_depth)-1 downto 0);

      ram_we_o            : out std_logic;
      ram_data_o          : out std_logic_vector(g_ram_width-1 downto 0);

      ram_data_i          : in  std_logic_vector(g_ram_width-1 downto 0)
    );
  end component ram_ctrl;

  component led_pwm is
    port
    (
      clk_i               : in  std_logic;

      rst_n_a_i           : in  std_logic;

      adc_result_valid_i  : in  std_logic;
      adc_channel_i       : in  std_logic_vector( 4 downto 0);
      adc_result_i        : in  std_logic_vector(11 downto 0);

      chan_display_sel_i  : in  std_logic;

      led_o               : out std_logic_vector( 7 downto 0)
    );
  end component led_pwm;
  
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
  signal clk_cascade            : std_logic;
  signal clk_100meg             : std_logic;
  signal clk_adc                : std_logic;
  
  signal rst_n                  : std_logic;
  signal rst                    : std_logic;
  
  signal cascade_pll_locked     : std_logic;
  signal cascade_pll_not_locked : std_logic;
  signal main_pll_locked        : std_logic;
  
  signal adc_cmd_valid          : std_logic;
  signal adc_cmd_channel        : std_logic_vector( 4 downto 0);
  signal adc_cmd_startofpacket  : std_logic;
  signal adc_cmd_endofpacket    : std_logic;
  signal adc_cmd_ready          : std_logic;
  signal adc_resp_valid         : std_logic;
  signal adc_resp_channel       : std_logic_vector( 4 downto 0);
  signal adc_resp_data          : std_logic_vector(11 downto 0);
  
  signal ram_addr               : std_logic_vector(f_log2_size(c_ram_depth)-1 downto 0);
  signal ram_data_in            : std_logic_vector(c_ram_width-1 downto 0);
  signal ram_data_out           : std_logic_vector(c_ram_width-1 downto 0);
  signal ram_We                 : std_logic;
  
  signal led                    : std_logic_vector(7 downto 0);
  signal btn                    : std_logic_vector(g_nr_buttons-1 downto 0);
  signal chan_sel_btn           : std_logic;
  signal chan_sel_btn_d0        : std_logic;
  signal chan_sel               : std_logic;

  signal stim                   : unsigned(11 downto 0);
  signal d                      : unsigned(16 downto 0);
  attribute keep : boolean;
  attribute keep of ram_data_out : signal is true;
  attribute keep of clk_100meg : signal is true;
  signal tmp : unsigned(27 downto 0);
  signal tmp2 : unsigned(27 downto 0);
  signal ledt : unsigned(7 downto 0);
  signal ledt2 : unsigned(7 downto 0);
  
begin

  cmp_btn_debounce : debouncer
    generic map
    (
      g_nr_buttons => g_nr_buttons
    )
    port map
    (
      clk_i   => clk_100meg,
      btn_n_i => btn_n_i,
      btn_o   => btn
    );
  
  rst           <= btn(0);
  chan_sel_btn  <= btn(1);
  
  rst_n <= not rst;
  
  cmp_adc_pll : altera_cascade_pll
    port map
    (
      areset => '0',
      inclk0 => clk_50meg_i,
      c0     => clk_cascade,
      locked => cascade_pll_locked
    );
    
  cascade_pll_not_locked <= not cascade_pll_locked;
  
  cmp_main_pll : altera_main_pll
    port map
    (
      areset => cascade_pll_not_locked,
      inclk0 => clk_cascade,
      c0     => clk_adc,
      c1     => clk_100meg,
      locked => main_pll_locked
    );

  cmp_altera_adc_control : altera_adc_control
    port map
    (
      adc_pll_clock_clk      => clk_adc,
      adc_pll_locked_export  => main_pll_locked,
      clock_clk              => clk_100meg,
      command_valid          => adc_cmd_valid,
      command_channel        => adc_cmd_channel,
      command_startofpacket  => '0',
      command_endofpacket    => '0',
      command_ready          => adc_cmd_ready,
      reset_sink_reset_n     => '1',
      response_valid         => adc_resp_valid,
      response_channel       => adc_resp_channel,
      response_data          => adc_resp_data,
      response_startofpacket => open,
      response_endofpacket   => open
    );

  p_chan_sel : process (clk_100meg, rst_n) is
  begin
    if (rst_n = '0') then
      chan_sel <= '0';
      chan_sel_btn_d0 <= '0';
    elsif rising_edge(clk_100meg) then
      chan_sel_btn_d0 <= chan_sel_btn;
      if (chan_sel_btn = '1') and (chan_sel_btn_d0 = '0') then
        chan_sel <= not chan_sel;
      end if;
    end if;
  end process;
  
  cmp_adc_ctrl : adc_ctrl
    port map
    (
      clk_i               => clk_100meg,

      rst_n_a_i           => rst_n,
      
      channel_sel_i       => chan_sel,
    
      cmd_ready_i         => adc_cmd_ready,
      cmd_valid_o         => adc_cmd_valid,
      cmd_channel_o       => adc_cmd_channel
    );

--  p_stim : process (clk_100meg) is
--  begin
--    if rising_edge(clk_100meg) then
--      d <= d + 1;
--      if (d = 122_070) then
--        d <= (others => '0');
--        stim <= stim + 1;
--      end if;
--    end if;
--  end process;
--
--  adc_resp_valid <= '1';
--  adc_resp_data <= std_logic_vector(stim);
  
  cmp_led_pwm : led_pwm
    port map
    (
      clk_i               => clk_100meg,

      rst_n_a_i           => rst_n,

      chan_display_sel_i  => chan_sel,
      adc_result_valid_i  => adc_resp_valid,
      adc_channel_i       => adc_resp_channel,
      adc_result_i        => adc_resp_data,

      led_o               => led
    );

----  procesS(clk_100meg, rst_n)
----  begin
----    if (rst_n = '0') then
----      tmp <= (others => '0');
----      ledt <= (others => '0');
----      ledt2 <= (others => '0');
----    elsif rising_edge(clk_100meg) then
----      if (adc_resp_valid = '1') then
----        tmp <= tmp+1;
----        if (tmp = 999_999) then
----          tmp <= (others => '0');
----          ledt <= ledt + 1;
----        end if;
----        tmp2 <= tmp2 + 1;
----        if (tmp2 = 249_999) then
----          tmp2 <= (others => '0');
----          ledt2 <= ledt2 + 1;
----        end if;
----      end if;
----    end if;
----  end process;

  led_n_o(6 downto 0) <= not led(6 downto 0);
  led_n_o(7) <= not chan_sel;

  cmp_ram_ctrl : ram_ctrl
    generic map
    (
      g_adc_nr_bits => 12,

      g_ram_width   => c_ram_width,
      g_ram_depth   => c_ram_depth
    )
    port map
    (
      clk_i               => clk_100meg,
        
      rst_n_a_i           => rst_n,

      adc_result_valid_i  => adc_resp_valid,
      adc_channel_i       => adc_resp_channel,
      adc_result_i        => adc_resp_data,

      ram_addr_o          => ram_addr,

      ram_we_o            => ram_we,
      ram_data_o          => ram_data_in,

      dbg_o => dbg_o, 
      ram_data_i          => ram_data_out
    );

  cmp_ram : generic_spram
    generic map (
      g_data_width               => c_ram_width,
      g_size                     => c_ram_depth
    )
    port map (
      rst_n_i   => rst_n,
      clk_i     => clk_100meg,
      we_i      => ram_we,
      a_i       => ram_addr,
      d_i       => ram_data_in,
      q_o       => ram_data_out
    );


end architecture struct;