library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;


entity bemicro_adc is
  port
  (
    clk_50meg_i : in  std_logic;
    led_n_o     : out std_logic_vector(7 downto 0)
  );
end entity bemicro_adc;
  
  
architecture struct of bemicro_adc is

  constant c_ram_width : natural := 16;
  constant c_ram_depth : natural := 128;
  
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
    clk_i               : in std_logic;
    
    cmd_ready_i         : in  std_logic;
    cmd_valid_o         : out std_logic;
    cmd_channel_o       : out std_logic_vector(4 downto 0)
  );
  end component adc_ctrl;

--  -- RAM control
--  component ram_ctrl is
--    generic
--    (
--      g_adc_nr_bits : natural := 12;
--      
--      g_ram_width   : natural := 32;
--      g_ram_depth   : natural := 1024
--    );
--    port
--    (
--      clk_i             : in  std_logic;
--  
--      adc_data_valid_i  : in  std_logic;
--      adc_channel_i     : in  std_logic_vector(4 downto 0);
--      adc_result_i      : in  std_logic_vector(g_adc_nr_bits-1 downto 0);
--  
--      ram_addr_o        : out std_logic_vector(log2(g_ram_depth)-1 downto 0);
--  
--      ram_wr_o          : out std_logic;
--      ram_data_o        : out std_logic_vector(g_ram_width-1 downto 0);
--  
--      ram_rd_o          : out std_logic;
--      ram_data_i        : in  std_logic_vector(g_ram_width-1 downto 0)
--    );
--  end component ram_ctrl;

  component led_pwm is
    port
    (
      clk_i               : in  std_logic;

      rst_a_i             : in  std_logic;

      adc_result_valid_i  : in  std_logic;
      adc_result_i        : in  std_logic_vector(11 downto 0);

      led_o               : out std_logic_vector( 7 downto 0)
    );
  end component led_pwm;

  --===============================================================================================
  -- Signal declarations
  --===============================================================================================
  signal clk_cascade            : std_logic;
  signal clk_100meg             : std_logic;
  signal clk_adc                : std_logic;
  
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
  
--  signal ram_addr : std_logic_vector(log2(c_ram_depth)-1 downto 0);
--  signal ram_data : std_logic_vector(c_ram_width-1 downto 0);
  
  signal led : std_logic_vector(7 downto 0);
  
  signal stim : unsigned(11 downto 0);
  signal d : unsigned(16 downto 0);
  
begin

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

  cmp_adc_ctrl : adc_ctrl
    port map
    (
      clk_i               => clk_100meg,

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

      rst_a_i             => '0',

      adc_result_valid_i  => adc_resp_valid,
      adc_result_i        => adc_resp_data,

      led_o               => led
    );

  led_n_o <= not led;

--  cmp_ram_ctrl : ram_ctrl
--    generic map
--    (
--      g_adc_nr_bits => 12,
--
--      g_ram_width   => c_ram_width;
--      g_ram_depth   => c_ram_depth;
--    )
--    port map
--    (
--      clk_i             => clk_50meg_i,
--
--      adc_data_valid_i  => adc_resp_valid,
--      adc_channel_i     => adc_resp_channel,
--      adc_result_i      => adc_resp_data,
--
--      ram_addr_o        => ram_addr,
--
--      ram_wr_o          => ram_wr_en,
--      ram_data_o        => ram_data,
--
--      ram_rd_o          => open,
--      ram_data_i        => (others => '0'),
--    );

end architecture struct;