library ieee;
use ieee.std_logic_1164.all;

entity plls is
  port
  (
    rst_a_i           : in  std_logic;
    clk_50meg_i       : in  std_logic;

    clk_adc_o         : out std_logic;
    clk_100meg_o      : out std_logic;

    main_pll_locked_o : out std_logic
  );
end entity plls;


architecture struct of plls is

  --===============================================================================================
  -- Component declarations
  --===============================================================================================
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

  --===============================================================================================
  -- Signal declarations
  --===============================================================================================
  signal clk_cascade            : std_logic;
  signal clk_adc                : std_logic;
  signal clk_100meg             : std_logic;

  signal cascade_pll_locked     : std_logic;
  signal cascade_pll_not_locked : std_logic;
  signal main_pll_locked        : std_logic;

begin

  -- Instantiate PLLs
  cmp_cascade_pll : altera_cascade_pll
    port map
    (
      areset => rst_a_i,
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

  -- Assign outputs
  clk_adc_o         <= clk_adc;
  clk_100meg_o      <= clk_100meg;
  main_pll_locked_o <= main_pll_locked;

end architecture;