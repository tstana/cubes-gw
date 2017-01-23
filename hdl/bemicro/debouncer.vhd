library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.gencores_pkg.all;


entity debouncer is
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
end entity debouncer;


architecture behav of debouncer is

  --===============================================================================================
  -- Type declarations
  --===============================================================================================
  type t_btn_debounce_count is array (g_nr_buttons-1 downto 0) of
                                  unsigned(log2_ceil(g_debounce_cycles)-1 downto 0);

  --===============================================================================================
  -- Signal declarations
  --===============================================================================================
  signal btn_debounce_count : t_btn_debounce_count;
  signal btn_debounce_en    : std_logic_vector(g_nr_buttons-1 downto 0);
  signal btn_n              : std_logic_vector(g_nr_buttons-1 downto 0);
  signal btn_n_d0           : std_logic_vector(g_nr_buttons-1 downto 0);

  
begin

gen_btn_debounce : for i in g_nr_buttons-1 downto 0 generate

  cmp_btn_sync : gc_sync_ffs
    port map
    (
      clk_i     => clk_i,
      rst_n_i   => '1',
      data_i    => btn_n_i(i),
      synced_o  => btn_n(i)
    );
  
  p_btn_debounce : process (clk_i) is
  begin
    if rising_edge(clk_i) then
      btn_n_d0(i) <= btn_n(i);
      if ((btn_n(i) xor btn_n_d0(i)) = '1') then
        btn_debounce_en(i) <= '1';
      end if;
      
      if (btn_debounce_en(i) = '1') then
        btn_debounce_count(i) <= btn_debounce_count(i) + 1;
        if (btn_debounce_count(i) = g_debounce_cycles) then
          btn_o(i) <= not btn_n(i);
          btn_debounce_en(i) <= '0';
        end if;
      end if;
    end if;
  end process;
  
end generate gen_btn_debounce;

end architecture;