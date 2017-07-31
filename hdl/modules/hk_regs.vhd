library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.genram_pkg.all;
use work.msp_pkg.all;


entity hk_regs is
  port
  (
    clk_i               : in  std_logic;
    rst_n_a_i           : in  std_logic;
    
    -- Number of bytes available on data request
    num_bytes_o         : out std_logic_vector(c_msp_dl_width-1 downto 0);
    
    -- Interface to MSP data buffer
    data_ld_p_i         : in  std_logic;
    we_o                : out std_logic;
    addr_o              : out std_logic_vector(f_log2_size(c_msp_mtu)-1 downto 0);
    data_o              : out std_logic_vector(7 downto 0);
    data_rdy_p_o        : out std_logic;
    
    -- Interface to modules providing housekeeping
    gw_vers_i           : in  std_logic_vector(15 downto 0);
    leds_i              : in  std_logic_vector( 7 downto 0)
  );
end entity hk_regs;

architecture behav of hk_regs is

  type t_state is (
    IDLE,
    SEND_DATA
  );
  
  constant c_num_hk_bytes : natural := 3;
  
  signal state      : t_state;
  
  signal addr       : unsigned(f_log2_size(c_msp_mtu)-1 downto 0);
  signal data       : std_logic_vector( 7 downto 0);
  
begin

  p_send_data : process (clk_i, rst_n_a_i) is
  begin
    if (rst_n_a_i = '0') then
      state <= IDLE;
      addr <= (others => '0');
      data <= (others => '0');
      we_o <= '0';
      data_rdy_p_o <= '0';
    elsif rising_edge(clk_i) then
      data_rdy_p_o <= '0';
      case state is
        when IDLE =>
          addr <= (others => '0');
          we_o <= '0';
          if (data_ld_p_i = '1') then
            state <= SEND_DATA;
            data <= gw_vers_i(15 downto 8);
            we_o <= '1';
          end if;
        when SEND_DATA =>
          addr <= addr + 1;
          if (addr = 0) then
            data <= gw_vers_i(7 downto 0);
          elsif (addr = 1) then
            data <= leds_i;
          else
            data_rdy_p_o <= '1';
            we_o <= '0';
            state <= IDLE;
          end if;
        when others =>
          state <= IDLE;
      end case;
    end if;
  end process p_send_data;

  data_o <= data;
  addr_o <= std_logic_vector(addr);
  num_bytes_o <= std_logic_vector(to_unsigned(c_num_hk_bytes, num_bytes_o'length));
  
end architecture behav;