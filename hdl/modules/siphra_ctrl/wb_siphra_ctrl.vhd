--==============================================================================
-- Wishbone slave for SIPHRA controller
--==============================================================================
--
-- author: Theodor Stana (theodor.stana@gmail.com)
--
-- date of creation: 2017-03-03
--
--==============================================================================
-- GNU LESSER GENERAL PUBLIC LICENSE
--==============================================================================
-- This source file is free software; you can redistribute it and/or modify it
-- under the terms of the GNU Lesser General Public License as published by the
-- Free Software Foundation; either version 2.1 of the License, or (at your
-- option) any later version. This source is distributed in the hope that it
-- will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty
-- of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
-- See the GNU Lesser General Public License for more details. You should have
-- received a copy of the GNU Lesser General Public License along with this
-- source; if not, download it from http://www.gnu.org/licenses/lgpl-2.1.html
--==============================================================================
-- last changes:
--    2017-03-03   Theodor Stana     File created
--==============================================================================
-- TODO: -
--==============================================================================

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.wishbone_pkg.all;


entity wb_siphra_ctrl is
  port
  (
    clk_i           : in  std_logic;
    rst_n_a_i       : in  std_logic;
    
    spi_cs_n_o      : out std_logic;
    spi_sclk_o      : out std_logic;
    spi_mosi_o      : out std_logic;
    spi_miso_i      : in  std_logic;
    
    siphra_sysclk_o : out std_logic;
    siphra_txd_i    : in  std_logic;
    
    wbs_i           : in  t_wishbone_slave_in;
    wbs_o           : out t_wishbone_slave_out
  );
end entity wb_siphra_ctrl;


architecture behav of wb_siphra_ctrl is

  --============================================================================
  -- Type declarations
  --============================================================================
  type t_chanrater is array (0 to 15) of std_logic_vector(31 downto 0);
  type t_chan_rate_counter is array (0 to 15) of unsigned(15 downto 0);
  
  --============================================================================
  -- Constant declarations
  --============================================================================
  -- Addresses
  constant c_siphra_ch01rater_ofs : std_logic_vector(8 downto 0) := '0' & x"00";
  constant c_siphra_ch02rater_ofs : std_logic_vector(8 downto 0) := '0' & x"10";
  constant c_siphra_ch03rater_ofs : std_logic_vector(8 downto 0) := '0' & x"20";
  constant c_siphra_ch04rater_ofs : std_logic_vector(8 downto 0) := '0' & x"30";
  constant c_siphra_ch05rater_ofs : std_logic_vector(8 downto 0) := '0' & x"40";
  constant c_siphra_ch06rater_ofs : std_logic_vector(8 downto 0) := '0' & x"50";
  constant c_siphra_ch07rater_ofs : std_logic_vector(8 downto 0) := '0' & x"60";
  constant c_siphra_ch08rater_ofs : std_logic_vector(8 downto 0) := '0' & x"70";
  constant c_siphra_ch09rater_ofs : std_logic_vector(8 downto 0) := '0' & x"80";
  constant c_siphra_ch10rater_ofs : std_logic_vector(8 downto 0) := '0' & x"90";
  constant c_siphra_ch11rater_ofs : std_logic_vector(8 downto 0) := '0' & x"a0";
  constant c_siphra_ch12rater_ofs : std_logic_vector(8 downto 0) := '0' & x"b0";
  constant c_siphra_ch13rater_ofs : std_logic_vector(8 downto 0) := '0' & x"c0";
  constant c_siphra_ch14rater_ofs : std_logic_vector(8 downto 0) := '0' & x"d0";
  constant c_siphra_ch15rater_ofs : std_logic_vector(8 downto 0) := '0' & x"e0";
  constant c_siphra_ch16rater_ofs : std_logic_vector(8 downto 0) := '0' & x"f0";
  constant c_siphra_datar_ofs     : std_logic_vector(8 downto 0) := '1' & x"00";
  constant c_siphra_csr_ofs       : std_logic_vector(8 downto 0) := '1' & x"04";
  constant c_siphra_adcr_ofs      : std_logic_vector(8 downto 0) := '1' & x"08";
  
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
  signal wb_dat_in        : std_logic_vector(c_wishbone_data_width-1 downto 0);
  signal wb_dat_out       : std_logic_vector(c_wishbone_data_width-1 downto 0);
  signal wb_adr           : std_logic_vector(c_wishbone_address_width-1 downto 0);
  signal wb_cyc           : std_logic;
  signal wb_stb           : std_logic;
  signal wb_we            : std_logic;
  signal wb_ack           : std_logic;

  signal csr              : std_logic_vector(c_wishbone_data_width-1 downto 0);
  signal csr_write_p      : std_logic;
  signal datar            : std_logic_vector(c_wishbone_data_width-1 downto 0);
  signal datar_write_p    : std_logic;
  signal adcr             : std_logic_vector(c_wishbone_data_width-1 downto 0);
  signal adcr_read_p      : std_logic;
  
  signal reg_op           : std_logic;
  signal reg_op_start_p   : std_logic;
  signal reg_op_ready     : std_logic;
  signal reg_addr         : std_logic_vector( 6 downto 0);
  signal reg_data_in      : std_logic_vector(31 downto 0);
  signal reg_data_out     : std_logic_vector(31 downto 0);
  
  signal adc_value        : std_logic_vector(11 downto 0);
  signal adc_trig_type    : std_logic_vector( 1 downto 0);
  signal adc_chan         : std_logic_vector( 4 downto 0);
  signal adc_valid        : std_logic;
  signal adc_valid_d0     : std_logic;
  signal adc_valid_p      : std_logic;
  
  signal chanrater_read_p   : std_logic_vector(15 downto 0);
  signal chanrater          : t_chanrater;
  signal chan_rate_counter  : t_chan_rate_counter;
  
  signal counter_1sec       : unsigned(26 downto 0);
  signal tick_1sec_p        : std_logic;
  
--==============================================================================
--  architecture begin
--==============================================================================
begin

  --============================================================================
  -- I/O logic
  --============================================================================
  wb_adr <= wbs_i.adr;
  wb_cyc <= wbs_i.cyc;
  wb_stb <= wbs_i.stb;
  wb_we  <= wbs_i.we;
  wb_dat_in <= wbs_i.dat;
  
  wbs_o.ack <= wb_ack;
  wbs_o.dat <= wb_dat_out;
  wbs_o.err <= '0';
  wbs_o.rty <= '0';
  wbs_o.stall <= '0';
  wbs_o.int <= '0';

  --============================================================================
  -- Wishbone controller
  --============================================================================
  p_wb_regs : process (clk_i, rst_n_a_i) is
  begin
    if (rst_n_a_i = '0') then
      wb_ack <= '0';
      wb_dat_out <= (others => '0');
      csr_write_p <= '0';
      datar_write_p <= '0';
      adcr_read_p <= '0';
      chanrater_read_p <= (others => '0');
      
    elsif rising_edge(clk_i) then
      adcr_read_p <= '0';
      chanrater_read_p <= (others => '0');
      if (wb_cyc = '1') and (wb_stb = '1') then
        wb_ack <= '1';
        if (wb_we = '1') then
          case wb_adr(8 downto 0) is
            when c_siphra_datar_ofs =>
              datar_write_p <= '1';
            when c_siphra_csr_ofs =>
              csr_write_p <= '1';
            when others =>
              null;
          end case;
        else
          case wb_adr(8 downto 0) is
            when c_siphra_datar_ofs =>
              wb_dat_out <= datar;
            when c_siphra_csr_ofs =>
              wb_dat_out <= csr;
            when c_siphra_adcr_ofs =>
              wb_dat_out <= adcr;
              adcr_read_p <= '1';
            when c_siphra_ch01rater_ofs =>
              wb_dat_out <= chanrater(0);
              chanrater_read_p(0) <= '1';
            when c_siphra_ch02rater_ofs =>
              wb_dat_out <= chanrater(1);
              chanrater_read_p(1) <= '1';
            when c_siphra_ch03rater_ofs =>
              wb_dat_out <= chanrater(2);
              chanrater_read_p(2) <= '1';
            when c_siphra_ch04rater_ofs =>
              wb_dat_out <= chanrater(3);
              chanrater_read_p(3) <= '1';
            when c_siphra_ch05rater_ofs =>
              wb_dat_out <= chanrater(4);
              chanrater_read_p(4) <= '1';
            when c_siphra_ch06rater_ofs =>
              wb_dat_out <= chanrater(5);
              chanrater_read_p(5) <= '1';
            when c_siphra_ch07rater_ofs =>
              wb_dat_out <= chanrater(6);
              chanrater_read_p(6) <= '1';
            when c_siphra_ch08rater_ofs =>
              wb_dat_out <= chanrater(7);
              chanrater_read_p(7) <= '1';
            when c_siphra_ch09rater_ofs =>
              wb_dat_out <= chanrater(8);
              chanrater_read_p(8) <= '1';
            when c_siphra_ch10rater_ofs =>
              wb_dat_out <= chanrater(9);
              chanrater_read_p(9) <= '1';
            when c_siphra_ch11rater_ofs =>
              wb_dat_out <= chanrater(10);
              chanrater_read_p(10) <= '1';
            when c_siphra_ch12rater_ofs =>
              wb_dat_out <= chanrater(11);
              chanrater_read_p(11) <= '1';
            when c_siphra_ch13rater_ofs =>
              wb_dat_out <= chanrater(12);
              chanrater_read_p(12) <= '1';
            when c_siphra_ch14rater_ofs =>
              wb_dat_out <= chanrater(13);
              chanrater_read_p(13) <= '1';
            when c_siphra_ch15rater_ofs =>
              wb_dat_out <= chanrater(14);
              chanrater_read_p(14) <= '1';
            when c_siphra_ch16rater_ofs =>
              wb_dat_out <= chanrater(15);
              chanrater_read_p(15) <= '1';
            when others =>
              wb_dat_out <= (others => '0');
          end case;
        end if;
      end if;
      
      if (wb_ack = '1') then
        wb_ack <= '0';
        csr_write_p <= '0';
        datar_write_p <= '0';
      end if;
    end if;
  end process p_wb_regs;

  --============================================================================
  -- Wishbone-mapped registers
  --============================================================================
  -- DATAR
  datar <= reg_data_out;
  
  p_datar : process (clk_i, rst_n_a_i) is
  begin
    if (rst_n_a_i = '0') then
      reg_data_in  <= (others => '0');
    elsif rising_edge(clk_i) then
      if (datar_write_p = '1') then
        reg_data_in <= wb_dat_in;
      end if;
    end if;
  end process p_datar;

  -- CSR
  csr(0)            <= reg_op;
  csr(7 downto 1)   <= reg_addr;
  csr(15 downto 8)  <= (others => '0');
  csr(16)           <= reg_op_ready;
  csr(31 downto 17) <= (others => '0');

  p_csr : process(clk_i, rst_n_a_i) is
  begin
    if (rst_n_a_i = '0') then
      reg_op <= '0';
      reg_addr <= (others => '0');
    elsif rising_edge(clk_i) then
      if (csr_write_p = '1') then
        reg_op <= wb_dat_in(0);
        reg_addr <= wb_dat_in(7 downto 1);
      end if;
    end if;
  end process p_csr;
  
  -- ADCR
  p_adcr : process(clk_i, rst_n_a_i) is
  begin
    if (rst_n_a_i = '0') then
      adc_valid_d0 <= '0';
      adc_valid_p <= '0';
      adcr <= (others => '0');
    elsif rising_edge(clk_i) then
    
      adc_valid_d0 <= adc_valid;
      adc_valid_p <= (not adc_valid_d0) and adc_valid;
      
      if (adc_valid_p = '1') then
        adcr(11 downto  0) <= adc_value;
        adcr(15 downto 12) <= (others => '0');
        adcr(20 downto 16) <= adc_chan;
        adcr(22 downto 21) <= adc_trig_type;
        adcr(30 downto 17) <= (others => '0');
        adcr(31) <= '1';
      end if;
      
      if (adcr_read_p = '1') then
        adcr(31) <= '0';
      end if;
    end if;
  end process p_adcr;
  
  -- CHxxRATER
gen_chanrater : for i in 0 to 15 generate
  p_chanrater : process (clk_i, rst_n_a_i) is
  begin
    if (rst_n_a_i = '0') then
      chanrater(i) <= (others => '0');
    elsif rising_edge(clk_i) then
      if (chanrater_read_p(i) = '1') then
        chanrater(i) <= (others => '0');
      elsif (tick_1sec_p = '1') then
        chanrater(i) <= x"0000" & std_logic_vector(chan_rate_counter(i));
      end if;
    end if;
  end process;
end generate;
  
  --============================================================================
  -- SIPHRA controller core
  --============================================================================
  -- Start register operation on CSR write
  -- (+1 clock cycle to forward 'reg_addr' written on 'csr_write_p' to siphra_ctrl)
  p_reg_op_start : process (clk_i, rst_n_a_i) is
  begin
    if (rst_n_a_i = '0') then
      reg_op_start_p <= '0';
    elsif rising_edge(clk_i) then
      reg_op_start_p <= csr_write_p;
    end if;
  end process p_reg_op_start;
  
  -- Instantiate core
  cmp_siphra_ctrl : siphra_ctrl
    port map
    (
      ---------------------------------------------------------------------------
      -- Clock, active-low async. reset
      ---------------------------------------------------------------------------
      clk_i             => clk_i,
      rst_n_a_i         => rst_n_a_i,
      
      ---------------------------------------------------------------------------
      -- SIPHRA register ports
      ---------------------------------------------------------------------------
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
      
      ---------------------------------------------------------------------------
      -- SIPHRA SYSCLK port
      ---------------------------------------------------------------------------
      sysclk_o          => siphra_sysclk_o,

      ---------------------------------------------------------------------------
      -- SIPHRA ADC readout ports
      ---------------------------------------------------------------------------
      txd_i             => siphra_txd_i,
      adc_value_o       => adc_value,
      adc_chan_o        => adc_chan,
      adc_trig_type_o   => adc_trig_type,
      adc_valid_o       => adc_valid,

      ---------------------------------------------------------------------------
      -- SPI ports
      ---------------------------------------------------------------------------
      spi_cs_n_o        => spi_cs_n_o,
      spi_sclk_o        => spi_sclk_o,
      spi_mosi_o        => spi_mosi_o,
      spi_miso_i        => spi_miso_i
    );

  --============================================================================
  -- Channel rate counters
  --============================================================================
  p_counter_1sec : process (clk_i, rst_n_a_i) is
  begin
    if (rst_n_a_i = '0') then
      counter_1sec <= (others => '0');
      tick_1sec_p <= '0';
    elsif rising_edge(clk_i) then
      counter_1sec <= counter_1sec + 1;
      tick_1sec_p <= '0';
      if (counter_1sec = 99_999_999) then
        counter_1sec <= (others => '0');
        tick_1sec_p <= '1';
      end if;
    end if;
  end process;
  
gen_rate_counters : for i in 0 to 15 generate
  p_chan_rate_counter : process (clk_i, rst_n_a_i) is
  begin
    if (rst_n_a_i = '0') then
      chan_rate_counter(i) <= (others => '0');
    elsif rising_edge(clk_i) then
      if (tick_1sec_p = '1') then
        chan_rate_counter(i) <= (others => '0');
      elsif (adc_valid_p = '1') and (to_integer(unsigned(adc_chan)) = i+1) then
        chan_rate_counter(i) <= chan_rate_counter(i) + 1;
      end if;
    end if;
  end process;
end generate;
  
end architecture behav;
--==============================================================================
--  architecture end
--==============================================================================
