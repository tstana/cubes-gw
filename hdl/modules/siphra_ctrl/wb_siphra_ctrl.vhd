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
    
    siphra_sysclk_o : out  std_logic;
    
    wbs_i           : in  t_wishbone_slave_in;
    wbs_o           : out t_wishbone_slave_out
  );
end entity wb_siphra_ctrl;


architecture behav of wb_siphra_ctrl is

  --============================================================================
  -- Constant declarations
  --============================================================================
  -- Addresses
  constant c_siphra_datar_ofs   : std_logic_vector(1 downto 0) := "00";
  constant c_siphra_csr_ofs     : std_logic_vector(1 downto 0) := "01";

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
  
  signal reg_op           : std_logic;
  signal reg_op_start_p   : std_logic;
  signal reg_op_ready     : std_logic;
  signal reg_addr         : std_logic_vector( 6 downto 0);
  signal reg_data_in      : std_logic_vector(31 downto 0);
  signal reg_data_out     : std_logic_vector(31 downto 0);
  
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
      
    elsif rising_edge(clk_i) then
      if (wb_cyc = '1') and (wb_stb = '1') then
        wb_ack <= '1';
        if (wb_we = '1') then
          case wb_adr(3 downto 2) is
            when c_siphra_datar_ofs =>
              datar_write_p <= '1';
            when c_siphra_csr_ofs =>
              csr_write_p <= '1';
            when others =>
              null;
          end case;
        else
          case wb_adr(3 downto 2) is
            when c_siphra_datar_ofs =>
              wb_dat_out <= datar;
            when c_siphra_csr_ofs =>
              wb_dat_out <= csr;
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
      -- SPI ports
      ---------------------------------------------------------------------------
      spi_cs_n_o        => spi_cs_n_o,
      spi_sclk_o        => spi_sclk_o,
      spi_mosi_o        => spi_mosi_o,
      spi_miso_i        => spi_miso_i
    );


end architecture behav;
--==============================================================================
--  architecture end
--==============================================================================
