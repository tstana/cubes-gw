--==============================================================================
-- IDE3380 (SIPHRA) SPI Controller Core
--==============================================================================
--
-- author: Theodor Stana (theodor.stana@gmail.com)
--
-- date of creation: 2017-03-02
--
--==============================================================================
-- Description:
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
--    2017-03-02   Theodor Stana     File created
--==============================================================================
-- TODO: -
--==============================================================================

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.gencores_pkg.all;
use work.siphra_pkg.all;


entity siphra_ctrl is
  generic
  (
    g_num_addr_bits : natural :=  7;
    g_num_data_bits : natural := 26
  );
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
    reg_addr_i        : in  std_logic_vector(g_num_addr_bits-1 downto 0);
    reg_data_i        : in  std_logic_vector(g_num_data_bits-1 downto 0);
    
    -- Register operation done
    reg_op_ready_o    : out std_logic;
    
    ---------------------------------------------------------------------------
    -- SPI ports
    ---------------------------------------------------------------------------
    spi_cs_n_o        : out std_logic;
    spi_sclk_o        : out std_logic;
    spi_mosi_o        : out std_logic;
    spi_miso_i        : in  std_logic    
  );
end entity siphra_ctrl;


architecture behav of siphra_ctrl is

  --============================================================================
  -- Type declarations
  --============================================================================
  type t_state is (
    IDLE,
    SHIFT_DATA,
    SHIFT_OP,
    SHIFT_ADDR,
    SPI_START_TRANSFER,
    SPI_TRANSFER
  );

  --============================================================================
  -- Constant declarations
  --============================================================================
  -- Add the R/W bit to the address width
  -- ** for use in adding to number of bits to send via SPI without incurring
  --    an extra adder.
  constant c_addr_op_width    : natural := g_num_addr_bits + 1;
  constant c_addr_bit_shifts  : natural := g_num_addr_bits - 1;

  --============================================================================
  -- Component declarations
  --============================================================================
  ------------------------------------------------------------------------------
  -- SPI Master
  ------------------------------------------------------------------------------
  component spi_master is
    generic(
      -- clock division ratio (SCLK = clk_sys_i / (2 ** g_div_ratio_log2).
      g_div_ratio_log2 : integer := 2;

      -- data bits generic or from port?
      g_data_bits_generic : boolean := true;

      -- number of data bits per transfer; if from port, set to max. value expected on port
      g_num_data_bits  : integer := 2);
    port (
      clk_sys_i  : in std_logic;
      rst_n_i    : in std_logic;

      -- state of the Chip select line (1 = CS active). External control
      -- allows for multi-transfer commands (SPI master itself does not
      -- control the state of spi_cs_n_o)
      cs_i       : in std_logic;

      -- 1: start next transfer (using CPOL, DATA and SEL from the inputs below)
      start_p_i  : in  std_logic;

      -- Clock polarity: 1: slave clocks in the data on rising SCLK edge, 0: ...
      -- on falling SCLK edge
      cpol_i     : in  std_logic;

      -- Number of bits to send
      data_len_i : in std_logic_vector(log2_ceil(g_num_data_bits)-1 downto 0);

      -- TX Data input 
      data_i     : in  std_logic_vector(g_num_data_bits - 1 downto 0);

      -- 1: data_o contains the result of last read operation. Core is ready to initiate
      -- another transfer.
      ready_o    : out std_logic;

      -- data read from selected slave, valid when ready_o == 1.
      data_o     : out std_logic_vector(g_num_data_bits - 1 downto 0);

      -- these are obvious
      spi_cs_n_o : out std_logic;
      spi_sclk_o : out std_logic;
      spi_mosi_o : out std_logic;
      spi_miso_i : in  std_logic
      );
  end component spi_master;

  --============================================================================
  -- Signal declarations
  --============================================================================
  signal spi_cs         : std_logic;
  
  signal state          : t_state;
  signal data_sreg      : std_logic_vector(g_num_data_bits-1 downto 0);
  signal addr_sreg      : std_logic_vector(g_num_addr_bits-1 downto 0);
  signal shift_count    : unsigned(log2_ceil(g_num_data_bits)-1 downto 0);
  signal bits_to_send   : unsigned(log2_ceil(g_num_data_bits)-1 downto 0);
  signal reg_op_ready   : std_logic;

  signal spi_start_p    : std_logic;
  signal spi_data_in    : std_logic_vector(g_num_data_bits-1 downto 0);
  signal spi_data_out   : std_logic_vector(g_num_data_bits-1 downto 0);
  signal spi_ready      : std_logic;
  
--==============================================================================
--  architecture begin
--==============================================================================
begin

  --============================================================================
  -- FSM to handle variable-length registers on ASIC end
  --============================================================================
  p_fsm : process (clk_i, rst_n_a_i) is
  begin
    if (rst_n_a_i = '0') then
      state <= IDLE;
      spi_cs <= '0';
      data_sreg <= (others => '0');
      addr_sreg <= (others => '0');
      shift_count <= (others => '0');
      bits_to_send <= (others => '0');
      spi_data_out <= (others => '0');
      spi_start_p  <= '0';
      spi_cs       <= '0';
      
    elsif rising_edge(clk_i) then
      
      case state is
      
        when IDLE =>
          spi_cs <= '0';
          spi_start_p <= '0';
          reg_op_ready <= '1';
          if (reg_op_start_p_i = '1') then
            reg_op_ready <= '0';
            shift_count <= to_unsigned(f_siphra_reg_width(reg_addr_i), shift_count'length);
            data_sreg <= reg_data_i;
            addr_sreg <= reg_addr_i;
            state <= SHIFT_DATA;
          end if;
          
        when SHIFT_DATA =>
          spi_data_out <= data_sreg(0) & spi_data_out(spi_data_out'high downto 1);
          data_sreg <= '0' & data_sreg(data_sreg'high downto 1);
          shift_count <= shift_count - 1;
          if (shift_count = 0) then
            state <= SHIFT_OP;
          end if;
          
        when SHIFT_OP =>
          spi_data_out <= reg_op_i & spi_data_out(spi_data_out'high downto 1);
          shift_count  <= to_unsigned(c_addr_bit_shifts, shift_count'length);
          state <= SHIFT_ADDR;
          
        when SHIFT_ADDR =>
          spi_data_out <= addr_sreg(0) & spi_data_out(spi_data_out'high downto 1);
          data_sreg <= ('0' & data_sreg(data_sreg'high downto 1));
          shift_count <= shift_count - 1;
          if (shift_count = 0) then
            state <= SPI_START_TRANSFER;
          end if;
          
        when SPI_START_TRANSFER =>
          bits_to_send <= to_unsigned(c_addr_op_width + f_siphra_reg_width(reg_addr_i),
                              bits_to_send'length);
          spi_start_p <= '1';
          spi_cs <= '1';
          state <= SPI_TRANSFER;
          
        when SPI_TRANSFER =>
          spi_start_p <= '0';
          if (spi_ready = '1') then
            state <= IDLE;
          end if;
          
        when others =>
          state <= IDLE;
          
      end case;
      
    end if;
  end process p_fsm;
  
  -- Outputs assignment
  reg_op_ready_o <= reg_op_ready;
  
  --============================================================================
  -- Instantiate SPI master
  --============================================================================
  cmp_spi_master : spi_master
    generic map
    (
      -- clock division ratio (SCLK = clk_sys_i / (2 ** g_div_ratio_log2).
      g_div_ratio_log2 => 7,

      -- data bits generic or from port?
      g_data_bits_generic => false,

      -- number of data bits per transfer; if from port, set to max. value expected on port
      g_num_data_bits  => g_num_data_bits
    )
    port map
    (
        clk_sys_i  => clk_i,
        rst_n_i    => rst_n_a_i,
        
        cs_i       => spi_cs,
        
        start_p_i  => spi_start_p,
        
        cpol_i     => '1',
        
        data_len_i => std_logic_vector(bits_to_send),
        
        data_i     => spi_data_in,
        
        ready_o    => spi_ready,
        
        data_o     => spi_data_out,
        
        spi_cs_n_o => spi_cs_n_o,
        spi_sclk_o => spi_sclk_o,
        spi_mosi_o => spi_mosi_o,
        spi_miso_i => spi_miso_i
    );


end architecture behav;
--==============================================================================
--  architecture end
--==============================================================================
