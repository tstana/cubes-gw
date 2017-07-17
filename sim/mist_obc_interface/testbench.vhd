--==============================================================================
-- Testbench for MIST OBC interface module
--==============================================================================
--
-- author: Theodor Stana (theodor.stana@gmail.com)
--
-- date of creation: 2017-07-17
--
-- version: 1.0
--
-- description:
--
-- dependencies:
--
-- references:
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
--    2017-07-17   Theodor Stana     File created
--==============================================================================
-- TODO: -
--==============================================================================

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.genram_pkg.all;


entity testbench is
end entity testbench;


architecture arch of testbench is

  --============================================================================
  -- Type declarations
  --============================================================================
  type t_master_state is (
    IDLE,
    SEND_TRANSACTION_HEADER,

    PREP_F_ACK,
    PREP_T_ACK,
    SEND_HEADER_FRAME,
    
    RECEIVE_DATA_FRAME,
    SEND_DATA_FRAME,
    APPLY_DATA_FRAME,
    
    UART_TX_START,
    UART_WRAPPER_STOP
  );
  
  --============================================================================
  -- Constant declarations
  --============================================================================
  constant CLK_PERIOD   : time := 10 ns;
  constant RESET_PERIOD : time := 45 ns;
  
  constant BAUD_DIV_INT : natural := 867;
  constant BAUD_DIV     : std_logic_vector :=
      std_logic_vector(to_unsigned(BAUD_DIV_INT, f_log2_size(BAUD_DIV_INT)));

  --============================================================================
  -- Component declarations
  --============================================================================
  component uart is
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
  end component uart;

  --============================================================================
  -- Signal declarations
  --============================================================================
  -- Clock, reset signals
  signal clk_100meg, rst_n          : std_logic;
  
  -- Master-side UART signals
  signal master_txd, master_rxd     : std_logic;
  signal master_tx_data             : std_logic_vector(7 downto 0);
  signal master_rx_data             : std_logic_vector(7 downto 0);
  signal master_tx_start_p          : std_logic;
  signal master_tx_ready            : std_logic;
  signal master_rx_ready            : std_logic;
  
  -- MSP signals
  signal master_state               : t_master_state;
  signal transaction_ongoing        : std_logic;
  
  -- 1us counter between transactions
  signal count_1us                  : integer range 0 to 100;
  signal first_run                  : boolean := false;
  
--==============================================================================
--  architecture begin
--==============================================================================
begin

  --============================================================================
  -- Clock and reset signals
  --============================================================================
  P_CLK : process
  begin
    clk_100meg <= '1';
    wait for CLK_PERIOD/2;
    clk_100meg <= '0';
    wait for CLK_PERIOD/2;
  end process P_CLK;
  
  P_RST : process
  begin
    rst_n <= '0';
    wait for RESET_PERIOD;
    rst_n <= '1';
    wait;
  end process P_RST;

  --============================================================================
  -- Implement "I2C master" over UART
  --============================================================================
  -- Instantiate component
  U_UART : uart
    generic map
    (
      g_baud_div_bits => f_log2_size(BAUD_DIV_INT)
    )
    port map
    (
      -- Clock, reset
      clk_i         => clk_100meg,
      rst_n_a_i     => rst_n,

      -- Ports to external world
      rxd_i         => master_rxd,
      txd_o         => master_txd,

      -- Ports to other logic
      baud_div_i    => BAUD_DIV,

      tx_data_i     => master_tx_data,
      tx_start_p_i  => master_tx_start_p,
      tx_ready_o    => master_tx_ready,

      rx_ready_o    => master_rx_ready,
      rx_data_o     => master_rx_data,

      frame_err_o   => open
    );
    
  -- MSP master FSM
  P_FSM : process (clk_100meg, rst_n)
  begin
    if (rst_n = '0') then
      master_state <= IDLE;
      transaction_ongoing <= '0';
    elsif rising_edge(clk_100meg) then
    
      case master_state is
        when IDLE =>
          if (transaction_ongoing = '0') and (first_run = false) then
            count_1us <= count_1us + 1;
            if (count_1us = 99) then
              count_1us <= 0;
              master_state <= SEND_TRANSACTION_HEADER;
            end if;
          else
            first_run <= false;
          end if;
          
        when others =>
          master_state <= IDLE;
          
      end case;
      
    end if;
  end process P_FSM;

end architecture arch;
--==============================================================================
--  architecture end
--==============================================================================
