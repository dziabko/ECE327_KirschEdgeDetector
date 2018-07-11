
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.util.all;
use work.kirsch_synth_pkg.all;

entity three_x_sum is 
  port(
    i_val      : in unsigned ( 9 downto 0);
    i_multiply : in std_logic;
    i_reset    : in std_logic;
    o_result   : out unsigned ( 13 downto 0);
    o_overflow : out std_logic;
    clk        : in std_logic
  );
end entity;
architecture main of three_x_sum is 
  signal sum	: unsigned (13 downto 0);
begin
  process is
  begin 
  wait until rising_edge(clk);
  if (i_reset = '1') then
      sum <= (others => '0');
      o_overflow <= '0';
  else
      if (i_multiply <= '1') then
          sum <= sum + shift_left (sum, 1);
	  o_overflow <= '1' when sum (13 downto 12) > 0;
      else
      	  sum <= sum + i_val;
      end if;
  end if;
  end process;
  o_result <= sum;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.util.all;
use work.kirsch_synth_pkg.all;

entity kirsch is
  port(
    clk        : in  std_logic;                      
    reset      : in  std_logic;                      
    i_valid    : in  std_logic;                 
    i_pixel    : in  unsigned(7 downto 0);
    o_valid    : out std_logic;                 
    o_edge     : out std_logic;	                     
    o_dir      : out direction_ty;
    o_mode     : out mode_ty;
    o_row      : out unsigned(7 downto 0);
    o_col      : out unsigned(7 downto 0)
  );  
end entity;


architecture main of kirsch is
begin
    
end architecture;


library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.util.all;
use work.kirsch_synth_pkg.all;

entity kirsch_maxdir is
  port(
    clk				: in std_logic;
	reset			: in std_logic;
	a,b,c,d,e,f,g,h : in unsigned(7 downto 0);
	data_valid		: in std_logic;
	sel1			: in std_logic_vector(1 downto 0);
	o_max			: out std_logic_vector(2 downto 0)
  );
end entity;

architecture maxdir of kirsch_maxdir is
  signal max_dir : direction_ty;
begin

  firstMax : process(clk, reset)
  begin
	  if (reset='1') then
	    max_dir <= dir_n;
	  elsif (clk'EVENT and data_valid='1') then
		if (sel1="00") then
		  if (a > d) then
			max_dir <= dir_n;
		  else
			max_dir <= dir_ne;
		  end if;
		elsif (sel1="01") then
		  if (c > f) then
			max_dir <= dir_e;
		  else
			max_dir <= dir_se;
		  end if;
		elsif (sel1="10") then
		  if (e > h) then
			max_dir <= dir_s;
		  else
			max_dir <= dir_sw;
		  end if;
		else
		  if (g > b) then
			max_dir <= dir_w;
		  else
			max_dir <= dir_nw;
		  end if;
		end if;
	  end if;
  end process;
end architecture;
