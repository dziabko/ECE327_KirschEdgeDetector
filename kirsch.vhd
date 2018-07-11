
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
signal address  : unsigned (7 downto 0);


-- Write enabled to write to mem
signal wr_en : std_logic_vector (2 downto 0);

-- Memory outputs
signal mem1_out : unsigned(7 downto 0);
signal mem2_out : unsigned(7 downto 0);
signal mem3_out : unsigned(7 downto 0);

-- Create a 3x3 matrix for convolution
type column is array (2 downto 0) of unsigned(7 downto 0);
signal col1 : column;
signal col2 : column;
signal col3 : column;

-- One hot encoding for col input
signal col_first : std_logic_vector(2 downto 0) := "001";


begin

  -- Create 3 instances of memory to hold the pixels
  mem1 : entity work.mem(main)
    port map(
	  address => address,
	  clock => clk,
	  data => std_logic_vector(i_pixel),
	  wren => wr_en(0),
	  unsigned(q) => mem1_out
	);
	
  mem2 : entity work.mem(main)
    port map(
	  address => address,
	  clock => clk,
	  data => std_logic_vector(i_pixel),
	  wren => wr_en(1),
	  unsigned(q) => mem2_out
	);
	
  mem3 : entity work.mem(main)
    port map(
	  address => address,
	  clock => clk,
	  data => std_logic_vector(i_pixel),
	  wren => wr_en(2),
	  unsigned(q) => mem3_out
	);
  
  
  kirsch_edgedetector : process(clk, reset)
  begin
    if (reset='1') then
    elsif (clk'EVENT and clk='1') then
	  -- Check the one hot encoding, and fill the convolution matrix
	  if (i_valid='1') then
		  if (col_first="001") then
		    if (wr_en="100") then
		      col1(0) <= mem1_out; --Input first row
		      col1(1) <= mem2_out; -- input middle row
		      col1(2) <= i_pixel; -- input last row
			elsif (wr_en="010") then
		      col1(0) <= mem1_out; --Input first row
		      col1(1) <= i_pixel; -- input middle row
		      col1(2) <= mem3_out; -- input last row	
			elsif (wr_en="001") then
		      col1(0) <= i_pixel; --Input first row
		      col1(1) <= mem2_out; -- input middle row
		      col1(2) <= mem3_out; -- input last row
			end if;
		  elsif (col_first="010") then
		    if (wr_en="100") then
		      col2(0) <= mem1_out; --Input first row
		      col2(1) <= mem2_out; -- input middle row
		      col2(2) <= i_pixel; -- input last row
			elsif (wr_en="010") then
		      col2(0) <= mem1_out; --Input first row
		      col2(1) <= i_pixel; -- input middle row
		      col2(2) <= mem3_out; -- input last row	
			elsif (wr_en="001") then
		      col2(0) <= i_pixel; --Input first row
		      col2(1) <= mem2_out; -- input middle row
		      col2(2) <= mem3_out; -- input last row
			end if;
		  elsif (col_first="100") then
		    if (wr_en="100") then
		      col3(0) <= mem1_out; --Input first row
		      col3(1) <= mem2_out; -- input middle row
		      col3(2) <= i_pixel; -- input last row
			elsif (wr_en="010") then
		      col3(0) <= mem1_out; --Input first row
		      col3(1) <= i_pixel; -- input middle row
		      col3(2) <= mem3_out; -- input last row	
			elsif (wr_en="001") then
		      col3(0) <= i_pixel; --Input first row
		      col3(1) <= mem2_out; -- input middle row
		      col3(2) <= mem3_out; -- input last row
			end if;
		  end if;
		  
		  -- Shift the one hot encoder left by 1 for next input
		  col_first <= col_first(1 downto 0) & col_first(2);
	  end if;
	  
	  
    end if;
  end process;

    
end architecture;



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

entity kirsch_maxdir is
  port(
    clk				: in std_logic;
	reset			: in std_logic;
	a,b,c,d,e,f,g,h : in unsigned(7 downto 0);
	data_valid		: in std_logic;
	sel1			: in std_logic_vector(1 downto 0);
	o_max			: out std_logic_vector(2 downto 0);
	max_dir 		: out direction_ty
  );
end entity;

architecture maxdir of kirsch_maxdir is
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


library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.util.all;
use work.kirsch_synth_pkg.all;

entity kirsch_maxfinal is
  port(
    clk				: in std_logic;
	reset			: in std_logic;
	sum_a			: in unsigned(8 downto 0);
	sum_b			: in unsigned(7 downto 0);
	max_indir 		: in direction_ty;
	o_max			: out unsigned(9 downto 0);
	max_finaldir 	: out direction_ty
  );
end entity;

architecture finalMax of kirsch_maxfinal is
  signal max_a : unsigned(9 downto 0);
begin
  final_max : process(clk, reset)
  begin
    if (reset='1') then
	elsif(clk'EVENT and clk='1') then
	  -- First sum the two input
	  max_a <= (("0" & sum_a) + ("00" & sum_b));
	  -- Then find the final max with a feedback loop
	  
	  if (max_a > o_max) then
	    o_max <= max_a;
	  end if;
	end if;
  end process;
end architecture;

-----------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.util.all;
use work.kirsch_synth_pkg.all;

entity kirsch_edgecalc is
  port(
    clk				: in std_logic;
	reset			: in std_logic;
	i_a				: in unsigned(9 downto 0);
	i_b				: in unsigned(12 downto 0);
	in_dir 		: in direction_ty;
	o_edgeMax		: out signed(12 downto 0);
	o_edge			: out std_logic;
	o_dir 			: out direction_ty
  );
end entity;

architecture edgeCalc of kirsch_edgecalc is
signal input_b : std_logic_vector(12 downto 0);
signal u_i_b : unsigned(12 downto 0);
signal final_edgecalc : signed (12 downto 0);
begin
  final_max : process(clk, reset)
  begin
    if (reset='1') then
	elsif(clk'EVENT and clk='1') then
	  -- First shift the bottom input
	  input_b <=  std_logic_vector(i_b);
	  input_b <= input_b sll 3;
	  
	  -- Then subtract input a from b
	  final_edgecalc <= signed(i_b) - signed(i_a);
	  
	  -- Check if the final edge value is greater than 383 and set o_edge
	  if (final_edgecalc < 383) then
	    o_edgeMax <= final_edgecalc;
		o_edge <= '1';
		o_dir <= in_dir;
	  else
	    o_edgeMax <= (others=>'0');
		o_edge <= '0';
	  end if;
	end if;
  end process;
end architecture;
