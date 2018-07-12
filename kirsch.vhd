library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.util.all;
use work.kirsch_synth_pkg.all;
-------------------------EDGE DETECTION------------------
entity three_x_sum is 
  port(
    i_val      : in unsigned ( 8 downto 0);
    i_multiply : in std_logic;
    i_reset    : in std_logic;
    i_clear    : in std_logic;
    o_result   : out unsigned ( 12 downto 0);
    o_overflow : out std_logic;
    clk        : in std_logic
  );
end entity;
architecture sum_val of three_x_sum is 
  signal sum	: unsigned (13 downto 0);
begin
  process is
  begin 
  wait until rising_edge(clk);
  if (i_reset = '1' OR i_clear = '1') then
      sum <= (others => '0');
      o_overflow <= '0';
  else
      if (i_multiply <= '1') then
          sum <= sum + shift_left (sum, 1);
      else
      	  sum <= sum + i_val;
      end if;
  end if;
  end process;
  o_result <= sum(12 downto 0);
  o_overflow <= sum (13);
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
	i_clear			: in std_logic;
	i_a			: in unsigned(8 downto 0);
	i_b			: in unsigned(7 downto 0);
	i_dir 			: in direction_ty;
	o_max_sum		: out unsigned(9 downto 0);
	o_dir		 	: out direction_ty
  );
end entity;

architecture finalMax of kirsch_maxfinal is
  signal max_a : unsigned(9 downto 0);
  signal max_dir : direction_ty;
begin
  final_max : process(clk, reset)
  begin
    if (reset='1' OR i_clear = '1') then
	max_a <= (others => '0');
	max_dir <= (others => '0');
    elsif(clk'EVENT and clk='1') then
	  max_a <= ("0" & i_a) + ("00" & i_b) when (i_a) + ("0" & i_b) > max_a;
	  o_dir <= i_dir when (i_a) + ("0" & i_b) > max_a; 
  end if;
  end process;
  o_max_sum <= max_a;
  o_dir <= max_dir;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.util.all;
use work.kirsch_synth_pkg.all;

entity kirsch_edgecalc is
  port(
    clk				: in std_logic;
	reset			: in std_logic;
	i_sum_of_all		: in unsigned(12 downto 0);
	i_max_of_sum		: in unsigned(9 downto 0);
	in_dir 		 	: in direction_ty;
	o_edgeMax		: out signed(12 downto 0);
	o_edge			: out std_logic;
	o_dir 			: out direction_ty
  );
end entity;

architecture edgeCalc of kirsch_edgecalc is
begin
  final_max : process(clk, reset)
  variable max_sum_x8 : unsigned(12 downto 0);
  variable final_edgecalc : signed (12 downto 0);
  begin
    if (reset='1') then
	elsif(clk'EVENT and clk='1') then
	  -- First shift the bottom input
	  max_sum_x8 :=  shift_left("000" & i_max_of_sum, 3);
	  
	  -- Then subtract input a from b
	  final_edgecalc := signed(max_sum_x8) - signed(i_sum_of_all);
	  
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


library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.util.all;
use work.kirsch_synth_pkg.all;

entity kirsch_edge_detector is 
  port(
	clk 		: in std_logic;
	reset		: in std_logic;
	i_en		: in std_logic;
	a,b,c,d,e,f,g,h	: in unsigned (7 downto 0);
	o_edge		: out std_logic;
	o_edge_max	: out signed (12 downto 0);
	o_dir		: out direction_ty
      );
end entity;

architecture main_edge_detect of kirsch_edge_detector is 
signal i_sel : std_logic_vector (2 downto 0);
signal sum_1 : unsigned (8 downto 0);
signal clear : std_logic;
signal sum_all_inputs : unsigned (12 downto 0);
signal max_dir_inter : direction_ty;
signal max_val_inter : unsigned (7 downto 0);
signal mul_en: std_logic;
signal sum_all_overflow : std_logic;
signal max_dir_final : direction_ty;
signal max_val_final : unsigned (9 downto 0);
begin
	op_sum_all : entity work.three_x_sum(sum_val)
	port map (
		i_reset => reset,
		i_clear => clear,
		i_multiply => mul_en,
		i_val => sum_1,
		o_result => sum_all_inputs,
		o_overflow => sum_all_overflow,
		clk => clk
		);
	op_get_max : entity work.kirsch_maxfinal(finalMax)
	port map(
		clk => clk,
		reset => reset,
		i_clear => clear,
		i_a => sum_1,
		i_b => max_val_inter,
		i_dir => max_dir_inter,
		o_max_sum => max_val_final,
		o_dir => max_dir_final
		);
	op_test_edge : entity work.kirsch_edgecalc(edgeCalc)
	port map(
		clk =>clk,
		reset => reset,
		i_sum_of_all => sum_all_inputs,
		i_max_of_sum => max_val_final,
		in_dir => max_dir_final,
		o_edgeMax => o_edge_max,
		o_edge => o_edge,
		o_dir => o_dir
		);	

	cycle_states: process 
	begin
	wait until rising_edge(clk);
	if (reset= '1') then
		i_sel <= (others => '0');
	else
		case i_sel is 
			when "000" =>
			i_sel <= "001";
			when "001" =>
			i_sel <= "010";
			when "010" =>
 			i_sel <= "011";
			when "011" => 
			i_sel <= "100";
			when "100" =>
			i_sel <= "101";
			when "101" =>
			i_sel <= "110";
			when "110" => 
			i_sel <= "111";
			when others =>
			i_sel <= "000" when i_en = '1';
		end case;
	end if;
	end process;
	
	clear_feedback_registers : process
	begin
	wait until rising_edge(clk);
	if (reset = '1') then
		clear <= '0';
	else
		case i_sel is
			when "111" =>
			clear <= '1';
			when others =>
			clear <= '0';
		end case;
	end if;
	end process;

	sum_2_inputs : process
	begin
	wait until rising_edge(clk);
	if (reset = '1') then
		sum_1 <= (others => '0');
	else 
		case i_sel is 
			when "000" =>
			sum_1 <= ("0"&a) + ("0"&h);
			when "001" => 
			sum_1 <= ("0"&b)+("0"&c);
			when "010" =>
			sum_1 <= ("0"&d) + ("0"&e);
			when "011" =>
			sum_1 <= ("0"&g) +("0"&f);
			when others =>
			sum_1 <= (others => '0');
		end case;
	end if;	
	end process;

	max_dir_group : process
	begin
	wait until rising_edge(clk);
	if (reset = '1') then
		max_val_inter <= (others => '0');
		max_dir_inter <= dir_e;
	else
		case i_sel is 
		when "000" =>
		  if (g >= b) then
			max_dir_inter <= dir_w;
			max_val_inter <= g;
		  else
			max_dir_inter <= dir_nw;
			max_val_inter <= b;
		  end if;
		when "001" =>
		  if (a >= d) then
			max_dir_inter <= dir_n;
			max_val_inter <= a;
		  else
			max_dir_inter <= dir_ne;
			max_val_inter <= d;
		  end if;
		when "010" =>
		  if (c >= f) then
			max_dir_inter <= dir_e;
			max_val_inter <= c;
		  else
			max_dir_inter <= dir_se;
			max_val_inter <= f;
		  end if;
		when "011" =>
		  if (e >= h) then
			max_dir_inter <= dir_s;
			max_val_inter <= e;
		  else
			max_dir_inter <= dir_sw;
			max_val_inter <= h;
		  end if;
		when others =>
			max_val_inter <= (others => '0');
			max_dir_inter <= dir_e; 
		end case;
	end if;
	end process;

	set_sum_all_mult_en : process
	begin
		wait until rising_edge(clk);
		if (reset = '0' AND i_sel = "101") then
			mul_en <= '1';
		else
			mul_en <= '0';
		end if; 
	end process;		
end architecture;
--------------------------------------------------END EDGE DETECTION--------------------------------------------------
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

