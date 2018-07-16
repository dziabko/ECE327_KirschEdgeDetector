-------------------------------EDGE DETECTOR----------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.util.all;
use work.kirsch_synth_pkg.all;

entity three_x_sum is 
  port(
    i_val      : in unsigned (8 downto 0);
    i_mux_sel_a  : in std_logic; --'1' selects o_result*2 '0' selects i_val
    i_mux_sel_b  : in std_logic; --'1' selects i_val from previous cycle '0' selects result;
    i_reset    : in std_logic;
    o_result   : out unsigned (12 downto 0);
    clk        : in std_logic
  );
end entity;
architecture sum_val of three_x_sum is 
  signal sum	: unsigned (12 downto 0);
  signal i_val_prev : unsigned (8 downto 0);
begin
  calculate_sum : process 
  variable op_a : unsigned (12 downto 0);
  variable op_b : unsigned (12 downto 0);
  begin 
  wait until rising_edge(clk);
  i_val_prev <= i_val;
  if (i_reset = '1') then
      sum <= (others => '0');
      i_val_prev <= (others => '0');
      op_a := (others => '0');
      op_b := (others => '0');
  else
      op_a := shift_left(sum, 1)  when i_mux_sel_a = '1' else "0000" & i_val;
      op_b := "0000" & i_val_prev when i_mux_sel_b = '1' else sum;
      sum <= op_a + op_b;
  end if;
  end process;
  o_result <= sum;
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
  final_max : process
  variable sum : unsigned (9 downto 0); 
  begin
    wait until rising_edge(clk);
    o_max_sum <= max_a;
    o_dir <= max_dir;
    if (reset='1') then
	  max_a <= (others => '0');
	  max_dir <= (others => '0');
	  o_max_sum <= (others => '0');
	  o_dir <= (others => '0');
    else
	  --Move reg_a to next reg
	  o_max_sum <= max_a;
	  o_dir <= max_dir;
	  
	  -- Sum the inputs, and find the max
	  sum := ("0"&i_a) + ("00"&i_b);
	  -- If sum is greater than cur max, set max to 0
	  if(sum > max_a or i_clear='1') then
		max_a <= sum;
		max_dir <= i_dir;
	  end if;
	  
	  
    end if;
  end process;
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
	i_out_en		: in std_logic;
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
	o_edge <= '0';
	o_dir <= dir_e;
	elsif(clk'EVENT and clk='1') then
	  -- First shift the bottom input
	  max_sum_x8 :=  shift_left("000" & i_max_of_sum, 3);  
	  -- Then subtract input a from b
	  final_edgecalc := signed(max_sum_x8) - signed(i_sum_of_all);
	  
	  -- Check if the final edge value is greater than 383 and set o_edge
	  if (final_edgecalc > 383 AND i_out_en = '1') then
		o_edge <= '1';
		o_dir <= in_dir;
	  else
		o_edge <= '0';
		o_dir <= dir_e;
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
	o_dir		: out direction_ty;
	o_done		: out std_logic
     );
end entity;

architecture main_edge_detect of kirsch_edge_detector is 
signal i_sel : std_logic_vector (7 downto 0);
signal sum_1 : unsigned (8 downto 0);
signal clear : std_logic;
signal sum_all_inputs : unsigned (12 downto 0);
signal max_dir_inter : direction_ty;
signal max_val_inter : unsigned (7 downto 0);
signal max_dir_final : direction_ty;
signal max_val_final : unsigned (9 downto 0);
signal three_add_mux_a : std_logic;
signal three_add_mux_b : std_logic;
begin
	op_sum_all : entity work.three_x_sum(sum_val)
	port map (
		i_reset => reset,
		i_mux_sel_b => three_add_mux_b,
		i_mux_sel_a => three_add_mux_a,
		i_val => sum_1,
		o_result => sum_all_inputs,
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
		i_out_en => i_sel(6),
		o_edge => o_edge,
		o_dir => o_dir
		);
	cycle_states: process 
	begin
	wait until rising_edge(clk);
	  if (reset= '1') then
		o_done <= '0';
		i_sel <= (others => '0');
	  else
		--Need shift i_sel left
		--i_sel depicts how far along the inputs are along the stage
		i_sel <= std_logic_vector(shift_left(unsigned(i_sel), 1));
		i_sel(0) <= i_en;
		o_done <= i_sel(6);
	  end if;
	end process;

	clear_max_final_output : process
	begin
	wait until rising_edge(clk);
	if (reset = '1') then
		clear <= '0';
	else
		case i_sel(0) is
			when '1' =>
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
		case i_sel(3 downto 0) is 
			when "0001"  =>
			sum_1 <= ("0"&a) + ("0"&h);
			when "0010" => 
			sum_1 <= ("0"&b)+("0"&c);
			when "0100" =>
			sum_1 <= ("0"&d) + ("0"&e);
			when others =>
			sum_1 <= ("0"&g) +("0"&f);
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
		case i_sel(3 downto 0) is 
		when "0001" =>
		  if (g >= b) then
			max_dir_inter <= dir_w;
			max_val_inter <= g;
		  else
			max_dir_inter <= dir_nw;
			max_val_inter <= b;
		  end if;
		when "0010" =>
		  if (a >= d) then
			max_dir_inter <= dir_n;
			max_val_inter <= a;
		  else
			max_dir_inter <= dir_ne;
			max_val_inter <= d;
		  end if;
		when "0100" =>
		  if (c >= f) then
			max_dir_inter <= dir_e;
			max_val_inter <= c;
		  else
			max_dir_inter <= dir_se;
			max_val_inter <= f;
		  end if;
		  
		when "1000" =>
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

	set_control_three_x_sum : process
	begin
		wait until rising_edge(clk);
		if (reset = '1') then
			three_add_mux_a <= '0';
			three_add_mux_b <= '0';
		else
			three_add_mux_b <= i_sel(1);
			three_add_mux_a <= i_sel(4);
					   
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
signal edge_done : std_logic;
--row and address of our image
signal address  : unsigned (7 downto 0);
signal row 	: unsigned (7 downto 0);
signal current_column : unsigned (7 downto 0);
signal current_row : unsigned (7 downto 0);
signal mode : mode_ty;
-- Write enabled to write to mem
signal wr_en : std_logic_vector (2 downto 0);

-- Memory outputs
signal mem1_out : unsigned(7 downto 0);
signal mem2_out : unsigned(7 downto 0);
signal mem3_out : unsigned(7 downto 0);

-- One hot encoding for col input

-- Signal detect DFD start
signal dfd_start : std_logic := '0';

signal a,b,c,d,e,f,g,h,i : unsigned (7 downto 0);
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
	
	-- Create the kirsch_edge_detector entity
	Edge_Detector : entity work.kirsch_edge_detector(main_edge_detect)
	  port map(
	    clk => clk,
		reset => reset,
		a => a, h => h, g => g,
		b => b, f => f,
		c => c, d => d, e => e,
		o_edge => o_edge,
		o_dir => o_dir,
		i_en => dfd_start,
		o_done => edge_done
	  );
  set_o_mode : process
  begin
   wait until rising_edge(clk);
   if (reset = '1') then
    mode <= m_reset;
   elsif (dfd_start = '1') then
    mode <= m_busy;
   elsif (o_valid <= '1' OR (reset = '0' AND mode = m_reset)) then
    mode <= m_idle;
   end if;
  end process;
 
  set_display_column_and_row : process
  begin
  wait until rising_edge(clk);
	if (reset='1' or (current_column >= 254 AND current_row >= 254 and i_valid = '1')) then
		current_column <= to_unsigned(1,current_column'length);
		current_row <= to_unsigned(1,current_row'length);
	elsif (edge_done = '1') then
		current_column <= current_column + 1;
		if (current_column >= 254) then
			current_column <= to_unsigned(1, current_column'length);
			current_row <= current_row + 1;
		end if;
	end if;
  end process;
  kirsch_edgedetector : process(clk, reset)
  begin
    if (reset='1') then
	  
	  --Reset our wr_en, address, row
	  wr_en <= "001";
	  address <= (others => '0');
	  row <= (others => '0');
	  a <= (others => '0');
	  b <= (others => '0');
	  c <= (others => '0');
	  d <= (others => '0');
	  e <= (others => '0');
	  f <= (others => '0');
	  g <= (others => '0');
	  h <= (others => '0');
	  i <= (others => '0');
    elsif (clk'EVENT and clk='1') then
	  -------------------------------------------------------------------------------------------------
	  -- Check the one hot encoding, and fill the convolution matrix
	  if (i_valid='1') then
		-- Shift values of matrix
		--1st col
		a <= b;
		h <= i;
		g <= f;
		--2nd col
		b <= c;
		i <= d;
		f <= e;
		
		--Add value to 3rd conv column
		if (wr_en="100") then
		  --3rd col
		  c <= mem1_out;
		  d <= mem2_out;
		  e <= i_pixel;
		elsif(wr_en="010") then
		  --3rd col
		  c <= mem3_out;
		  d <= mem1_out;
		  e <= i_pixel;
		elsif(wr_en="001") then
		  --3rd col
		  c <= mem2_out;
		  d <= mem3_out;
		  e <= i_pixel;
		end if;
		
		  
		  
		  ---------------------------------------------------------------------------------------------
		--Set next address for next input, and check for column
		if (address<255) then
		  address <= address + 1;
		elsif (address=255 and row=255) then --Last pixel
		  address <= (others => '0');
		  row <= (others => '0');
		elsif (address=255 and row<255) then --Last column, and not finished the last row
		  address <= (others => '0');
		  row <= row + 1;
		  wr_en <= wr_en(1 downto 0) & wr_en(2);
		end if;
	  end if;
	  
    end if;
  end process;
  dfd_start <= '1' when i_valid = '1' AND address >= 2 AND row >=2 else '0';
  --Assign row and column ports
  o_mode <= mode;
  o_row <= current_row;
  o_col <= current_column;
  o_valid <= edge_done;
    
end architecture;
