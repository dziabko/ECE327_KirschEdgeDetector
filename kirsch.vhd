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
  process is
  variable op_a unsigned (12 downto 0);
  variable op_b unsigned (12 downto 0);
  begin 
  wait until rising_edge(clk);
  i_val_prev <= i_val;
  if (i_reset = '1') then
      sum <= (others => '0');
      i_val_prev <= (others => '0');
      op_a <= (others => '0');
      op_b <= (others => '0');
  else
      op_a <= shift_left(sum, 1)  when i_mux_sel_a = '1' else i_val;
      op_b <= "0000" & i_val_prev when i_mux_sel_b = '1' else sum;
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
  final_max : process(clk, reset)
  variable sum : unsigned (9 downto 0); 
  begin
    if (reset='1' OR i_clear = '1') then
	  max_a <= (others => '0');
	  max_dir <= (others => '0');
    elsif(clk'EVENT and clk='1') then
	  sum := ("0"&i_a) + ("00"&i_b);
	  if ( sum > max_a ) then
		max_a <= sum;
		max_dir <= i_dir;
	  end if;
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
	i_hold_out		: in std_logic;
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
	o_edge <= '0';
	o_edgeMax <= (others=>'0');
	o_dir <= dir_e;
	elsif(clk'EVENT and clk='1' AND i_hold_out = '0') then
	  -- First shift the bottom input
	  max_sum_x8 :=  shift_left("000" & i_max_of_sum, 3);
	  
	  -- Then subtract input a from b
	  final_edgecalc := signed(max_sum_x8) - signed(i_sum_of_all);
	  
	  -- Check if the final edge value is greater than 383 and set o_edge
	  if (final_edgecalc > 383) then
	    o_edgeMax <= final_edgecalc;
		o_edge <= '1';
		o_dir <= in_dir;
	  else
	    o_edgeMax <= (others=>'0');
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
	o_edge_max	: out signed (12 downto 0);
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
signal mul_en: std_logic;
signal max_dir_final : direction_ty;
signal max_val_final : unsigned (9 downto 0);
begin
	op_sum_all : entity work.three_x_sum(sum_val)
	port map (
		i_reset => reset,
		i_clear => clear,
		i_multiply => mul_en,
		i_val => sum_1,
		i_sel => i_sel,
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
		i_hold_out => o_done,
		o_edgeMax => o_edge_max,
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
		i_sel <= i_sel(6 downto 0) & i_valid;
		o_done <= i_sel(7);
	  end if;
	end process;

	clear_feedback_registers : process
	begin
	wait until rising_edge(clk);
	if (reset = '1') then
		clear <= '0';
	else
		case i_sel is
			when "000" =>
			  clear <= '1' when i_en = '0' else '0';
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
		if (reset = '0' AND i_sel = "100") then
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
signal edge_done : std_logic;
--row and address of our image
signal address  : unsigned (7 downto 0);
signal row 	: unsigned (7 downto 0);
signal current_address : unsigned (7 downto 0);
signal current_row : unsigned (7 downto 0);
signal mode : mode_ty;
-- Write enabled to write to mem
signal wr_en : std_logic_vector (2 downto 0);

-- Memory outputs
signal mem1_out : unsigned(7 downto 0);
signal mem2_out : unsigned(7 downto 0);
signal mem3_out : unsigned(7 downto 0);

-- One hot encoding for col input
--signal col_first : std_logic_vector(2 downto 0) := "001";

--Temp o_edge_max
signal o_edge_max : signed(12 downto 0);

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
		o_edge_max => o_edge_max,
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
 
  
  kirsch_edgedetector : process(clk, reset)
  begin
    if (reset='1') then
	  dfd_start <= '0';
	  
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

		-- Shift the one hot encoder left by 1 for next input
		current_address <= address;
		current_row <= row; 
		--Set next address for next input, and check for column
		if (address<255) then
		  address <= address + 1;
		elsif (address=255 and row=255) then --Last pixel
		  address <= (others => '0');
		  row <= (others => '0');
		  --col_first <= "001";
		elsif (address=255 and row<255) then --Last column, and not finished the last row
		  address <= (others => '0');
		  row <= row + 1;
		  wr_en <= wr_en(1 downto 0) & wr_en(2);
		end if;
		
		if (address >= 2 and row >= 2) then
		 dfd_start <= '1';
		end if;
		  
	  end if;

	  
	  --Start DFD only once we're in 3rd column and 3rd row
	  if (dfd_start = '1') then
		dfd_start <= '0';
	  end if;
	  
	  
    end if;
  end process;
  
  --Assign row and column ports
  o_mode <= mode;
  o_row <= current_row - 1 when (current_row > 0) else (others => '0');
  o_col <= current_address - 1 when (current_address > 0) else (others => '0');
  o_valid <= edge_done;
    
end architecture;
