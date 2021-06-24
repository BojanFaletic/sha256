LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.numeric_std.ALL;
USE work.sha_types.ALL;

ENTITY sha256_tb IS
END ENTITY sha256_tb;

ARCHITECTURE Behavioral OF sha256_tb IS
    COMPONENT sha256 IS
        PORT (
            clk, rst_n : IN STD_LOGIC;
            -- fifo style
            text_length : IN uint32_t;
            text_chunk  : IN text_chunk_t;

            -- signaling
            data_in_valid : IN STD_LOGIC;
            ready         : OUT STD_LOGIC;
            busy          : OUT STD_LOGIC;
            done_out      : OUT STD_LOGIC;

            -- output
            hash_out : OUT hash_out_t
        );
    END COMPONENT sha256;

    SIGNAL clk : STD_LOGIC := '0';
    SIGNAL rst_n : STD_LOGIC := '0';

    SIGNAL text_length : uint32_t := to_unsigned(3, 32);
    TYPE t_temp_msg IS ARRAY(0 TO 63) OF uint8_t;
    SIGNAL temp_msg : t_temp_msg := (0 => x"61", 1 => x"62", 2 => x"63", OTHERS => x"00");

    SIGNAL text_chunk : text_chunk_t;
    signal data_in_valid : std_logic := '0';
    signal ready : std_logic;
    signal done_out : std_logic;
    signal busy : std_logic;
begin

p_async : process
begin
    for i in 0 to 63 loop
        text_chunk(8*(i+1)-1 downto 8*i) <= std_logic_vector(temp_msg(i));
    end loop;
    wait;
end process p_async;


p_clk_generator : process
begin
    clk <= not clk;
    wait for 5 ns;
end process p_clk_generator;

p_run : process
begin
    wait for 100 ns;
    rst_n <= '1';
    data_in_valid <= '1';
    wait for 10 ns;
    data_in_valid <= '0';


    wait;
end process p_run;

T : sha256 port map(
    clk => clk,
    rst_n => rst_n,
    data_in_valid => data_in_valid,
    text_length => text_length,
    text_chunk => text_chunk,
    ready => ready,
    busy => busy,
    done_out => done_out,
    hash_out => open
);

END ARCHITECTURE Behavioral;