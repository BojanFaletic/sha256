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

    SIGNAL text_length : uint32_t := to_unsigned(65, 32);
    TYPE t_temp_msg IS ARRAY(0 TO 63) OF uint8_t;
    --SIGNAL temp_msg : t_temp_msg := (0 => x"61", 1 => x"62", 2 => x"63", OTHERS => x"00");
    SIGNAL temp_msg : t_temp_msg;
    SIGNAL temp_msg_0 : t_temp_msg := (OTHERS => x"61");
    SIGNAL temp_msg_1 : t_temp_msg := (0 => x"61", OTHERS => x"00");

    SIGNAL text_chunk : text_chunk_t;
    SIGNAL data_in_valid : STD_LOGIC := '0';
    SIGNAL ready : STD_LOGIC;
    SIGNAL done_out : STD_LOGIC;
    SIGNAL busy : STD_LOGIC;

    TYPE t_hash IS ARRAY(0 TO 31) OF uint8_t;
    CONSTANT out_hash_correct : t_hash := (x"63", x"53", x"61", x"c4", x"8b", x"b9", x"ea", x"b1", x"41", x"98", x"e7", x"6e", x"a8", x"ab", x"7f", x"1a", x"41", x"68", x"5d", x"6a", x"d6", x"2a", x"a9", x"14", x"6d", x"30", x"1d", x"4f", x"17", x"eb", x"0a", x"e0");
    signal hash_out : hash_out_t;

    FUNCTION TC(x : t_temp_msg) RETURN text_chunk_t IS
        VARIABLE tmp : text_chunk_t;
    BEGIN
        FOR i IN 0 TO 63 LOOP
            tmp(8 * (i + 1) - 1 DOWNTO 8 * i) := STD_LOGIC_VECTOR(x(i));
        END LOOP;
        RETURN tmp;
    END FUNCTION TC;

    -- check if output hash matches correct
    function check_hash(a : t_hash; b : hash_out_t) return boolean is
        begin
            for i in 0 to 31 loop
                if b(8*(i+1)-1 downto 8*i) = std_logic_vector(a(0)) then
                    return false;
                end if;
            end loop;
            return true;
    end function check_hash;
BEGIN

    p_async : PROCESS
    BEGIN
        WAIT;
    END PROCESS p_async;
    p_clk_generator : PROCESS
    BEGIN
        clk <= NOT clk;
        WAIT FOR 5 ns;
    END PROCESS p_clk_generator;

    p_run : PROCESS
    BEGIN
        WAIT FOR 100 ns;
        rst_n <= '1';
        data_in_valid <= '1';
        text_chunk <= TC(temp_msg_0);
        WAIT FOR 10 ns;
        data_in_valid <= '0';
        WAIT UNTIL busy = '0';

        data_in_valid <= '1';
        text_chunk <= TC(temp_msg_1);
        WAIT FOR 10 ns;
        data_in_valid <= '0';

        wait until done_out = '1';
        if check_hash(out_hash_correct, hash_out) then
            report "Hash passed!";
        else
            report "Hash ERROR!";
        end if;

        WAIT;
    END PROCESS p_run;

    T : sha256 PORT MAP(
        clk           => clk,
        rst_n         => rst_n,
        data_in_valid => data_in_valid,
        text_length   => text_length,
        text_chunk    => text_chunk,
        ready         => ready,
        busy          => busy,
        done_out      => done_out,
        hash_out      => hash_out
    );

END ARCHITECTURE Behavioral;