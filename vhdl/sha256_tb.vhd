LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.numeric_std.ALL;
USE work.sha_types.ALL;

ENTITY sha256_tb IS
END ENTITY sha256_tb;

-- how to generate hash:
-- for x in hashlib.sha256(b'a'*65).digest(): print(f'x"{int(x):02x}",', end='')

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
    SIGNAL temp_msg : t_temp_msg;
    SIGNAL temp_msg_0 : t_temp_msg := (OTHERS => x"61");
    SIGNAL temp_msg_1 : t_temp_msg := (0 => x"61", OTHERS => x"00");
    SIGNAL temp_msg_2 : t_temp_msg := (0 => x"61", 1 => x"61", 2 => x"61", OTHERS => x"00");

    SIGNAL text_chunk : text_chunk_t;
    SIGNAL data_in_valid : STD_LOGIC := '0';
    SIGNAL ready : STD_LOGIC;
    SIGNAL done_out : STD_LOGIC;
    SIGNAL busy : STD_LOGIC;

    -- expected calculated hashes
    TYPE t_hash IS ARRAY(0 TO 31) OF uint8_t;
    CONSTANT out_hash_correct_1 : t_hash := (x"63", x"53", x"61", x"c4", x"8b", x"b9", x"ea", x"b1", x"41", x"98", x"e7", x"6e", x"a8", x"ab", x"7f", x"1a", x"41", x"68", x"5d", x"6a", x"d6", x"2a", x"a9", x"14", x"6d", x"30", x"1d", x"4f", x"17", x"eb", x"0a", x"e0");
    CONSTANT out_hash_correct_2 : t_hash := (x"98", x"34", x"87", x"6d", x"cf", x"b0", x"5c", x"b1", x"67", x"a5", x"c2", x"49", x"53", x"eb", x"a5", x"8c", x"4a", x"c8", x"9b", x"1a", x"df", x"57", x"f2", x"8f", x"2f", x"9d", x"09", x"af", x"10", x"7e", x"e8", x"f0");
    SIGNAL hash_out : hash_out_t;

    FUNCTION TC(x : t_temp_msg) RETURN text_chunk_t IS
        VARIABLE tmp : text_chunk_t;
    BEGIN
        FOR i IN 0 TO 63 LOOP
            tmp(8 * (i + 1) - 1 DOWNTO 8 * i) := STD_LOGIC_VECTOR(x(i));
        END LOOP;
        RETURN tmp;
    END FUNCTION TC;

    -- check if output hash matches correct
    FUNCTION check_hash(a : t_hash; b : hash_out_t) RETURN BOOLEAN IS
    BEGIN
        FOR i IN 0 TO 31 LOOP
            IF b(8 * (i + 1) - 1 DOWNTO 8 * i) /= STD_LOGIC_VECTOR(a(i)) THEN
                RETURN false;
            END IF;
        END LOOP;
        RETURN true;
    END FUNCTION check_hash;
BEGIN

    p_clk_generator : PROCESS
    BEGIN
        clk <= NOT clk;
        WAIT FOR 5 ns;
    END PROCESS p_clk_generator;

    p_run : PROCESS
    BEGIN
        WAIT FOR 100 ns;

        -- test 1 (65*a)
        rst_n <= '1';
        data_in_valid <= '1';
        text_chunk <= TC(temp_msg_0);
        text_length <= to_unsigned(65, 32);
        WAIT FOR 10 ns;
        data_in_valid <= '0';
        WAIT UNTIL busy = '0';

        data_in_valid <= '1';
        text_chunk <= TC(temp_msg_1);
        WAIT FOR 10 ns;
        data_in_valid <= '0';

        WAIT UNTIL done_out = '1' AND rising_edge(clk);
        IF check_hash(out_hash_correct_1, hash_out) THEN
            REPORT "Hash passed!";
        ELSE
            REPORT "Hash ERROR!";
        END IF;

        -- test simple (3*a)
        WAIT FOR 10 ns;
        data_in_valid <= '1';
        text_chunk <= TC(temp_msg_2);
        text_length <= to_unsigned(3, 32);
        WAIT FOR 10 ns;
        data_in_valid <= '0';

        WAIT UNTIL done_out = '1' AND rising_edge(clk);
        IF check_hash(out_hash_correct_2, hash_out) THEN
            REPORT "Hash passed!";
        ELSE
            REPORT "Hash ERROR!";
        END IF;

        REPORT "Done";
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