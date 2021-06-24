LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.numeric_std.ALL;

------ type definitions
PACKAGE sha_types IS
  SUBTYPE uint64_t IS unsigned(63 DOWNTO 0);
  SUBTYPE uint32_t IS unsigned(31 DOWNTO 0);
  SUBTYPE uint8_t IS unsigned(7 DOWNTO 0);
  SUBTYPE text_chunk_t IS STD_LOGIC_VECTOR(8 * 64 - 1 DOWNTO 0);
  SUBTYPE hash_out_t IS STD_LOGIC_VECTOR(8 * 32 - 1 DOWNTO 0);
END PACKAGE sha_types;

------ main sha crypto
LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.numeric_std.ALL;
USE work.sha_types.ALL;

ENTITY sha256 IS
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
END ENTITY sha256;

ARCHITECTURE Behavioral OF sha256 IS

  -- standard SHA256 function
  FUNCTION CH(x, y, z : uint32_t) RETURN uint32_t IS
  BEGIN
    RETURN (x AND y) XOR ((NOT x) AND z);
  END FUNCTION CH;

  FUNCTION MAJ(x, y, z : uint32_t) RETURN uint32_t IS
  BEGIN
    RETURN (x AND y) XOR (x & z) XOR (y AND z);
  END FUNCTION MAJ;

  FUNCTION EP0(x : uint32_t) RETURN uint32_t IS
  BEGIN
    RETURN rotate_right(x, 2) XOR rotate_right(x, 13) XOR rotate_right(x, 22);
  END FUNCTION EP0;

  FUNCTION EP1(x : uint32_t) RETURN uint32_t IS
  BEGIN
    RETURN rotate_right(x, 6) XOR rotate_right(x, 11) XOR rotate_right(x, 25);
  END FUNCTION EP1;

  FUNCTION SIG0(x : uint32_t) RETURN uint32_t IS
  BEGIN
    RETURN rotate_right(x, 7) XOR rotate_right(x, 18) XOR shift_right(x, 3);
  END FUNCTION SIG0;

  FUNCTION SIG1(x : uint32_t) RETURN uint32_t IS
  BEGIN
    RETURN rotate_right(x, 17) XOR rotate_right(x, 19) XOR shift_right(x, 10);
  END FUNCTION SIG1;

  -- K root
  TYPE t_k IS ARRAY(0 TO 63) OF uint32_t;
  CONSTANT k : t_k :=
  (
  x"428a2f98", x"71374491", x"b5c0fbcf", x"e9b5dba5", x"3956c25b", x"59f111f1", x"923f82a4", x"ab1c5ed5",
  x"d807aa98", x"12835b01", x"243185be", x"550c7dc3", x"72be5d74", x"80deb1fe", x"9bdc06a7", x"c19bf174",
  x"e49b69c1", x"efbe4786", x"0fc19dc6", x"240ca1cc", x"2de92c6f", x"4a7484aa", x"5cb0a9dc", x"76f988da",
  x"983e5152", x"a831c66d", x"b00327c8", x"bf597fc7", x"c6e00bf3", x"d5a79147", x"06ca6351", x"14292967",
  x"27b70a85", x"2e1b2138", x"4d2c6dfc", x"53380d13", x"650a7354", x"766a0abb", x"81c2c92e", x"92722c85",
  x"a2bfe8a1", x"a81a664b", x"c24b8b70", x"c76c51a3", x"d192e819", x"d6990624", x"f40e3585", x"106aa070",
  x"19a4c116", x"1e376c08", x"2748774c", x"34b0bcb5", x"391c0cb3", x"4ed8aa4a", x"5b9cca4f", x"682e6ff3",
  x"748f82ee", x"78a5636f", x"84c87814", x"8cc70208", x"90befffa", x"a4506ceb", x"bef9a3f7", x"c67178f2");

  TYPE t_sm IS (init, transform_pre_0, transform_pre_1, transform, transform_final, final, done);
  SIGNAL sm : t_sm;

  -- ctx structure
  TYPE t_state IS ARRAY(0 TO 7) OF uint32_t;
  SIGNAL state : t_state;
  SIGNAL data_len : uint32_t;
  SIGNAL bit_len : uint64_t;
  TYPE t_data IS ARRAY(0 TO 63) OF uint8_t;
  SIGNAL data : t_data;

  -- counters
  SIGNAL transform_counter : INTEGER RANGE 0 TO 63;
  SIGNAL chunk_process_cnt : INTEGER RANGE 0 TO 63;

  -- variables
  SIGNAL a, b, c, d, e, f, g, h, i, j : uint32_t;
  TYPE t_m IS ARRAY(0 TO 63) OF uint32_t;
  SIGNAL text_rem : uint8_t;

  -- status variable (this should probably be optimized)
  SIGNAL is_final_blk : BOOLEAN;
  TYPE t_hash IS ARRAY(0 TO 31) OF uint8_t;
  SIGNAL hash : t_hash;

BEGIN
  -- preprocessing
  -- check reminder of text_input (used in final block modulo 64)
  text_rem <= resize(text_length(5 DOWNTO 0), 8);
  -- length in bits of full text (8*sizeof(msg))
  bit_len <= resize(shift_left(text_length, 3), 64);
  -- hash data type format to hash_type
  p_async : PROCESS (hash)
  BEGIN
    FOR i IN 0 TO 31 LOOP
      hash_out(8 * (i + 1) - 1 DOWNTO 8 * i) <= STD_LOGIC_VECTOR(hash(i));
    END LOOP;
  END PROCESS p_async;

  p_main : PROCESS (clk)
    VARIABLE v_t1, v_t2 : uint32_t;
    VARIABLE m : t_m;
  BEGIN
    IF rising_edge(clk) THEN
      IF rst_n = '0' THEN
        -- init state (reset ctx)
        data_len <= to_unsigned(0, 32);

        state(0) <= x"6a09e667";
        state(1) <= x"bb67ae85";
        state(2) <= x"3c6ef372";
        state(3) <= x"a54ff53a";
        state(4) <= x"510e527f";
        state(5) <= x"9b05688c";
        state(6) <= x"1f83d9ab";
        state(7) <= x"5be0cd19";
        busy <= '0';
        ready <= '0';
        sm <= init;
      ELSE
        IF sm = init THEN
          ready <= '1';
          IF data_in_valid = '1' THEN
            -- copy input data to core buffer
            FOR i IN 0 TO 63 LOOP
              data(i) <= unsigned(text_chunk(8 * (i + 1) - 1 DOWNTO 8 * i));
            END LOOP;
            -- data_len = 0

            -- signalling
            sm <= transform_pre_0;
            chunk_process_cnt <= 0;
            busy <= '1';
            IF text_length < 56 THEN
              is_final_blk <= true;
            ELSE
              is_final_blk <= false;
            END IF;
          END IF;
        ELSIF sm = transform_pre_0 THEN
          FOR i IN 0 TO 15 LOOP
            m(i) := data(4 * i) & data(4 * i + 1) & data(4 * i + 2) & data(4 * i + 3);
          END LOOP;
          sm <= transform_pre_1;
        ELSIF sm = transform_pre_1 THEN
          -- sha transform
          FOR i IN 16 TO 63 LOOP
            m(i) := SIG1(m(i - 2)) + m(i - 7) + SIG0(m(i - 15)) + m(i - 16);
          END LOOP;

          -- init variable
          a <= state(0);
          b <= state(1);
          c <= state(2);
          d <= state(3);
          e <= state(4);
          f <= state(5);
          g <= state(6);
          h <= state(7);

          -- process chunk
          transform_counter <= 0;
          sm <= transform;
        ELSIF sm = transform THEN
          -- process chunk (transform function)
          v_t1 := h + EP1(e) + CH(e, f, g) + k(transform_counter) + m(transform_counter);
          IF transform_counter = 16 THEN
            NULL;
          END IF;
          v_t2 := EP0(a) + MAJ(a, b, c);
          h <= g;
          g <= f;
          f <= e;
          e <= d + v_t1;
          d <= c;
          b <= a;
          a <= v_t1 + v_t2;
          transform_counter <= transform_counter + 1;

          IF transform_counter = 63 THEN
            sm <= transform_final;
          END IF;
        ELSIF sm = transform_final THEN
          state(0) <= state(0) + a;
          state(1) <= state(1) + b;
          state(2) <= state(2) + c;
          state(3) <= state(3) + d;
          state(4) <= state(4) + e;
          state(5) <= state(5) + f;
          state(6) <= state(6) + g;
          state(7) <= state(7) + h;

          -- clear data to all zeros (used in final stage)
          FOR i IN 0 TO 63 LOOP
            data(i) <= x"00";
          END LOOP;

          -- check if this is last block of hash
          chunk_process_cnt <= chunk_process_cnt + 1;
          IF is_final_blk THEN
            -- hashing is complete
            sm <= done;
          ELSIF (chunk_process_cnt + 1) * 64 > text_length THEN
            -- jump to final or process another hash
            sm <= final;
          ELSE
            -- process next block
            sm <= transform_pre_0;
          END IF;
        ELSIF sm = final THEN
          -- process final block (append length)

          -- assume data is less than 56
          ASSERT text_rem >= 56 REPORT "not implemented";

          data(to_integer(text_rem)) <= x"80";

          -- append data length to end of block
          data(63) <= bit_len(7 DOWNTO 0);
          data(62) <= bit_len(15 DOWNTO 8);
          data(61) <= bit_len(23 DOWNTO 16);
          data(60) <= bit_len(31 DOWNTO 24);
          data(59) <= bit_len(39 DOWNTO 32);
          data(58) <= bit_len(47 DOWNTO 40);
          data(57) <= bit_len(55 DOWNTO 48);
          data(56) <= bit_len(63 DOWNTO 56);

          is_final_blk <= true;

          -- this if from pre_transform state (should optimize)
          -- init variable
          a <= state(0);
          b <= state(1);
          c <= state(2);
          d <= state(3);
          e <= state(4);
          f <= state(5);
          g <= state(6);
          h <= state(7);

          -- process chunk
          transform_counter <= 0;
          sm <= transform;

          -- process last chunk

        ELSIF sm = done THEN
          -- write output hash to output

          -- reverse to little indian and output
          FOR i IN 0 TO 3 LOOP
            hash(i) <= shift_left(state(0), (24 - i * 8))(7 DOWNTO 0);
            hash(i + 4) <= shift_left(state(1), (24 - i * 8))(7 DOWNTO 0);
            hash(i + 8) <= shift_left(state(2), (24 - i * 8))(7 DOWNTO 0);
            hash(i + 12) <= shift_left(state(3), (24 - i * 8))(7 DOWNTO 0);
            hash(i + 16) <= shift_left(state(4), (24 - i * 8))(7 DOWNTO 0);
            hash(i + 20) <= shift_left(state(5), (24 - i * 8))(7 DOWNTO 0);
            hash(i + 24) <= shift_left(state(6), (24 - i * 8))(7 DOWNTO 0);
            hash(i + 28) <= shift_left(state(7), (24 - i * 8))(7 DOWNTO 0);
          END LOOP;

          sm <= init;
        END IF;
      END IF;
    END IF;
  END PROCESS p_main;
  p_signalization : PROCESS (sm)
  BEGIN
    IF sm /= done THEN
      done_out <= '0';
    ELSE
      done_out <= '1';
    END IF;
  END PROCESS p_signalization;
END ARCHITECTURE Behavioral;