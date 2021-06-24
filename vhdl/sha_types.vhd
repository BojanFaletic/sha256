
LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.numeric_std.ALL;

PACKAGE sha_types IS
    SUBTYPE uint64_t IS unsigned(63 DOWNTO 0);
    SUBTYPE uint32_t IS unsigned(31 DOWNTO 0);
    SUBTYPE uint8_t IS unsigned(7 DOWNTO 0);
    SUBTYPE text_chunk_t IS STD_LOGIC_VECTOR(8 * 64 - 1 DOWNTO 0);
    SUBTYPE hash_out_t IS STD_LOGIC_VECTOR(8 * 32 - 1 DOWNTO 0);

    -- used in m block
    TYPE t_data IS ARRAY(0 TO 63) OF uint8_t;
END PACKAGE sha_types;