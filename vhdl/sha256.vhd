LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.numeric_std.ALL;

ENTITY sha256 IS
END ENTITY sha256;

architecture Behavioral of sha256 is
    subtype uint32_t is unsigned(31 downto 0);
    subtype uint8_t is unsigned(7 downto 0);
begin

end architecture Behavioral;