-- (c) EMARD
-- LICENSE=BSD

-- generic (vendor-agnostic) serializer

LIBRARY ieee;
USE ieee.std_logic_1164.all;

ENTITY serializer_generic IS
PORT
(
  tx_in	       : IN STD_LOGIC_VECTOR(29 DOWNTO 0);
  tx_inclock   : IN STD_LOGIC; -- 10x tx_syncclock
  tx_syncclock : IN STD_LOGIC;
  tx_out       : OUT STD_LOGIC_VECTOR(2 DOWNTO 0)
);
END;

ARCHITECTURE SYN OF serializer_generic IS
BEGIN
  tx_out <= (others => '0');
END SYN;
