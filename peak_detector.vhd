LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.numeric_std.ALL;
USE work.common_pack.ALL;
LIBRARY UNISIM;
USE UNISIM.VCOMPONENTS.ALL;

ENTITY PEAK_DETECTOR IS
  PORT 
  (
    clk    : IN std_logic;
    reset  : IN std_logic;
    rxdata : IN std_logic;
---    led : out std_logic_vector(7 downto 0);
    txData : OUT std_logic
  );
END;

ARCHITECTURE STRUCT OF PEAK_DETECTOR IS

  COMPONENT UART_TX_CTRL IS
    PORT 
    (
      SEND    : IN STD_LOGIC;
      DATA    : IN STD_LOGIC_VECTOR (7 DOWNTO 0);
      CLK     : IN STD_LOGIC;
      READY   : OUT STD_LOGIC;
      UART_TX : OUT STD_LOGIC
    );
  END COMPONENT; 
 
  COMPONENT UART_RX_CTRL IS
    PORT 
    (
      RxD        : IN std_logic; -- serial data in
      sysclk     : IN std_logic; -- system clock
      reset      : IN std_logic; -- synchronous reset
      rxDone     : IN std_logic; -- data succesfully read (active high)
      rcvDataReg : OUT std_logic_vector(7 DOWNTO 0); -- received data
      dataReady  : OUT std_logic; -- data ready to be read
      setOE      : OUT std_logic; -- overrun error (active high)
      setFE      : OUT std_logic -- frame error (active high)
    );
  END COMPONENT;

  COMPONENT dataGen IS
    PORT 
    (
      -- Clk and Reset
      clk   : IN std_logic;
      reset : IN std_logic; -- synchronous reset
      -- Ports with dataProc
      ctrlIn  : IN std_logic;
      ctrlOut : OUT std_logic;
      data    : OUT std_logic_vector(7 DOWNTO 0)
    );
  END COMPONENT;
 
  COMPONENT dataConsume IS
    PORT 
    (
      -- Clk and Reset
      clk   : IN std_logic;
      reset : IN std_logic; -- synchronous reset
      -- LEDs
---      led : out std_logic_vector(7 downto 4);
      -- Ports with cmdProc
      start        : IN std_logic;
      numWords_bcd : IN BCD_ARRAY_TYPE(2 DOWNTO 0);
      dataReady    : OUT std_logic;
      byte         : OUT std_logic_vector(7 DOWNTO 0);
      maxIndex     : OUT BCD_ARRAY_TYPE(2 DOWNTO 0);
      dataResults  : OUT CHAR_ARRAY_TYPE(0 TO 6);
      seqDone      : OUT std_logic;
      -- Ports with dataGen
      ctrlIn  : IN std_logic;
      ctrlOut : OUT std_logic;
      data    : IN std_logic_vector(7 DOWNTO 0)
    );
  END COMPONENT;

  COMPONENT cmdProc IS
    PORT 
    (
      -- Clk and Reset
      clk   : IN std_logic;
      reset : IN std_logic;
      -- LEDs
---      led : out std_logic_vector(3 downto 0);
      -- Rx
      rxNow   : IN std_logic; -- valid
      rxData  : IN std_logic_vector(7 DOWNTO 0); --data
      rxDone  : OUT std_logic; -- done
      ovErr   : IN std_logic;
      framErr : IN std_logic;
      -- Tx
      txDone : IN std_logic;
      txData : OUT std_logic_vector(7 DOWNTO 0);
      txNow  : OUT std_logic;
      -- Ports with dataProc
      start        : OUT std_logic;
      numWords_bcd : OUT BCD_ARRAY_TYPE(2 DOWNTO 0);
      dataReady    : IN std_logic;
      byte         : IN std_logic_vector(7 DOWNTO 0);
      maxIndex     : IN BCD_ARRAY_TYPE(2 DOWNTO 0);
      dataResults  : IN CHAR_ARRAY_TYPE(0 TO RESULT_BYTE_NUM - 1);
      seqDone      : IN std_logic
    );
  END COMPONENT;

    SIGNAL sig_start, ctrl_genDriv, ctrl_consDriv, sig_dataReady, sig_seqDone   : std_logic;
    SIGNAL sig_rxDone, sig_rxNow, sig_ovErr, sig_framErr, sig_txNow, sig_txDone : std_logic;
 
    SIGNAL sig_rxData, sig_txData, sig_byte                                     : std_logic_vector(7 DOWNTO 0);
    SIGNAL sig_maxIndex                                                         : BCD_ARRAY_TYPE(2 DOWNTO 0);
    SIGNAL led_sig_data                                                         : std_logic_vector (7 DOWNTO 4) := x"0";
    SIGNAL led_sig_cmd                                                          : std_logic_vector (3 DOWNTO 0) := x"0";
    SIGNAL sig_dataResults                                                      : CHAR_ARRAY_TYPE(0 TO 6);
    SIGNAL sig_numWords_bcd                                                     : BCD_ARRAY_TYPE(2 DOWNTO 0);
 
    SIGNAL sig_data                                                             : std_logic_vector(7 DOWNTO 0);
 
    CONSTANT SEQ_COUNT_MAX                                                      : INTEGER := 1; -- defines how many runs to test

    TYPE ARRAY3D_TYPE IS ARRAY (0 TO SEQ_COUNT_MAX) OF CHAR_ARRAY_TYPE(0 TO RESULT_BYTE_NUM - 1);
    TYPE ARRAY3D_BCD_TYPE IS ARRAY (0 TO SEQ_COUNT_MAX) OF BCD_ARRAY_TYPE(2 DOWNTO 0);
    TYPE SEQUENCE_TYPE IS ARRAY (INTEGER RANGE <>) OF CHAR_ARRAY_TYPE(1 TO 2);

BEGIN
---	led(7 downto 4) <= led_sig_data; -- Each of them takes half of the lights
---	led(3 downto 0) <= led_sig_cmd;

	dataGen1 : dataGen
	PORT MAP
	(
		clk     => clk, 
		reset   => reset, 
		ctrlIn  => ctrl_consDriv, 
		ctrlOut => ctrl_genDriv, 
		data    => sig_data
	);

	dataConsume1 : dataConsume
	PORT MAP
	(
		clk          => clk, 
		reset        => reset, 
		start        => sig_start, 
		numWords_bcd => sig_numWords_bcd, 
		ctrlIn       => ctrl_genDriv, 
		ctrlOut      => ctrl_consDriv, 
		dataReady    => sig_dataReady, 
		byte         => sig_byte, 
		data         => sig_data, 
		seqDone      => sig_seqDone, 
		maxIndex     => sig_maxIndex, 
---		led => led_sig_data(7 downto 4),
		dataResults => sig_dataResults
	);

	cmdProc1 : cmdProc
	PORT MAP
	(
		clk          => clk, 
		reset        => reset, 
		rxNow        => sig_rxNow, 
		rxData       => sig_rxData, 
		txData       => sig_txData, 
		rxDone       => sig_rxDone, 
		ovErr        => sig_ovErr, 
		framErr      => sig_framErr, 
		txNow        => sig_txNow, 
		txDone       => sig_txDone, 
		start        => sig_start, 
		numWords_bcd => sig_numWords_bcd, 
		dataReady    => sig_dataReady, 
		byte         => sig_byte, 
		maxIndex     => sig_maxIndex, 
		seqDone      => sig_seqDone, 
---		led => led_sig_cmd(3 downto 0),
		dataResults => sig_dataResults
	);

	tx : UART_TX_CTRL
	PORT MAP
	(
		SEND    => sig_txNow, 
		DATA    => sig_txData, 
		CLK     => clk, 
		READY   => sig_txDone, 
		UART_TX => txData
	); 

	rx : UART_RX_CTRL
	PORT MAP
	(
		RxD        => rxData, -- input serial line
		sysclk     => clk, 
		reset      => reset, 
		rxDone     => sig_rxdone, 
		rcvDataReg => sig_rxData, 
		dataReady  => sig_rxNow, 
		setOE      => sig_ovErr, 
		setFE      => sig_framerr
	);

END;