LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.numeric_std.ALL;
USE work.common_pack.ALL;

ENTITY cmdProc IS
  PORT
  (
    clk          : IN std_logic;
    reset        : IN std_logic;
    rxnow        : IN std_logic;
    rxData       : IN std_logic_vector (7 DOWNTO 0);
    txData       : OUT std_logic_vector (7 DOWNTO 0);
    rxdone       : OUT std_logic;
    ovErr        : IN std_logic;
    framErr      : IN std_logic;
    txnow        : OUT std_logic;
    txdone       : IN std_logic;
    start        : OUT std_logic;
    numWords_bcd : OUT BCD_ARRAY_TYPE(2 DOWNTO 0);
    dataReady    : IN std_logic;
    byte         : IN std_logic_vector(7 DOWNTO 0);
    maxIndex     : IN BCD_ARRAY_TYPE(2 DOWNTO 0);
    dataResults  : IN CHAR_ARRAY_TYPE(0 TO RESULT_BYTE_NUM - 1);
---    led          : OUT std_logic_vector(3 DOWNTO 0);
    seqDone      : IN std_logic
  );
END cmdProc;

ARCHITECTURE cmd OF cmdProc IS

  TYPE STATE_TYPE IS (
		Print, Print_Wait,
		IDLE, A_cmd, A_N1, A_N2, A_byte, A_startByte,
		-- Below are not actual 'states', they are identifiers for facilitation
		FEED, CARRIAGE, A_2, A_3,
		P_2, P_3, P_Sp, P_i1, P_i2, P_i3, L_first, L_second, L_space);
	--
  SIGNAL curState, nextState, reg_prevS, sig_prevS, reg_futS, sig_futS : STATE_TYPE := IDLE; -- Initialize all to the IDLE state
  --
  SIGNAL sig_n1, sig_n2, sig_n3, sig_EnPrint, sig_EnfutS : BOOLEAN := FALSE;
  SIGNAL reg_hasProc, sig_hasProc                        : BOOLEAN := FALSE; -- Whether a squence has been processed
  SIGNAL reg_value, sig_value                            : std_logic_vector(7 DOWNTO 0) := x"00"; -- value to print to the terminal
  --
  SIGNAL reg_listCounter : INTEGER RANGE 0 TO 7; -- Count the 7 bytes to print in the L command
  --
  SIGNAL reg_numWords_bcd : BCD_ARRAY_TYPE(1 DOWNTO 0) := (x"0", x"0"); -- Stores the A command's first 2 BCD numbers
  --
  FUNCTION byteToHex (conversion : std_logic_vector(3 DOWNTO 0)) RETURN std_logic_vector IS -- Converts hex values to ASCII hex representation
  BEGIN
    CASE conversion IS
      WHEN x"0"  => RETURN x"30";
      WHEN x"1"  => RETURN x"31";
      WHEN x"2"  => RETURN x"32";
      WHEN x"3"  => RETURN x"33";
      WHEN x"4" => RETURN x"34";
      WHEN x"5" => RETURN x"35";
      WHEN x"6" => RETURN x"36";
      WHEN x"7" => RETURN x"37";
      --
      WHEN x"8" => RETURN x"38";
      WHEN x"9" => RETURN x"39";
      WHEN x"A" => RETURN x"41";
      WHEN x"B" => RETURN x"42";
      WHEN x"C" => RETURN x"43";
      WHEN x"D" => RETURN x"44";
      WHEN x"E" => RETURN x"45";
      WHEN x"F" => RETURN x"46";
      WHEN OTHERS      => RETURN x"3F";
    END CASE;
  END byteToHex;
  --
  BEGIN
    -- Processes - there are sequential ones and combinational ones
		
    -- ***Sequential Circuits***
    stateReg : PROCESS (reset, clk) -- Memory circuit for the state register
    BEGIN
      IF clk'EVENT AND clk = '1' THEN
        curState <= nextState;
        IF reset = '1' THEN
          curState <= IDLE;
        END IF;
      END IF;
    END PROCESS; -- stateReg
		
    list_counter_process : PROCESS (reset, clk) -- Memory circuit for counting
    -- Counts the number of bytesv(there are 7) printed in the L command
    BEGIN
      IF clk'EVENT AND clk = '1' THEN
        IF reset = '1' THEN
          reg_listCounter <= 0;
        ELSE
          IF sig_prevS = L_space THEN
            reg_listCounter <= reg_listCounter + 1;
          ELSIF reg_prevS = IDLE THEN -- resets the counter after counting finishes
            reg_listCounter <= 0;
          END IF;
        END IF;
      END IF;
    END PROCESS; -- list_counter_process
		
    bcdNumWords_reg : PROCESS (reset, clk) -- Memory circuit
    -- Stores the ANNN(the Ns) into the numWords_bcd output
    BEGIN
      IF clk'EVENT AND clk = '1' THEN
        IF reset = '1' THEN
          numWords_bcd(2) <= x"0";
          numWords_bcd(1) <= x"0";
          numWords_bcd(0) <= x"0";
        ELSIF sig_n3 = TRUE THEN
          numWords_bcd(2)     <= rxData(3 DOWNTO 0);
          reg_numWords_bcd(1) <= rxData(3 DOWNTO 0);
        ELSIF sig_n2 = TRUE THEN
          numWords_bcd(1)     <= rxData(3 DOWNTO 0);
          reg_numWords_bcd(0) <= rxData(3 DOWNTO 0);
        ELSIF sig_n1 = TRUE THEN
          numWords_bcd(0) <= rxData(3 DOWNTO 0);
        END IF;
      END IF;
    END PROCESS; -- bcdNumWords_reg
		
    print_reg : PROCESS (reset, clk) -- memory circuit
    -- Accounts for printing to the terminal
    BEGIN
      IF clk'EVENT AND clk = '1' THEN
        IF reset = '1' THEN
          reg_prevS <= IDLE;
          reg_value <= x"00";
        ELSIF sig_EnPrint = TRUE THEN
          reg_prevS <= sig_prevS;
          reg_value <= sig_value;
        END IF;
      END IF;
    END PROCESS; -- print_reg
		
    fut_reg : PROCESS (reset, clk) -- memory circuit
    -- Accounts for printing to the terminal
    BEGIN
      IF clk'EVENT AND clk = '1' THEN
        IF reset = '1' THEN
          reg_futS <= IDLE;
        ELSIF sig_EnfutS = TRUE THEN
          reg_futS <= sig_futS;
        END IF;
      END IF;
    END PROCESS; -- print_reg
		
    hasProc_reg : PROCESS (reset, clk) -- memory circuit
    -- Remembers that A command is ran at least once
    BEGIN
      IF clk'EVENT AND clk = '1' THEN
        IF reset = '1' THEN
          reg_hasProc <= FALSE;
        ELSIF sig_hasProc = TRUE THEN
          reg_hasProc <= TRUE;
        END IF;
      END IF;
    END PROCESS; -- hasProc_reg
    -- ***End of Sequential Circuits***
		
    -- ***Combinational Next State Logic***
    nextStateLogic : PROCESS (curState, rxData, rxNow, txDone, dataReady, byte, seqDone,
      reg_hasProc, reg_numWords_bcd, reg_prevS, reg_value, reg_futS)
    BEGIN
      -- Default conditions (prevent latches)
      nextState <= curState;
---      led       <= x"0"; -- all off
			
			-- [Determines the next state]
      CASE curState IS
        WHEN IDLE =>
          IF rxnow = '1' THEN
            nextState <= Print;
          END IF;
---          led <= "1111";
        --
        WHEN Print =>
          nextState <= Print_Wait;
        --
        WHEN Print_Wait =>
---          led <= "0101";
          IF txDone = '1' THEN -- Waiting for Tx to finish printing to the terminal
            CASE reg_prevS IS
              -- This is a mapping table to figure out what the nextState is
              -- In here this is another case statement in which
              -- it looks at registers reg_prevS and reg_futS
              --
              -- Though these registers are of type StateType, they are not next states
              -- Rather, they are used as identifiers to direct the flow of the printing to the terminal
              -- e.g. to print the L command, you need a flow of - 7 dataResults in order
              -- e.g. (dR(0), space, dR(1), space ...)
              --
              -- reg_prevS is responsible for the intermidiate flow of the printing (dR(0) -> space)
              -- reg_futS is responsible for the outer flow of the state printing
              --
              -- i.e. after finishing printing the last space it goes back to IDLE state
              --
              -- [Determines A, L or P commands]
              WHEN IDLE =>
                IF reg_value = x"41" OR reg_value = x"61" THEN -- A command
                  nextState <= A_cmd; -- waits for the the fist N typed to the terminal
                ELSIF (reg_value = x"50" OR reg_value = x"70") AND reg_hasProc = TRUE THEN -- P command
                  nextState <= Print;
                ELSIF (reg_value = x"4C" OR reg_value = x"6C") AND reg_hasProc = TRUE THEN -- L command
                  nextState <= Print;
                ELSE -- If not A, P, or L
                  nextState <= IDLE;
                END IF;
              -- [---]
              -- [Printing for the L command]
              WHEN L_first =>
                nextState <= Print;
              --
              WHEN L_second =>
                nextState <= Print;
              --
              WHEN L_space =>
                nextState <= Print;
              -- [---]
              -- [Printing for the P command]
              WHEN P_2 =>
                nextState <= Print;
              --
              WHEN P_3 =>
                nextState <= Print;
              --
              WHEN P_Sp =>
                nextState <= Print;
              --
              WHEN P_i1 =>
                nextState <= Print;
              --
              WHEN P_i2 =>
                nextState <= Print;
              --
              WHEN P_i3 =>
                nextState <= Print;
              -- [---]
              -- [Printing for the A command]
              WHEN A_cmd =>
                IF reg_value = x"41" OR reg_value = x"61" THEN -- if 'a' or 'A'
                  nextState                                    <= A_cmd;
                ELSIF reg_value >= x"30" AND reg_value <= x"39" THEN -- if 0-9
                  nextState                                    <= A_N1;
                ELSIF (reg_value = x"50" OR reg_value = x"70") AND reg_hasProc = TRUE THEN -- P command
                  nextState <= Print;
                ELSIF (reg_value = x"4C" OR reg_value = x"6C") AND reg_hasProc = TRUE THEN -- L command
                  nextState <= Print;
                ELSE
                  nextState <= IDLE;
                END IF;
              --
              WHEN A_N1 =>
                IF reg_value = x"41" OR reg_value = x"61" THEN -- if 'a' or 'A'
                  nextState                                    <= A_cmd;
                ELSIF reg_value >= x"30" AND reg_value <= x"39" THEN -- 0-9
                  nextState                                    <= A_N2;
                ELSIF (reg_value = x"50" OR reg_value = x"70") AND reg_hasProc = TRUE THEN -- P command
                  nextState <= Print;
                ELSIF (reg_value = x"4C" OR reg_value = x"6C") AND reg_hasProc = TRUE THEN -- L command
                  nextState <= Print;
                ELSE
                  nextState <= IDLE;
                END IF;
              --
              WHEN A_N2 =>
                IF reg_value = x"41" OR reg_value = x"61" THEN -- if 'a' or 'A'
                  nextState                                    <= A_cmd;
                ELSIF reg_value >= x"30" AND reg_value <= x"39" THEN -- if 0-9
                  IF reg_value = x"30" AND reg_numWords_bcd(1) = x"0" AND reg_numWords_bcd(0) = x"0" THEN
                    -- check if the 3 Ns are all 0, it returns to IDLE state (i.e. ignores A000)
                    nextState <= IDLE;
                  ELSE
                    nextState <= Print;
                  END IF;
                ELSIF (reg_value = x"50" OR reg_value = x"70") AND reg_hasProc = TRUE THEN -- P command
                  nextState <= Print;
                ELSIF (reg_value = x"4C" OR reg_value = x"6C") AND reg_hasProc = TRUE THEN -- L command
                  nextState <= Print;
                ELSE
                  nextState <= IDLE;
                END IF;
              -- [---]
              -- [Printing an 'Enter']
              WHEN FEED =>
                nextState <= Print;
                --
              WHEN CARRIAGE =>
                -- Multiple 'Enter's has to be printed e.g. [print to terminal] L -> Enter -> dataResults -> Enter
                -- Looking at reg_futS we determine which state it goes to
                IF reg_futS = A_startByte THEN
                  nextState <= reg_futS;
                ELSIF reg_futS = IDLE THEN
                  nextState <= reg_futS;
                ELSIF reg_futS = P_2 THEN
                  nextState <= Print;
                ELSIF reg_futS = L_first THEN
                  nextState <= Print;
                END IF;
              -- [---]
              -- [Printing A command and the Ns]
              WHEN A_byte =>
                nextState <= Print;
              --
              WHEN A_2 =>
                nextState <= Print;
              --
              WHEN A_3 =>
                IF reg_futS = IDLE THEN
                  nextState <= Print;
                ELSE
                  nextState <= A_startByte;
                END IF;
              -- [---]
              WHEN OTHERS =>
                nextState <= IDLE;
              --
            END CASE; -- end of the look-up table on reg_prevS
          ELSE -- Wait when Tx not yet finished transmitting to the terminal
            nextState <= Print_Wait;
          END IF;
          -- End of Print_Wait
					
        --[Waits for the ANNN inputs from the terminal]
        WHEN A_cmd =>
---          led <= "0001";
          IF rxNow = '1' THEN -- Stays in the same state if no rxNow
            nextState <= Print;
          END IF;
        --
        WHEN A_N1 =>
---          led <= "0010";
          IF rxnow = '1' THEN
            nextState <= Print;
          END IF;
        --
        WHEN A_N2 =>
---          led <= "0011";
          IF rxnow = '1' THEN
            nextState <= Print;
          END IF;
        -- [---]
        -- [Getting the bytes form dataProc]
        WHEN A_startByte =>
          -- This state is used to assert start = '1',
          -- since we decided to use a Moore machine, A_startByte and A_byte the same
          -- differs only on whether 'start' is asstered or not
          -- and that A_startByte is valid for one clock cycle only
          IF dataReady = '1' AND txDone = '1' THEN
            nextState <= Print;
          ELSE
            nextState <= A_byte;
          END IF;
        --
        WHEN A_byte =>
          IF dataReady = '1' AND txDone = '1' THEN
            nextState <= Print;
          ELSE
            nextState <= A_byte;
          END IF;
        -- [---]
        WHEN OTHERS =>
          nextState <= IDLE;
        --
      END CASE; -- end of the nextState case
    END PROCESS; -- nextStateLogic
		
    -- ***Combinational Output Logic***
    ctrlOut : PROCESS (curState, rxNow, rxData, txDone, byte, seqDone, dataReady, dataResults, maxIndex,
      reg_prevS, reg_value, reg_hasProc, reg_futS, reg_listCounter)
    BEGIN
      -- Assigning default values
      -- For the output ports
      start  <= '0';
      rxDone <= '0';
      txNow  <= '0';
      txData <= x"00";
      -- Signals to enable the memory circuit for numWords_bcd and reg_numWords_bcd
      sig_n1 <= FALSE;
      sig_n2 <= FALSE;
      sig_n3 <= FALSE;
      -- Signals to enable and set the internal registers for printing
      sig_EnfutS  <= FALSE;
      sig_EnPrint <= FALSE;
      sig_prevS   <= IDLE;
      sig_futS    <= IDLE;
      sig_value   <= x"00";
      -- Signal to enable the register after a sequence has been processed
      sig_hasProc <= FALSE;
			
      -- [Outputs of the next state]
      CASE curState IS
			  -- [Setting the values to print - whateveer we type we echo]
        WHEN IDLE =>
          IF rxNow = '1' THEN
            -- Enables the registers reponsible for printing to the terminal
            sig_EnPrint <= TRUE;
            -- Directing the state machine to KNOW what to to print
            sig_prevS <= curState;
            -- Value to print to the terminal (in ASCII hex representation)
            sig_value <= rxData;
          END IF;
				-- [---]
        -- [Printing]
        WHEN Print =>
          -- Print goes for one clock cycle only because txNow has to go high for one clock cycle
          txNow  <= '1';
          txData <= reg_value; -- Put the value to be printed into txData
          rxDone <= '1'; -- Whenever we print, always acknowledges the rxData first no matter the circumstances
          CASE reg_prevS IS -- Another look-up table
            -- [Resets the endSequence register process and asserts the hasProc register process)
            WHEN CARRIAGE =>
              sig_hasProc <= TRUE;
            WHEN OTHERS =>
              NULL;
        END CASE; -- End of the look-up table on reg_prevS
        -- [---]
        -- [Still printing- but waiting for txDone and assigning other memories]
        WHEN Print_Wait =>
          -- It also determines what are the next steps after printing the current character
          -- Defines the procedure of the state machine
          txData <= reg_value;
          IF txDone = '1' THEN
            CASE reg_prevS IS
						  -- [Determining what command it is and directs the pattern of printing]
              WHEN IDLE =>
                IF (reg_value = x"50" OR reg_value = x"70") AND reg_hasProc = TRUE THEN -- P command
                  -- Enables the registers reponsible for printing to the terminal
                  sig_EnPrint <= TRUE;
                  -- Directing the state machine to KNOW what to to print
                  sig_prevS <= FEED;
                  -- Value to print to the terminal (in ASCII hex representation)
                  sig_value <= x"0A";
                  -- Enables the register reg_EnfutS
                  sig_EnfutS <= TRUE;
                  -- Determines where to go (i.e. after printing a 'P' -> Enter,
                  -- the program looks at reg_futS so it knows to print {dataResult(3) and maxIndex}
                  sig_futS <= P_2;
                ELSIF (reg_value = x"4C" OR reg_value = x"6C") AND reg_hasProc = TRUE THEN -- L command
                  sig_EnPrint <= TRUE;
                  sig_prevS   <= FEED;
                  sig_value   <= x"0A";
                  sig_EnfutS  <= TRUE;
                  sig_futS    <= L_first;
                END IF;
              -- [---]
              -- [Printing dataResults(0-7)]
              WHEN L_first =>
                sig_EnPrint <= TRUE;
                sig_prevS   <= L_second;
                sig_value   <= byteToHex(dataResults(reg_listCounter)(3 DOWNTO 0));
                sig_EnfutS  <= TRUE;
                sig_futS    <= IDLE;
              --
              WHEN L_second =>
                sig_EnPrint <= TRUE;
                sig_prevS   <= L_space;
                sig_value   <= x"20";
              --
              WHEN L_space =>
                IF reg_listCounter = 7 THEN -- End of the 7 bytes to be printed, so print Enter instead
                  sig_EnPrint <= TRUE;
                  sig_prevS   <= FEED;
                  sig_value   <= x"0A";
                ELSE
                  sig_EnPrint <= TRUE;
                  sig_prevS   <= L_first;
                  sig_value   <= byteToHex(dataResults(reg_listCounter)(7 DOWNTO 4));
                END IF;
              -- [---]
              -- [Printing dataResults(3) and MaxIndex}
              WHEN P_2 =>
                sig_EnPrint <= TRUE;
                sig_prevS   <= P_3;
                sig_value   <= byteToHex(dataResults(3)(3 DOWNTO 0));
                sig_EnfutS  <= TRUE;
                sig_futS    <= IDLE;
              --
              WHEN P_3 =>
                sig_EnPrint <= TRUE;
                sig_prevS   <= P_Sp;
                sig_value   <= x"20";
              --
              WHEN P_Sp =>
                sig_EnPrint <= TRUE;
                sig_prevS   <= P_i1;
                sig_value   <= x"3" & maxIndex(2);
              --
              WHEN P_i1 =>
                sig_EnPrint <= TRUE;
                sig_prevS   <= P_i2;
                sig_value   <= x"3" & maxIndex(1);
              --
              WHEN P_i2 =>
                sig_EnPrint <= TRUE;
                sig_prevS   <= P_i3;
                sig_value   <= x"3" & maxIndex(0);
              --
              WHEN P_i3 =>
                sig_EnPrint <= TRUE;
                sig_prevS   <= FEED;
                sig_value   <= x"0A";
              -- [---]
              -- [Printing Enter]
              WHEN FEED =>
                sig_EnPrint <= TRUE;
                sig_prevS   <= CARRIAGE;
                sig_value   <= x"0D";
              --
              WHEN CARRIAGE =>
                IF reg_futS = P_2 THEN
                  sig_EnPrint <= TRUE;
                  sig_prevS   <= reg_futS; -- afer enter do what the peak command
                  sig_value   <= byteToHex(dataResults(3)(7 DOWNTO 4));
                ELSIF reg_futS = L_first THEN
                  sig_EnPrint <= TRUE;
                  sig_prevS   <= reg_futS; -- afer enter do what the peak command
                  sig_value   <= byteToHex(dataResults(reg_listCounter)(7 DOWNTO 4));
                END IF;
              -- [---]
              -- [Printing 'ANNN' and the bytes from datProc to terminal]
              WHEN A_N2 => -- the third N
                IF reg_value >= x"30" AND reg_value <= x"39" THEN
                  sig_EnPrint <= TRUE;
                  sig_prevS   <= FEED;
                  sig_value   <= x"0A";
                  sig_EnfutS  <= TRUE;
                  sig_futS    <= A_startByte;
                END IF;
              --
              WHEN A_byte => -- the first half of byte
                sig_EnPrint <= TRUE;
                sig_prevS   <= A_2;
                sig_value   <= byteToHex(byte(3 DOWNTO 0));
              --
              WHEN A_2 => -- the second half of byte
                sig_EnPrint <= TRUE;
                sig_prevS   <= A_3;
                sig_value   <= x"20";
              --
              WHEN A_3 => -- the space
                IF reg_futS = IDLE THEN -- before the seqDone is asserted the value is A_startbyte
                  sig_EnPrint <= TRUE;
                  sig_prevS   <= FEED;
                  sig_value   <= x"0A";
                END IF;
              -- [---]
              WHEN OTHERS =>
                NULL; -- the defaults are assigned above
              --
            END CASE; -- End of the look-up table on reg_prevS
          END IF; -- if txDone = '1'
        -- End of Print_Wait
				
        -- [Waiting for inputs from A command]
        WHEN A_cmd =>
          IF rxnow = '1' THEN
            sig_n3      <= TRUE;
            sig_EnPrint <= TRUE;
            sig_prevS   <= curState;
            sig_value   <= rxData;
            -- Cater for e.g. ap
            IF ((rxData = x"50" OR rxData = x"70") OR (rxData = x"4C" OR rxData = x"6C"))
             AND reg_hasProc = TRUE THEN -- L & P command
              sig_prevS <= IDLE;
            END IF;
          END IF;
        --
        WHEN A_N1 =>
          IF rxnow = '1' THEN
            sig_n2      <= TRUE;
            sig_EnPrint <= TRUE;
            sig_prevS   <= curState;
            sig_value   <= rxData;
            -- Cater for e.g. a0p
            IF ((rxData = x"50" OR rxData = x"70") OR (rxData = x"4C" OR rxData = x"6C"))
              AND reg_hasProc = TRUE THEN -- L & P command
              sig_prevS <= IDLE;
            END IF;
          END IF;
        --
        WHEN A_N2 =>
          IF rxnow = '1' THEN
            sig_n1      <= TRUE;
            sig_EnPrint <= TRUE;
            sig_prevS   <= curState;
            sig_value   <= rxData;
            -- Cater for e.g. a00p
            IF ((rxData = x"50" OR rxData = x"70") OR (rxData = x"4C" OR rxData = x"6C"))
              AND reg_hasProc = TRUE THEN -- L & P command
              sig_prevS <= IDLE;
            END IF;
          END IF;
        -- [---]
				
        -- [Getting the value of 'byte' port from dataProc]
        WHEN A_startByte =>
          start <= '1'; -- start is asserted for one clock cycle only
        --
        WHEN A_byte =>
          start <= '0'; -- stating this for clarity only, can be removed as default is '0'
          IF dataReady = '1' AND txDone = '1' THEN -- data has to be ready
            IF seqDone = '1' THEN -- Asserts the memory circuit to remember it is the last byte
              sig_EnfutS <= TRUE;
              sig_futS   <= IDLE;
            END IF;
            sig_EnPrint <= TRUE;
            sig_prevS   <= curState;
            sig_value   <= byteToHex(byte(7 DOWNTO 4));
          END IF;
        -- [---]
        WHEN OTHERS =>
          NULL;
    --
    END CASE; -- End of the combinational output
  END PROCESS; -- ctrlOut
END cmd; -- cmd/cmdProc