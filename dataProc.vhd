LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.std_logic_unsigned.ALL;
USE ieee.numeric_std.ALL;
USE work.common_pack.ALL;

ENTITY dataConsume IS
	PORT (
		clk          : IN std_logic := 'X';
		reset        : IN std_logic := 'X';
		start        : IN std_logic := 'X';
		numWords_bcd : IN BCD_ARRAY_TYPE(2 DOWNTO 0);
		ctrlIn       : IN std_logic := 'X';
		ctrlOut      : OUT std_logic := 'X';
		data         : IN std_logic_vector(7 DOWNTO 0);
		dataReady    : OUT std_logic;
		byte         : OUT std_logic_vector(7 DOWNTO 0);
		seqDone      : OUT std_logic;
		maxIndex     : OUT BCD_ARRAY_TYPE(2 DOWNTO 0);
---		led			 : OUT std_logic_vector(7 downto 4); --LED is used to debug the program, as it can show at which state the system enters infinity loop
		dataResults  : OUT CHAR_ARRAY_TYPE(0 TO RESULT_BYTE_NUM - 1)
	);
END dataConsume;

--********************************************USE OF STATES AND ZONES***********************************************
--The design consists of two mealy state machines (stateProcessor and zoneProcessor) to optimize the run time. 
--stateProcessor:
--The system will starts at the IDLE state, when it receives a 'start' signal from the command processor, the processor will start to request
--for data from the data generator and process the byte, store and update relevant information into the register.
--zoneProcessor:
--Responsible for the seqDone and DataResult outputs
--The detail function of each processors will be explained at stateProcessor, zoneProcessor (bottom of the page).
--***********************************************USE OF REGISTERS***************************************************
--A lot of registers are used in the data processor to prevent directly linking the input and output port, and to build a memory circuit.
--The information from the input port are stored into register first, after processing, they are output to the output port at appropriate time.
--Serval advantages of the mechanism are:avoid latches, ensure all output terminals will have define values at all time, and more control over
--when to read the data from the input port and when to output the values.
--Each registers are controlled by their own set of reset and enable signals. The naming convention
--are: "(function of register/name of data)"_reg_(enable/reset) except for the counters, in which "reg" is omitted in the name.

ARCHITECTURE behav OF dataConsume IS
 
	TYPE state_type IS (IDLE, COMPARESTATE, FINISHANALYSIS);
	SIGNAL curState         : state_type := IDLE;
	SIGNAL nextState        : state_type := IDLE;
	SIGNAL prevState        : state_type := IDLE;
 
	TYPE zone_type IS (WAITZONE, UPDATEZONE);
	SIGNAL curZone           : zone_type := WAITZONE;
	SIGNAL nextZone          : zone_type := WAITZONE;
 
	SIGNAL dataResult_buffer : CHAR_ARRAY_TYPE(0 TO ((RESULT_BYTE_NUM - 1)/2 - 1));
	SIGNAL peakHappened      : BOOLEAN := FALSE;
	SIGNAL ctrlIn_delayed    : std_logic := '0';
	SIGNAL ctrlIn_detected   : std_logic := '0';
 
	--*****************************************register***************************************** 
	SIGNAL byte_reg              : std_logic_vector(7 DOWNTO 0) := "00000000";
	SIGNAL counter_bcd           : BCD_ARRAY_TYPE(2 DOWNTO 0) := ("0000", "0000", "0000");
	SIGNAL counter_peak          : INTEGER RANGE 0 TO 15 := 0;
	SIGNAL ctrlIn_delayed_reg    : std_logic := '0';
	SIGNAL ctrlIn_detected_reg   : std_logic := '0';
	SIGNAL ctrlOut_reg           : std_logic := '0';
	SIGNAL dataResult_buffer_reg : CHAR_ARRAY_TYPE(0 TO ((RESULT_BYTE_NUM - 1)/2 - 1)) := ("11111111", "11111111", "11111111");
	SIGNAL dataResult_reg        : CHAR_ARRAY_TYPE(0 TO RESULT_BYTE_NUM - 1) := ("11111111", "11111111", "11111111", "11111111", "11111111", "11111111", "11111111");
	SIGNAL numWords_bcd_reg      : BCD_ARRAY_TYPE(2 DOWNTO 0) := ("0000", "0000", "0000");
	SIGNAL maxIndex_reg          : BCD_ARRAY_TYPE(2 DOWNTO 0);
	SIGNAL peakHappened_reg      : BOOLEAN := FALSE;
	SIGNAL seqDone_reg           : std_logic := '0';

	--**************************************register enable**************************************
	SIGNAL byte_reg_enable               : std_logic := '0';
	SIGNAL counter_enable                : std_logic := '0';
	SIGNAL counter_start                 : std_logic := '0';
	SIGNAL counter_peak_enable           : std_logic := '0';
	SIGNAL ctrlIn_delayed_reg_enable     : std_logic := '0';
	SIGNAL ctrlIn_detected_reg_enable    : std_logic := '0';
	SIGNAL ctrlOut_reg_enable            : std_logic := '0';
	SIGNAL dataResult_buffer_reg_enable  : std_logic := '0';
	SIGNAL dataResult_reg_enable_AtP     : std_logic := '0';
	SIGNAL dataResult_reg_enable_PostP   : std_logic := '0';
	SIGNAL numWords_bcd_reg_enable       : std_logic := '0';
	SIGNAL maxIndex_reg_enable           : std_logic := '0';
	SIGNAL peakHappened_reg_enable_AtP   : std_logic := '0';
	SIGNAL peakHappened_reg_enable_PostP : std_logic := '0';
	SIGNAL prevState_enable              : std_logic := '0';
	SIGNAL seqDone_reg_enable            : std_logic := '0';
	SIGNAL seqDone_output_enable         : std_logic := '0';
 
	--**************************************register reset**************************************
	SIGNAL byte_reg_reset       : std_logic := '0';--
	SIGNAL counter_peak_reset   : std_logic := '0';--
	SIGNAL counter_reset        : std_logic := '0';--
	SIGNAL dataResult_reg_reset : std_logic := '0';--
 
BEGIN
	----------------------------------------------------------------------------------------------------------------------------------------
	--REGISTER Reg_byte is used to store the input data from the signal generator.
	--It will be updated when byte_reg_enable is on and is at raising clock edge
	Reg_byte : PROCESS (clk, reset)
	BEGIN
		IF clk'EVENT AND clk = '1' THEN
			IF reset = '1' OR byte_reg_reset = '1' THEN
				byte_reg <= "00000000";
			ELSIF byte_reg_enable = '1' THEN
				byte_reg <= data;
			END IF;
		END IF;
	END PROCESS;
	----------------------------------------------------------------------------------------------------------------------------------------
	--REGISTER Reg_dataResult_buffer is used to store the previous three bytes encountered by the system.
	--This is to prepared that at any time, the incoming byte might be the peak byte, and would require the data
	--of the previous three byte of it to be stored into dataResult_reg(0-2)	
	Reg_dataResult_buffer : PROCESS (clk, reset)
	BEGIN
		IF clk'EVENT AND clk = '1' THEN
			IF reset = '1' THEN
				dataResult_buffer_reg(0) <= "00000000";
				dataResult_buffer_reg(1) <= "00000000";
				dataResult_buffer_reg(2) <= "00000000";
			ELSIF dataResult_buffer_reg_enable = '1' THEN
				dataResult_buffer_reg(0) <= dataResult_buffer(1);
				dataResult_buffer_reg(1) <= dataResult_buffer(2);
				dataResult_buffer_reg(2) <= byte_reg;
			END IF;
		END IF;
	END PROCESS;
	----------------------------------------------------------------------------------------------------------------------------------------
	--REGISTER Reg_dataResult is used to store the peak byte, and three bytes before and after it.
	--At Peak Enable: dataResult(3)[The peak byte] is updated whever the current data is larger then the previous byte. At the same
	--time, the data from dataResult_buffer is also stored into the dataResult as before peak byte.
	--Post Peak Enable: the processor stores the current byte(which are classified as post peak case) into the correct position of
	--the dataResult array. The position is calculated by the realtive position from the peak(given by the peak counter) + 4, as the
	--first post byte peak is stored at dataResult(4)
	Reg_dataResult : PROCESS (clk, reset)
	BEGIN
		IF clk'EVENT AND clk = '1' THEN
			IF reset = '1' OR dataResult_reg_reset = '1' THEN
				dataResult_reg(0) <= "11111111";
				dataResult_reg(1) <= "11111111";
				dataResult_reg(2) <= "11111111";
				dataResult_reg(3) <= "11111111";
				dataResult_reg(4) <= "11111111";
				dataResult_reg(5) <= "11111111";
				dataResult_reg(6) <= "11111111";
			ELSIF dataResult_reg_enable_AtP = '1' THEN
				dataResult_reg(3)      <= byte_reg;
				dataResult_reg(0 TO 2) <= dataResult_buffer(0 TO 2);
			ELSIF dataResult_reg_enable_PostP = '1' THEN
				dataResult_reg(4 + counter_peak) <= byte_reg;
			END IF;
		END IF;
	END PROCESS;
	----------------------------------------------------------------------------------------------------------------------------------------
	--REGISTER Reg_peakHappened is used remember whether one of the last three bytes was a peak such that
	--When the sytem detects the current data is smaller than the previous data, it can decided whether it is a before peak or post peak situation
	--if peak did happened in during the last three byte, it will be a post peak situation.
	--There are two enable signals for the register, one update "peakHappened" to true when the processor goes to the "AtPeak" State,
	--the other one update "peakHappened" to False, when the processor process the last post peak byte (the last byte of dataResult),
	--becuase the next byte will no longer be a post peak case.
	Reg_peakHappened : PROCESS (clk, reset)
	BEGIN
		IF clk'EVENT AND clk = '1' THEN
			IF reset = '1' THEN
				peakHappened_reg <= False;
			ELSIF peakHappened_reg_enable_AtP = '1' THEN
				peakHappened_reg <= True;
			ELSIF peakHappened_reg_enable_PostP = '1' THEN
				IF counter_peak = 2 THEN
					peakHappened_reg <= False;
				END IF;
			END IF;
		END IF;
	END PROCESS;
	----------------------------------------------------------------------------------------------------------------------------------------
	--REGISTER Reg_maxIndex is used to store the max Index. Whenever the system is at peak, the current counter value will be stored into this register
	Reg_maxIndex : PROCESS (clk, reset)
	BEGIN
		IF clk'EVENT AND clk = '1' THEN
			IF reset = '1' THEN
				maxIndex_reg(0) <= "0000";
				maxIndex_reg(1) <= "0000";
				maxIndex_reg(2) <= "0000";
			ELSIF maxIndex_reg_enable = '1' THEN
				maxIndex_reg(2 DOWNTO 0) <= counter_bcd(2 DOWNTO 0);
			END IF;
		END IF;
	END PROCESS;
	----------------------------------------------------------------------------------------------------------------------------------------
	--REGISTER Reg_numWords is used to store the numWords infomation given by the command processor. This can avoid generating latches. 
	Reg_numWords : PROCESS (clk, reset)
	BEGIN
		IF clk'EVENT AND clk = '1' THEN
			IF reset = '1' THEN
				numWords_bcd_reg <= ("0000", "0000", "0000");
			ELSIF numWords_bcd_reg_enable = '1' THEN
				numWords_bcd_reg <= numWords_bcd;
			END IF;
		END IF;
	END PROCESS;
	----------------------------------------------------------------------------------------------------------------------------------------
	--REGISTER Reg_sequenceDone is used to detect whether the system has reached the end of the sequence. It is enabled after receiving 
	--the ctrlIn signal. Binary subtraction is conducted, where the value of (numWords - 1) is checked against the current counter value.
	--If they are equal, it means that sequence is done. It is (numWords - 1) becuase the counter starts counting from 0.
	--This information will be outputted to the terminal by the ZONE Processor at the first clock cycle of the FINISH ANALYSIS STATE
	Reg_sequenceDone         : PROCESS (clk, reset)
		VARIABLE temp            : BCD_ARRAY_TYPE(2 DOWNTO 0);
		VARIABLE carry1, carry10 : std_logic_vector(3 DOWNTO 0);
	BEGIN
		IF clk'EVENT AND clk = '1' THEN
			IF reset = '1' THEN
				seqDone_reg <= '0';
			ELSIF seqDone_reg_enable = '1' THEN
			--carries are introduced as direct subtraction might not always be possible e.g. in the case 1000 - 0001
				carry1  := "0000";
				carry10 := "0000";
				temp(0) := "0000";
				temp(1) := "0000";
				temp(2) := "0000";
				IF numWords_bcd_reg(0) < "0001" THEN
					temp(0) := numWords_bcd_reg(0) + "1010" - "0001";
					carry1  := "0001";
				ELSE
					temp(0) := numWords_bcd_reg(0) - "0001";
				END IF;
				IF numWords_bcd_reg(1) < carry1 THEN
					temp(1) := numWords_bcd_reg(1) + "1010" - carry1;
					carry10 := "0001";
				ELSE
					temp(1) := numWords_bcd_reg(1) - carry1;
				END IF;
				temp(2) := numWords_bcd_reg(2) - carry10;
				IF (counter_bcd(2) = temp(2)) AND (counter_bcd(1) = temp(1)) AND (counter_bcd(0) = temp(0)) THEN
					seqDone_reg <= '1';
				ELSE
					seqDone_reg <= '0';
				END IF;
			END IF;
		END IF;
	END PROCESS;
	----------------------------------------------------------------------------------------------------------------------------------------
	--REGISTER Reg_counter_bcd is a binary counter used count the index of the incoming byte. Binary addition,involving carries, is carried out. The system is programmed to 
	--count the first data as index 0. Thus, there are two types of enable in the counter to allow two different algorithms to be used, one for the first data case, 
	--one for other generic cases. Enable signals are only enabled for one clock cycle in every byte cycle, to ensure correct counting. They are asserted, when the
	--mealy state machines (STATE PROCESSORS) enters the COMPARE STATE
	Reg_counter_bcd_process                  : PROCESS (clk, reset)
		VARIABLE counter1, counter10, counter100 : std_logic_vector(3 DOWNTO 0);
	BEGIN
		IF clk'EVENT AND clk = '1' THEN
			IF reset = '1' OR counter_reset = '1' THEN
				counter_bcd(0) <= "0000";
				counter_bcd(1) <= "0000";
				counter_bcd(2) <= "0000";
				
			--algorithms for normal cases : counter_bcd <= counter_bcd + 1
			ELSIF counter_enable = '1' THEN
				counter1   := counter_bcd(0);
				counter10  := counter_bcd(1);
				counter100 := counter_bcd(2);
				IF counter1 < "1001" THEN
					counter1 := counter1 + 1;
				ELSIF counter1 >= "1001" THEN
					counter10 := counter10 + 1;
					counter1  := "0000";
				END IF;
				IF counter10 > "1001" THEN
					counter100 := counter100 + 1;
					counter10  := "0000";
				END IF;
				counter_bcd(0) <= counter1;
				counter_bcd(1) <= counter10;
				counter_bcd(2) <= counter100;
				
			--algorithms for the first data: counter_bcd <= counter_bcd
			--since the reset value of the counters are 0, if in the first data case, the (counter_bcd <= counter_bcd + 1) equation will store a 1 instead of a 0
			ELSIF counter_start = '1' THEN
				counter_bcd <= counter_bcd;
			END IF;
		END IF;
	END PROCESS;
	----------------------------------------------------------------------------------------------------------------------------------------
	--REGISTER Reg_counter_peak is a integer counter used to specify the relative position of the current byte from the last encountered peak.
	--It is enabled whether the system is in the At Peak case and reseted whenever the system is at Before Peak Case. 
	--Its main function is to be used in this line of code in the Post Peak case: cuurent byte = dataResult_reg(4+counter_peak). 
	--However, as the dataResult_reg is of 7 byte size only, it is imporant when counter peak >2, the system will not encountered the above line of code.
	--Therefore, when counter_peak =2, i.e. it is at the last post peak byte, it will set peakHappened_reg = False, then the system will no longer 
	--perceive the future incoming bytes as post peak byte, unless a peak byte is encountered again.
	Reg_counter_peak : PROCESS (clk, reset)
	BEGIN
		IF clk'EVENT AND clk = '1' THEN
			IF reset = '1' OR counter_peak_reset = '1' THEN
				counter_peak <= 0;
			ELSIF counter_peak_enable = '1' THEN
				counter_peak <= counter_peak + 1;
			END IF;
		END IF;
	END PROCESS;
	----------------------------------------------------------------------------------------------------------------------------------------
	--REGISTER: ctrlIn_delayed_reg, ctrlIn_detected_reg is used to detect where the ctrlIn data line has any transitions. ctrlIn_delayed_reg stored
	--the current value of ctrlIn. ctrlIn_detected made use of xor gate to compare the current ctrlIn value with ctrlIn_delayed, and output a high signal
	--when there is a transition.
	memory_CtrlIn : PROCESS (clk, reset)
	BEGIN
		IF rising_edge(clk) THEN
			IF reset = '1' THEN
				ctrlIn_delayed_reg  <= '0';
				ctrlIn_detected_reg <= '0';
			ELSIF ctrlIn_delayed_reg_enable = '1' OR ctrlIn_detected_reg_enable = '1' THEN
				ctrlIn_delayed_reg  <= ctrlIn;
				ctrlIn_detected_reg <= ctrlIn XOR ctrlIn_delayed;
			END IF;
		END IF;
	END PROCESS;
	----------------------------------------------------------------------------------------------------------------------------------------
	--REGISTER: ctrlOut_reg is used to output transition on the ctrlOut data line, as a request for data to the data Generator. 
	--ctrlOut_reg stored the current ctrl_Out value, whenever transition is required, it NOT this value and stores it into ctrlOut again. 
	ouput_CtrlOut : PROCESS (clk, reset)
	BEGIN
		IF rising_edge(clk) THEN
			IF reset = '1' THEN
				ctrlOut_reg <= '0';
			ELSIF ctrlOut_reg_enable = '1' THEN 
				ctrlOut_reg <= NOT ctrlOut_reg;
			END IF;
		END IF;
	END PROCESS;
	----------------------------------------------------------------------------------------------------------------------------------------
	--At every clock cycle, the outputs will equal to the value of its respective registers. This ensure that output are always defined, hence avoid latches	
	byte              <= byte_reg;
	maxIndex          <= maxIndex_reg;
	dataResults       <= dataResult_reg;
	ctrlOut           <= ctrlOut_reg;
	byte              <= byte_reg;
	peakHappened      <= peakHappened_reg;
	dataResult_buffer <= dataResult_buffer_reg;
	ctrlIn_delayed    <= ctrlIn_delayed_reg;
	----------------------------------------------------------------------------------------------------------------------------------------
	--Reg_stateNzone is used to set the nextState, nextZone of the two mealy machines(STATE and ZONE Processor)
	--It is also used to store the previous state data when enabled. 
	Reg_stateNzone : PROCESS (clk, reset)
	BEGIN
		IF clk'EVENT AND clk = '1' THEN
			IF reset = '1' THEN
				curState  <= IDLE;
				prevState <= IDLE;
				curZone   <= WAITZONE;
			ELSE
			--prevState_enable is asserted whenever there is transition of states, with the exception that it is always asserted at the FINSIH ANALYSIS STATE, 
			--such that "previous state = COMPARE STATE and current state = FINISH ANALYSIS" will only be valid for one clock cycle
				IF prevState_enable = '1' THEN
					prevState <= curState;
				END IF;
				curZone  <= nextZone;
				curState <= nextState;
			END IF;
		END IF;
	END PROCESS;
--***********************************************************ZONE Processor**************************************************************
	--Zone Processor is responsible for outputting dataReady and seqDone. It is stated that only when previous state equals to COMPARE STATE 
	--and current state equals to FINISH ANALYSIS, the output will be asserted. And this particular condition is only true at the first clock cycle of the FINISH ANALYSIS state.
	--At any other time, the processor will stay in the WAITZONE.
	
	--This extra processor is introduced because allow simultinuous runnning of the FINSIH ANALYSIS STATE and outputting dataReady and seqDone, hence minimize cycle count
	--Initially, there is a seqDone state in the stateProcessor to output the dataReady and seqDone, however, this would mean introducing 
	--one additional clock cycle to every byte, if we are going to process 1000 bytes, these would mean an addition of 1000 cycle counts. 
	--Furthermore, it is spotted that there is clearly inefficiency in seqDone and finish analysis state. Because the system is actually just waiting 
	--for the start byte at Finish Analysis State. The outputs is not asserted at finish analysis state because FINSIH ANALYSIS STATE might last
	--more than 1 clock cycle, but we only need the output to be high for one clock cycle. 
	processor_nextZone : PROCESS (curZone, nextZone, curState, prevState, dataResult_reg, seqDone_reg)
	BEGIN
		dataReady <= '0';
		seqDone   <= '0';
		CASE curZone IS
			WHEN WAITZONE => 
				IF curState = FINISHANALYSIS AND prevState = COMPARESTATE THEN
					dataReady <= '1';
					seqDone   <= seqDone_reg;
					nextZone  <= UPDATEZONE;
				ELSE
					nextZone <= WAITZONE;
				END IF;
			WHEN UPDATEZONE => 
				nextZone <= WAITZONE;
		END CASE;
	END PROCESS;
--***********************************************************STATE Processor**************************************************************
	--State Processor
	--Mealy approach allow more efficient use of states, hence less states and optimize the system.
	--System Cycle: IDLE -> COMPARESTATE -> FINISHANALYSIS -> COMPARESTATE (Then alternate between COMPARESTATE -> FINISHANALYSIS until the seqDone, and the system will go back to IDLE)
	processor_nextState : PROCESS (curState, start, ctrlIn, ctrlIn_detected_reg, byte_reg, dataResult_reg, peakHappened, seqDone_reg)
	BEGIN
		--All register enables, resets and outputs are assigned with default values, their values will only be changed if they are specified in the states
		--This approach avoid latches
		numWords_bcd_reg_enable       <= '0';
		byte_reg_enable               <= '0';
		dataResult_reg_enable_PostP   <= '0';
		dataResult_reg_enable_AtP     <= '0';
		maxIndex_reg_enable           <= '0';
		peakHappened_reg_enable_AtP   <= '0';
		peakHappened_reg_enable_PostP <= '0';
		dataResult_buffer_reg_enable  <= '0';
		seqDone_reg_enable            <= '0';
		ctrlIn_delayed_reg_enable     <= '0';
		ctrlIn_detected_reg_enable    <= '0';
		counter_peak_enable           <= '0';
		ctrlOut_reg_enable            <= '0';
		prevState_enable              <= '1';--special case as it is used to control the Zone Processor
		
		byte_reg_reset                <= '0';
		dataResult_reg_reset          <= '0';
		
		-- leds
---		led                           <= "1010";
		
		--counters are responsible of counting the number of byte processed, hence within every byte cycle, they are only enabled for ONE clock cycle upon receiving the start signal
		counter_peak_reset            <= '0';
		counter_enable                <= '0';
		counter_reset                 <= '0';
		counter_start                 <= '0';
		CASE curState IS
 
			WHEN IDLE => 
				counter_reset      <= '1';--counter is resetted at IDLE state
				counter_peak_reset <= '1';
				IF start = '1' THEN
					dataResult_reg_reset <= '1'; --dataResult is only resetted after receving start signal, as Cmd processor might wish to obtain the value of the outputs in the IDLE state before the next sequence starts.
					counter_start        <= '1'; --[counter]the counter_bcd enable signal for the first data 
					ctrlOut_reg_enable   <= '1'; --[output ctrlOut]requesting data from data gen
					nextState            <= COMPARESTATE;
				ELSE
					nextState <= IDLE;
				END IF;
---				led                    <= "0101";

			WHEN COMPARESTATE =>
			--main processing of data happens in this state.
				ctrlIn_delayed_reg_enable  <= '1'; --[checking ctrlIn]to store the ctrlin value
				ctrlIn_detected_reg_enable <= '1'; --[checking ctrlIn] to compare current ctrlIn value with the previous ctrlIn value
				byte_reg_enable            <= '1'; -- enabled at all time in this state, byte_reg will be updated when data is successfully obtained
				numWords_bcd_reg_enable    <= '1'; -- enabled at all time in this state, numWords_bcd_reg will be updated when data is successfully obtained
				seqDone_reg_enable         <= '1'; 
				prevState_enable           <= '0';
 
				IF ctrlIn_detected_reg = '1' THEN--indicate that we have obtained the data
					--The data are intepreted as SIGNED MAGNITUDE. 
					--This If statement compared the value of the current byte with the previous byte, in order to identify whether it is a AT PEAK, POST PEAK or BEFORE PEAK CASE. 
					--By comparing the sign bit of the curent byte sign with the previous byte, there are 9 possible combinations of data. In most combinations, 
					--we can directly distinguish which data is of bigger values, and only in two combinations, actual comparison of the remaining bits are required.
					--
					--ACTIONS TO BE DONE AT EACH CASES:
					--******************************************************[AT PEAK]******************************************************
					--1. storing the current byte value to dataResult position 3
					--2. buffer to dataResult position 0-2
					--3. shift and update the buffer
					--*****************************************************[POST PEAK]******************************************************
					--1. store the current byte to either dataResult position 4 or 5 or 6
					--2. shift and update the buffer
					--****************************************************[BEFORE PEAK]******************************************************
					--1. shift and update the buffer
					--
					--when both are positive numbers, we compare the value of the remaining bits
					IF byte_reg(7) = '0' AND dataResult_reg(3)(7) = '0' THEN
						IF (byte_reg(6 DOWNTO 0) > dataResult_reg(3)(6 DOWNTO 0)) OR (byte_reg(6 DOWNTO 0) = dataResult_reg(3)(6 DOWNTO 0)) THEN
							--[AT PEAK]
							dataResult_reg_enable_AtP    <= '1';
							maxIndex_reg_enable          <= '1';
							peakHappened_reg_enable_AtP  <= '1';
							counter_peak_reset           <= '1';
							dataResult_buffer_reg_enable <= '1';
						ELSIF byte_reg(6 DOWNTO 0) < dataResult_reg(3)(6 DOWNTO 0) AND peakHappened = True THEN
							--[POSTPEAK]
							dataResult_reg_enable_PostP   <= '1';
							peakHappened_reg_enable_PostP <= '1';
							counter_peak_enable           <= '1';
							dataResult_buffer_reg_enable  <= '1';
						ELSE
							--[BEFORE PEAK]
							counter_peak_reset           <= '1';
							dataResult_buffer_reg_enable <= '1';
						END IF;
						
					--when both are negative numbers, we compare the magnitude the remaining bits, and inverse the result as the final comparison output
					ELSIF byte_reg(7) = '1' AND dataResult_reg(3)(7) = '1' THEN
						IF byte_reg(6 DOWNTO 0) < dataResult_reg(3)(6 DOWNTO 0) THEN
							--[AT PEAK]
							dataResult_reg_enable_AtP    <= '1';
							maxIndex_reg_enable          <= '1';
							peakHappened_reg_enable_AtP  <= '1';
							counter_peak_reset           <= '1';
							dataResult_buffer_reg_enable <= '1';
						ELSIF byte_reg(6 DOWNTO 0) > dataResult_reg(3)(6 DOWNTO 0) AND peakHappened = True THEN
							--[POSTPEAK]
							dataResult_reg_enable_PostP   <= '1';
							peakHappened_reg_enable_PostP <= '1';
							counter_peak_enable           <= '1';
							dataResult_buffer_reg_enable  <= '1';
						ELSE
							--[BEFORE PEAK]
							counter_peak_reset           <= '1';
							dataResult_buffer_reg_enable <= '1';
						END IF;
					
					--For current data >= previous data
					--Possible combination:(current byte=positive and previous byte=negative), (current byte=positive and previous byte=0), (current byte=0 and previous byte=negative), (current byte=0 and previous byte=0)
					--only involve comparision of sign
					ELSIF (byte_reg(7) = '0' AND dataResult_reg(3)(7) = '1') OR (byte_reg(7) = '0' AND dataResult_reg(3) = x"0") OR (byte_reg = x"0" AND dataResult_reg(3)(7) = '1') OR (byte_reg = x"0" AND dataResult_reg(3) = x"0") THEN
						--[AT PEAK]
						dataResult_reg_enable_AtP    <= '1';
						maxIndex_reg_enable          <= '1';
						peakHappened_reg_enable_AtP  <= '1';
						counter_peak_reset           <= '1';
						dataResult_buffer_reg_enable <= '1';
					
					--For current data < previous data					
					--Possible combination:(current byte=negative and previous byte=positive), (current byte=negative and previous byte=0),(current byte=0 and previous byte=0)
					--only involve comparision of sign
					ELSIF (byte_reg(7) = '1' AND dataResult_reg(3)(7) = '0') OR (byte_reg(7) = '1' AND dataResult_reg(3) = x"0") THEN
						--when the current byte is smaller than the previous byte, it is either at Post Peak or Before Peak?
						IF peakHappened = True THEN --If one of the three previous bytes is a peak
							--[POST PEAK]
							dataResult_reg_enable_PostP   <= '1';
							peakHappened_reg_enable_PostP <= '1';
							counter_peak_enable           <= '1';
							dataResult_buffer_reg_enable  <= '1';
						ELSE --If none of the three previous bytes is a peak
							--[BEFORE PEAK]
							counter_peak_reset           <= '1';
							dataResult_buffer_reg_enable <= '1';
						END IF;
					END IF;
					prevState_enable <= '1'; --previous state register is enabled here, such that previous state in the next clock cycle will be COMPARESTATE
					nextState        <= FINISHANALYSIS;
				ELSE
					prevState_enable <= '1';
					nextState        <= COMPARESTATE;
				END IF;
 
			WHEN FINISHANALYSIS => 
			--if seq is Done, it will directly enter IDLE state, else it will be halted here to wait for the next start signal
				prevState_enable <= '1'; --prevState_enable is always enable in this state, such that previous state will equals to FINISH ANALYSIS at the second clock cycle of FINISH ANALYSIS, to ensure ZONE PROCESSOR will not stay in the UPDATEZONE
				IF seqDone_reg = '1' THEN
					nextState <= IDLE;
				ELSIF seqDone_reg = '0' AND start = '1' THEN
					counter_enable     <= '1'; --counter is only enable during the state transition period, this ensure it is only enabled for 1 clock cycle. 
					ctrlOut_reg_enable <= '1'; 
					nextState          <= COMPARESTATE;
				ELSE
					nextState <= FINISHANALYSIS;
				END IF;
			WHEN OTHERS => 
				nextState <= IDLE;
		END CASE;
	END PROCESS;
END behav;
