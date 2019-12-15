LIBRARY IEEE;
   USE IEEE.STD_LOGIC_1164.ALL;
   USE IEEE.NUMERIC_STD.ALL;
   USE IEEE.STD_LOGIC_MISC.ALL;
   USE IEEE.STD_LOGIC_ARITH.ALL;
   USE IEEE.STD_LOGIC_UNSIGNED.ALL;
   USE IEEE.MATH_REAL.ALL;
   USE IEEE.STD_LOGIC_TEXTIO.ALL;
LIBRARY STD;
   USE STD.TEXTIO.ALL;

LIBRARY UNISIM;
   USE UNISIM.VCOMPONENTS.ALL;

ENTITY Sonar_TB IS
   GENERIC(
      
      TARGET_X  : REAL := 5.0; -- in meters
      TARGET_Y  : REAL := 4.0; -- in meters
      TARGET_Z  : REAL := 3.0; -- in meters
      SNSR_DIST : REAL := 0.3  --in meters
   );
END ENTITY;

ARCHITECTURE tb OF Sonar_TB IS

   CONSTANT SPEED_DF_SOUND     : REAL := 1498.0;--meters per second
   CONSTANT BACKGROUND_LVL     : REAL := 0.3;   --in volts
   CONSTANT SIGNAL_PTP         : REAL := 2.0;   --in volts
   CONSTANT SIGNAL_FREQ        : REAL := 28000.0;  --in herz
   CONSTANT SIGNAL_DURATION    : REAL := 0.006; --in seconds
   CONSTANT Omega              : REAL := 2.0*MATH_PI*SIGNAL_FREQ;

   CONSTANT R                  : REAL := SQRT(TARGET_X**2.0 + TARGET_Y**2.0 + TARGET_Z**2.0);
   CONSTANT Rx                 : REAL := SQRT((TARGET_X - SNSR_DIST)**2.0 + TARGET_Y**2.0 + TARGET_Z**2.0);
   CONSTANT Ry                 : REAL := SQRT(TARGET_X**2.0 + (TARGET_Y - SNSR_DIST)**2.0 + TARGET_Z**2.0);
   CONSTANT Rz                 : REAL := SQRT(TARGET_X**2.0 + TARGET_Y**2.0 + (TARGET_Z - SNSR_DIST)**2.0);
   CONSTANT WIRE_DLY_0         : TIME := INTEGER((10.0**9.0)*R /SPEED_DF_SOUND) * 1 ns;
   CONSTANT WIRE_DLY_X         : TIME := INTEGER((10.0**9.0)*Rx/SPEED_DF_SOUND) * 1 ns;
   CONSTANT WIRE_DLY_Y         : TIME := INTEGER((10.0**9.0)*Ry/SPEED_DF_SOUND) * 1 ns;
   CONSTANT WIRE_DLY_Z         : TIME := INTEGER((10.0**9.0)*Rz/SPEED_DF_SOUND) * 1 ns;
   
   CONSTANT DURATION_CNT       : INTEGER := INTEGER(SIGNAL_DURATION/0.00000001); -- acd input approximationisdonein 10 ns steps

--   COMPONENT fir_compiler_0
--      PORT (
--   
--         aclk               : IN STD_LOGIC;
--         s_axis_data_tvalid : IN STD_LOGIC;
--         s_axis_data_tready : OUT STD_LOGIC;
--         s_axis_data_tdata  : IN STD_LOGIC_VECTOR(63 DOWNTO 0);
--      
--         m_axis_data_tvalid : OUT STD_LOGIC;
--         m_axis_data_tdata  : OUT STD_LOGIC_VECTOR(95 DOWNTO 0)
--      );
--   END COMPONENT;
   COMPONENT FIR
      GENERIC(
         SIGNAL_FREQ : REAL := 28000.0;  --in herz
         CLK_PER     : REAL := 20.0     --in ns
      );
      PORT(
         --system global controls
         rst     : IN STD_LOGIC;
         clk     : IN STD_LOGIC;  --12 MHZ main clock
         -- MPS SPI serial interface signals
         DIN_0   : IN STD_LOGIC_VECTOR(11 DOWNTO 0);
         DIN_x   : IN STD_LOGIC_VECTOR(11 DOWNTO 0);
         DIN_y   : IN STD_LOGIC_VECTOR(11 DOWNTO 0);
         DIN_z   : IN STD_LOGIC_VECTOR(11 DOWNTO 0);
         -- Register interface signals
         DOUT_0  : OUT STD_LOGIC_VECTOR(26 DOWNTO 0);
         DOUT_x  : OUT STD_LOGIC_VECTOR(26 DOWNTO 0);
         DOUT_y  : OUT STD_LOGIC_VECTOR(26 DOWNTO 0);
         DOUT_z  : OUT STD_LOGIC_VECTOR(26 DOWNTO 0)
      );
   END COMPONENT;
   
   
   COMPONENT clk_wiz_0
      PORT(
         clk_in1           : IN     STD_LOGIC;
         -- Clock out ports
         clk_out1          : OUT    STD_LOGIC;
         -- Status and control signals
         reset             : IN     STD_LOGIC;
         locked            : OUT    STD_LOGIC
      );
   END COMPONENT;

   SIGNAL sysclk        : STD_LOGIC := '0';
   SIGNAL clk           : STD_LOGIC := '0';
   SIGNAL ADC0          : STD_LOGIC_VECTOR(11 DOWNTO 0) := (OTHERS => '0');
   SIGNAL ADCx          : STD_LOGIC_VECTOR(11 DOWNTO 0) := (OTHERS => '0');
   SIGNAL ADCy          : STD_LOGIC_VECTOR(11 DOWNTO 0) := (OTHERS => '0');
   SIGNAL ADCz          : STD_LOGIC_VECTOR(11 DOWNTO 0) := (OTHERS => '0');
   SIGNAL ADC0_nois     : STD_LOGIC_VECTOR(11 DOWNTO 0) := (OTHERS => '0');
   SIGNAL ADCx_nois     : STD_LOGIC_VECTOR(11 DOWNTO 0) := (OTHERS => '0');
   SIGNAL ADCy_nois     : STD_LOGIC_VECTOR(11 DOWNTO 0) := (OTHERS => '0');
   SIGNAL ADCz_nois     : STD_LOGIC_VECTOR(11 DOWNTO 0) := (OTHERS => '0');

   SIGNAL ADC_out_0     : STD_LOGIC_VECTOR(11 DOWNTO 0) := (OTHERS => '0');
   SIGNAL ADC_out_x     : STD_LOGIC_VECTOR(11 DOWNTO 0) := (OTHERS => '0');
   SIGNAL ADC_out_y     : STD_LOGIC_VECTOR(11 DOWNTO 0) := (OTHERS => '0');
   SIGNAL ADC_out_z     : STD_LOGIC_VECTOR(11 DOWNTO 0) := (OTHERS => '0');
   SIGNAL tready        : STD_LOGIC;
   SIGNAL tdata         : STD_LOGIC_VECTOR(63 DOWNTO 0);
   SIGNAL tvalid        : STD_LOGIC;
   SIGNAL tdata_out     : STD_LOGIC_VECTOR(95 DOWNTO 0);
   SIGNAL inp_z         : STD_LOGIC_VECTOR(26 DOWNTO 0);
   SIGNAL inp_y         : STD_LOGIC_VECTOR(26 DOWNTO 0);
   SIGNAL inp_x         : STD_LOGIC_VECTOR(26 DOWNTO 0);
   SIGNAL inp_0         : STD_LOGIC_VECTOR(26 DOWNTO 0);
   
   
BEGIN

   -- generate the clock with 12 Mhz
   PROCESS_CLK_GEN: PROCESS
   BEGIN
      WAIT FOR 41667 ps;
      sysclk <= not sysclk;
   END PROCESS PROCESS_CLK_GEN;
   
   GEN_ADC0: PROCESS
      VARIABLE cur_time : REAL := 0.0;
   BEGIN
      WAIT FOR 1 us; --Initial delay
      WAIT FOR WIRE_DLY_0; --delay in the wather for position (000)
      WHILE TRUE LOOP
         FOR i IN 0 TO (DURATION_CNT - 1) LOOP
            cur_time     := cur_time + 0.00000001;
            IF (Omega*cur_time = 2.0*MATH_PI) THEN
               cur_time     := 0.0;
            END IF;
            --An_Clk_28kHz <= SIN(Omega*cur_time);
            ADC0 <= CONV_STD_LOGIC_VECTOR(INTEGER(1024.0*SIN(Omega*cur_time)), 12);
            WAIT FOR 10 ns;
         END LOOP;
         WAIT FOR 1000000 us; 
      END LOOP;
   END PROCESS GEN_ADC0;

   GEN_ADCx: PROCESS
      VARIABLE cur_time : REAL := 0.0;
   BEGIN
      WAIT FOR 1 us; --Initial delay
      WAIT FOR WIRE_DLY_X; --delay in the wather for position (000)
      WHILE TRUE LOOP
         FOR i IN 0 TO (DURATION_CNT - 1) LOOP
            cur_time     := cur_time + 0.00000001;
            IF (Omega*cur_time = 2.0*MATH_PI) THEN
               cur_time     := 0.0;
            END IF;
            --An_Clk_28kHz <= SIN(Omega*cur_time);
            ADCx <= CONV_STD_LOGIC_VECTOR(INTEGER(1024.0*SIN(Omega*cur_time)), 12);
            WAIT FOR 10 ns;
         END LOOP;
         WAIT FOR 1000000 us; 
      END LOOP;
   END PROCESS GEN_ADCx;
   
  GEN_ADCy: PROCESS
      VARIABLE cur_time : REAL := 0.0;
   BEGIN
      WAIT FOR 1 us; --Initial delay
      WAIT FOR WIRE_DLY_Y; --delay in the wather for position (000)
      WHILE TRUE LOOP
         FOR i IN 0 TO (DURATION_CNT - 1) LOOP
            cur_time     := cur_time + 0.00000001;
            IF (Omega*cur_time = 2.0*MATH_PI) THEN
               cur_time     := 0.0;
            END IF;
            --An_Clk_28kHz <= SIN(Omega*cur_time);
            ADCY <= CONV_STD_LOGIC_VECTOR(INTEGER(1024.0*SIN(Omega*cur_time)), 12);
            WAIT FOR 10 ns;
         END LOOP;
         WAIT FOR 1000000 us; 
      END LOOP;
   END PROCESS GEN_ADCy;
   
  GEN_ADCz: PROCESS
      VARIABLE cur_time : REAL := 0.0;
   BEGIN
      WAIT FOR 1 us; --Initial delay
      WAIT FOR WIRE_DLY_Z; --delay in the wather for position (000)
      WHILE TRUE LOOP
         FOR i IN 0 TO (DURATION_CNT - 1) LOOP
            cur_time     := cur_time + 0.00000001;
            IF (Omega*cur_time = 2.0*MATH_PI) THEN
               cur_time     := 0.0;
            END IF;
            --An_Clk_28kHz <= SIN(Omega*cur_time);
            ADCz <= CONV_STD_LOGIC_VECTOR(INTEGER(1024.0*SIN(Omega*cur_time)), 12);
            WAIT FOR 10 ns;
         END LOOP;
         WAIT FOR 1000000 us; 
      END LOOP;
   END PROCESS GEN_ADCz;

   PROCESS
      FILE Seed_File : TEXT;
      VARIABLE Open_Status : FILE_OPEN_STATUS;
      VARIABLE dataLine:LINE;
      VARIABLE seed1 : POSITIVE;
      VARIABLE seed2 : POSITIVE;
      VARIABLE rand : REAL;
      VARIABLE vout_sel : INTEGER;
   BEGIN
      FILE_OPEN(Open_Status, Seed_File, "Seeds", READ_MODE);
      IF (Open_Status /= OPEN_OK) THEN
         REPORT "Fresh start";
      ELSE
         IF (NOT ENDFILE(Seed_File)) THEN  
            READLINE(Seed_File, dataLine);
            READ(dataLine, seed1);
            READ(dataLine, seed2);
         END IF;
         FILE_CLOSE(Seed_File);
         UNIFORM(seed1, seed2, rand);
         FILE_OPEN(Open_Status, Seed_File, "Seeds", WRITE_MODE);
         WRITE(dataLine, seed1);
         WRITE(dataLine, ' ');
         WRITE(dataLine, seed2);
         WRITELINE(Seed_File, dataLine);
         FILE_CLOSE(Seed_File);
      END IF;
      LOOP
         UNIFORM(seed1, seed2, rand);
         ADC0_nois <= STD_LOGIC_VECTOR(TO_SIGNED(INTEGER(ROUND((rand - 0.5)*400.0)), 12));
         UNIFORM(seed1, seed2, rand);
         ADCx_nois <= STD_LOGIC_VECTOR(TO_SIGNED(INTEGER(ROUND((rand - 0.5)*400.0)), 12));
         UNIFORM(seed1, seed2, rand);
         ADCy_nois <= STD_LOGIC_VECTOR(TO_SIGNED(INTEGER(ROUND((rand - 0.5)*400.0)), 12));
         UNIFORM(seed1, seed2, rand);
         ADCz_nois <= STD_LOGIC_VECTOR(TO_SIGNED(INTEGER(ROUND((rand - 0.5)*400.0)), 12));
         WAIT FOR 10 ns;
      END LOOP;
   END PROCESS;
   

   ADC_out_0  <=  ADC0_nois + ADC0;
   ADC_out_x  <=  ADCx_nois + ADCX;
   ADC_out_y  <=  ADCy_nois + ADCY;
   ADC_out_z  <=  ADCz_nois + ADCZ;
--   ADC_out_0  <=  ADC0;
--   ADC_out_x  <=  ADCX;
--   ADC_out_y  <=  ADCY;
--   ADC_out_z  <=  ADCZ;

   clk_gen : clk_wiz_0
   PORT MAP(
      clk_in1   => sysclk,
      clk_out1  => clk,
      reset     => '0',
      locked    => OPEN
   );

   FIR_inst : FIR
   GENERIC MAP(
      SIGNAL_FREQ => 28000.0,  --in herz
      CLK_PER     => 20.0     --in ns
   )
   PORT MAP(
      --system global controls
      rst     => '0',
      clk     => clk,
      -- MPS SPI serial interface signals
      DIN_0   => ADC_out_0,
      DIN_x   => ADC_out_x,
      DIN_y   => ADC_out_y,
      DIN_z   => ADC_out_z,
      -- Register interface signals
      DOUT_0  => inp_z,
      DOUT_x  => inp_y,
      DOUT_y  => inp_x,
      DOUT_z  => inp_0
   );

END tb;
