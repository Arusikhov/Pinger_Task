LIBRARY IEEE;
   USE IEEE.STD_LOGIC_1164.ALL;
--   USE IEEE.STD_LOGIC_ARITH.ALL;
   USE IEEE.STD_LOGIC_UNSIGNED.ALL;
   USE IEEE.NUMERIC_STD.ALL;
   USE IEEE.MATH_REAL.ALL;

ENTITY Sonar IS
   PORT(
      --system global controls
      rst     : IN STD_LOGIC;
      clk     : IN STD_LOGIC;  --12 MHZ main clock
      -- MPS SPI serial interface signals
      SDI     : IN  STD_LOGIC;
      SCK     : IN  STD_LOGIC;
      SCSn    : IN  STD_LOGIC;
      SDO     : OUT STD_LOGIC;
      -- Register interface signals
      DAC_0   : IN STD_LOGIC_VECTOR(11 DOWNTO 0);
      DAC_x   : IN STD_LOGIC_VECTOR(11 DOWNTO 0);
      DAC_y   : IN STD_LOGIC_VECTOR(11 DOWNTO 0);
      DAC_z   : IN STD_LOGIC_VECTOR(11 DOWNTO 0);
      DAC_clk : OUT STD_LOGIC;
      DAC_pd  : OUT STD_LOGIC
   );
END ENTITY;

ARCHITECTURE rtl OF Sonar IS

------------- Begin Cut here for COMPONENT Declaration ------ COMP_TAG
COMPONENT fir_compiler_0
   PORT (
  
      aclk               : IN STD_LOGIC;
      s_axis_data_tvalid : IN STD_LOGIC;
      s_axis_data_tready : OUT STD_LOGIC;
      s_axis_data_tdata  : IN STD_LOGIC_VECTOR(63 DOWNTO 0);
    
      m_axis_data_tvalid : OUT STD_LOGIC;
      m_axis_data_tdata  : OUT STD_LOGIC_VECTOR(95 DOWNTO 0)
   );
END COMPONENT;
   TYPE T_state IS (IDLE, DOUT_SETUP, DOUT_HOLD, nCS_LD_HOLD);
	SIGNAL state         : T_state := IDLE;
   SIGNAL bit_cnt       : STD_LOGIC_VECTOR(5 DOWNTO 0):= (OTHERS => '0');
   SIGNAL shift_reg     : STD_LOGIC_VECTOR(31 DOWNTO 4):= (OTHERS => '0');
   TYPE T_vouts IS ARRAY (0 TO 15) OF STD_LOGIC_VECTOR(11 DOWNTO 0);
   SIGNAL vouts : T_vouts := (OTHERS => (OTHERS => '0'));
   
   SIGNAL clk_en  : STD_LOGIC := '0';
   SIGNAL clk_cnt : STD_LOGIC_VECTOR(2 DOWNTO 0):= (OTHERS => '0');
   SIGNAL wr_en   : STD_LOGIC := '0';
   SIGNAL bsy     : STD_LOGIC := '0';
   SIGNAL Vout_we_reg : STD_LOGIC := '0';
   
BEGIN

   vouts <= (Vout0_val, Vout1_val, Vout2_val, Vout3_val, Vout4_val, Vout5_val, Vout6_val, Vout7_val, 
            Vout8_val, Vout9_val, Vout10_val, Vout11_val, Vout12_val, Vout13_val, Vout14_val, Vout15_val);

   SDI <= shift_reg(31) WHEN (Vout_update(3) = '0') AND (bit_cnt(5) = '1') ELSE
          shift_reg(31) WHEN (Vout_update(3) = '1') AND (bit_cnt(5) = '0') ELSE
          '1';
		  
   shift_reg_o <= shift_reg;
   wr_en_o <= wr_en;
   clk_en_o <= clk_en;
   
   PROCESS(clk)
   BEGIN
      IF RISING_EDGE(clk) THEN
	     Vout_we_reg <= Vout_we;
         IF rst = '1' THEN
            clk_en  <= '0';
            clk_cnt <= "000";
            wr_en   <= '0';
         ELSE
            clk_cnt <= clk_cnt + '1';
            IF clk_cnt = "111" THEN
               clk_en <= '1';
            ELSE
               clk_en <= '0';
            END IF;
            IF Vout_we_reg = '1' THEN
               wr_en <= '1';
            ELSIF clk_en = '1' THEN
               wr_en <= '0';
            END IF;
            IF Vout_we_reg = '1' THEN
               dac_if_bsy <= '1';
            ELSIF wr_en = '0' AND bsy = '0' THEN
               dac_if_bsy <= '0';
            END IF;
         END IF;
      END IF;
   END PROCESS;
          
   
   PROCESS(clk)
   BEGIN
      IF RISING_EDGE(clk) THEN
         IF rst = '1' THEN
            SCK           <= '0';
            nCS_LD        <= '1';
            shift_reg     <= (OTHERS => '0');
            bit_cnt       <= (OTHERS => '0');
            bsy    <= '0';
            state         <= IDLE;
         ELSIF clk_en = '1' THEN
            CASE state IS
               WHEN IDLE =>
                  IF (wr_en = '1') THEN
                     bsy <= '1';
                     shift_reg  <= X"00" & "0011" & '0' & Vout_update(2 DOWNTO 0) & 
                                   vouts(TO_INTEGER(UNSIGNED(Vout_update)));-- & X"0";
                     nCS_LD     <= '0';
                     state      <= DOUT_SETUP;
                  ELSE
                     bsy <= '0';
                  END IF;
               WHEN DOUT_SETUP =>
                  SCK <= '1';
                  state <= DOUT_HOLD;
               WHEN DOUT_HOLD =>
                  SCK <= '0';
                  IF ((Vout_update(3) = '0') AND (bit_cnt(5) = '1')) OR 
                     ((Vout_update(3) = '1') AND (bit_cnt(5) = '0')) THEN
                     shift_reg <= shift_reg(30 DOWNTO 4) & '0';
                  END IF;
                  bit_cnt <= bit_cnt + '1';
                  IF bit_cnt = b"11_1111" THEN
                     nCS_LD <= '1';
                     state  <= nCS_LD_HOLD;
                  ELSE
                     state  <= DOUT_SETUP;
                  END IF;
               WHEN nCS_LD_HOLD =>
                  state  <= IDLE;
            END CASE;
         END IF;
      END IF;
   END PROCESS;

END ARCHITECTURE rtl;
