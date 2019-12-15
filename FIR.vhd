LIBRARY IEEE;
   USE IEEE.STD_LOGIC_1164.ALL;
--   USE IEEE.STD_LOGIC_ARITH.ALL;
   USE IEEE.STD_LOGIC_UNSIGNED.ALL;
   USE IEEE.NUMERIC_STD.ALL;
   USE IEEE.MATH_REAL.ALL;

ENTITY FIR IS
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
END ENTITY;

ARCHITECTURE rtl OF FIR IS

------------- Begin Cut here for COMPONENT Declaration ------ COMP_TAG
   CONSTANT TOTAL_PER_CNT : INTEGER := INTEGER(ROUND((10.0**9.0/SIGNAL_FREQ)/CLK_PER));
   CONSTANT STEP_CNT      : INTEGER := TOTAL_PER_CNT/8;
   CONSTANT MEM_ADDR_BITS : INTEGER := INTEGER(CEIL(LOG2((REAL(TOTAL_PER_CNT)))));
   
   SIGNAL mem0_addr : STD_LOGIC_VECTOR(10 DOWNTO 0):= (OTHERS => '0');
   SIGNAL mem1_addr : STD_LOGIC_VECTOR(10 DOWNTO 0):= (OTHERS => '0');
   SIGNAL mem2_addr : STD_LOGIC_VECTOR(10 DOWNTO 0):= (OTHERS => '0');
   SIGNAL mem3_addr : STD_LOGIC_VECTOR(10 DOWNTO 0):= (OTHERS => '0');
   SIGNAL mem4_addr : STD_LOGIC_VECTOR(10 DOWNTO 0):= (OTHERS => '0');
   SIGNAL mem5_addr : STD_LOGIC_VECTOR(10 DOWNTO 0):= (OTHERS => '0');
   SIGNAL mem6_addr : STD_LOGIC_VECTOR(10 DOWNTO 0):= (OTHERS => '0');

   SIGNAL mem0_raddr : STD_LOGIC_VECTOR(10 DOWNTO 0):= (OTHERS => '0');
   SIGNAL mem1_raddr : STD_LOGIC_VECTOR(10 DOWNTO 0):= (OTHERS => '0');
   SIGNAL mem2_raddr : STD_LOGIC_VECTOR(10 DOWNTO 0):= (OTHERS => '0');
   SIGNAL mem3_raddr : STD_LOGIC_VECTOR(10 DOWNTO 0):= (OTHERS => '0');
   SIGNAL mem4_raddr : STD_LOGIC_VECTOR(10 DOWNTO 0):= (OTHERS => '0');
   SIGNAL mem5_raddr : STD_LOGIC_VECTOR(10 DOWNTO 0):= (OTHERS => '0');
   SIGNAL mem6_raddr : STD_LOGIC_VECTOR(10 DOWNTO 0):= (OTHERS => '0');

   SIGNAL mem0_din  : STD_LOGIC_VECTOR(47 DOWNTO 0):= (OTHERS => '0');
   SIGNAL mem1_din  : STD_LOGIC_VECTOR(47 DOWNTO 0):= (OTHERS => '0');
   SIGNAL mem2_din  : STD_LOGIC_VECTOR(47 DOWNTO 0):= (OTHERS => '0');
   SIGNAL mem3_din  : STD_LOGIC_VECTOR(47 DOWNTO 0):= (OTHERS => '0');
   SIGNAL mem4_din  : STD_LOGIC_VECTOR(47 DOWNTO 0):= (OTHERS => '0');
   SIGNAL mem5_din  : STD_LOGIC_VECTOR(47 DOWNTO 0):= (OTHERS => '0');
   SIGNAL mem6_din  : STD_LOGIC_VECTOR(47 DOWNTO 0):= (OTHERS => '0');
   SIGNAL mem7_din  : STD_LOGIC_VECTOR(47 DOWNTO 0):= (OTHERS => '0');

   SIGNAL mem0_dout  : STD_LOGIC_VECTOR(47 DOWNTO 0):= (OTHERS => '0');
   SIGNAL mem1_dout  : STD_LOGIC_VECTOR(47 DOWNTO 0):= (OTHERS => '0');
   SIGNAL mem2_dout  : STD_LOGIC_VECTOR(47 DOWNTO 0):= (OTHERS => '0');
   SIGNAL mem3_dout  : STD_LOGIC_VECTOR(47 DOWNTO 0):= (OTHERS => '0');
   SIGNAL mem4_dout  : STD_LOGIC_VECTOR(47 DOWNTO 0):= (OTHERS => '0');
   SIGNAL mem5_dout  : STD_LOGIC_VECTOR(47 DOWNTO 0):= (OTHERS => '0');
   SIGNAL mem6_dout  : STD_LOGIC_VECTOR(47 DOWNTO 0):= (OTHERS => '0');

   SIGNAL mem_en     : STD_LOGIC_VECTOR(7 DOWNTO 0):= X"00";
   SIGNAL mem_en_reg : STD_LOGIC_VECTOR(7 DOWNTO 0):= X"00";
   
   COMPONENT blk_mem_gen_0
      PORT (
         clka  : IN STD_LOGIC;
         wea   : IN STD_LOGIC_VECTOR(0 DOWNTO 0);
         addra : IN STD_LOGIC_VECTOR(10 DOWNTO 0);
         dina  : IN STD_LOGIC_VECTOR(47 DOWNTO 0);
         clkb  : IN STD_LOGIC;
         addrb : IN STD_LOGIC_VECTOR(10 DOWNTO 0);
         doutb : OUT STD_LOGIC_VECTOR(47 DOWNTO 0)
      );
   END COMPONENT;

   COMPONENT mult_1448
   PORT (
      CLK : IN STD_LOGIC;
      A   : IN STD_LOGIC_VECTOR(11 DOWNTO 0);
      P   : OUT STD_LOGIC_VECTOR(22 DOWNTO 0)
   );
   END COMPONENT;
   
   COMPONENT mult_n1448
   PORT (
      CLK : IN STD_LOGIC;
      A : IN STD_LOGIC_VECTOR(11 DOWNTO 0);
      P : OUT STD_LOGIC_VECTOR(23 DOWNTO 0)
   );
   END COMPONENT;
   
   SIGNAL SUM2_reg : STD_LOGIC_VECTOR(107 DOWNTO 0);
   SIGNAL run_cntr : STD_LOGIC_VECTOR(7 DOWNTO 0) := (OTHERS => '0');

   
BEGIN

   DOUT_0  <= SUM2_reg(26 DOWNTO 0);
   DOUT_x  <= SUM2_reg(53 DOWNTO 27);
   DOUT_y  <= SUM2_reg(80 DOWNTO 54);
   DOUT_z  <= SUM2_reg(107 DOWNTO 81);

   mem0_din <= DIN_0 &--phase -180
               DIN_x &
               DIN_y &
               DIN_z;

   blk_mem0 : blk_mem_gen_0
   PORT MAP(
      clka   => clk,
      wea(0) => '1',
      addra  => mem0_addr,
      dina   => mem0_din,
      clkb   => clk,
      addrb  => mem0_raddr,
      doutb  => mem0_dout
   );
   
   mem1_din <= mem0_dout;--phase -135

   blk_mem1 : blk_mem_gen_0
   PORT MAP(
      clka  => clk,
      wea(0)    => '1',
      addra => mem1_addr,
      dina  => mem1_din,
      clkb  => clk,
      addrb => mem1_raddr,
      doutb => mem1_dout
   );

   mem2_din <= mem1_dout;--phase -90
          
   blk_mem2 : blk_mem_gen_0
   PORT MAP(
      clka  => clk,
      wea(0)    => '1',
      addra => mem2_addr,
      dina  => mem2_din,
      clkb  => clk,
      addrb => mem2_raddr,
      doutb => mem2_dout
   );

   mem3_din <= mem2_dout;--phase -45
   
   blk_mem3 : blk_mem_gen_0
   PORT MAP(
      clka  => clk,
      wea(0)    => '1',
      addra => mem3_addr,
      dina  => mem3_din,
      clkb  => clk,
      addrb => mem3_raddr,
      doutb => mem3_dout
   );

   mem4_din <= mem3_dout;--phase -0
   
   blk_mem4 : blk_mem_gen_0
   PORT MAP(
      clka  => clk,
      wea(0)    => '1',
      addra => mem4_addr,
      dina  => mem4_din,
      clkb  => clk,
      addrb => mem4_raddr,
      doutb => mem4_dout
   );

   mem5_din <= mem4_dout;--phase 45
   
   blk_mem5 : blk_mem_gen_0
   PORT MAP(
      clka  => clk,
      wea(0)    => '1',
      addra => mem5_addr,
      dina  => mem5_din,
      clkb  => clk,
      addrb => mem5_raddr,
      doutb => mem5_dout
   );

   mem6_din <= mem5_dout;--phase 90
   
   blk_mem6 : blk_mem_gen_0
   PORT MAP(
      clka  => clk,
      wea(0)    => '1',
      addra => mem6_addr,
      dina  => mem6_din,
      clkb  => clk,
      addrb => mem6_raddr,
      doutb => mem6_dout
   );

   mem7_din <= mem6_dout;--phase 135
   
   
   PROCESS(clk)
   BEGIN
      IF RISING_EDGE(clk) THEN
         IF rst = '1' THEN
            mem0_addr  <= (OTHERS => '0');
            mem1_addr  <= (OTHERS => '0');
            mem2_addr  <= (OTHERS => '0');
            mem3_addr  <= (OTHERS => '0');
            mem4_addr  <= (OTHERS => '0');
            mem5_addr  <= (OTHERS => '0');
            mem6_addr  <= (OTHERS => '0');

            mem0_raddr <= (OTHERS => '0');
            mem1_raddr <= (OTHERS => '0');
            mem2_raddr <= (OTHERS => '0');
            mem3_raddr <= (OTHERS => '0');
            mem4_raddr <= (OTHERS => '0');
            mem5_raddr <= (OTHERS => '0');
            mem6_raddr <= (OTHERS => '0');
            run_cntr   <= (OTHERS => '0');
         ELSE
            IF (run_cntr = STEP_CNT - 2) THEN
               mem_en_reg <= mem_en;
               run_cntr <= run_cntr + '1';
            ELSIF (run_cntr = STEP_CNT - 1) THEN
               mem_en <= mem_en(6 DOWNTO 0) & '1';
               run_cntr <= (OTHERS => '0');
            ELSE
               run_cntr <= run_cntr + '1';
            END IF;
            IF (mem_en_reg(0) = '1') THEN
               mem0_raddr <= mem0_raddr + '1';
            END IF;
            IF (mem_en_reg(1) = '1') THEN
               mem1_raddr <= mem1_raddr + '1';
            END IF;
            IF (mem_en_reg(2) = '1') THEN
               mem2_raddr <= mem2_raddr + '1';
            END IF;
            IF (mem_en_reg(3) = '1') THEN
               mem3_raddr <= mem3_raddr + '1';
            END IF;
            IF (mem_en_reg(4) = '1') THEN
               mem4_raddr <= mem4_raddr + '1';
            END IF;
            IF (mem_en_reg(5) = '1') THEN
               mem5_raddr <= mem5_raddr + '1';
            END IF;
            IF (mem_en_reg(6) = '1') THEN
               mem6_raddr <= mem6_raddr + '1';
            END IF;
            IF (mem_en(0) = '1') THEN
               mem0_addr <= mem0_addr + '1';
            END IF;
            IF (mem_en(1) = '1') THEN
               mem1_addr <= mem1_addr + '1';
            END IF;
            IF (mem_en(2) = '1') THEN
               mem2_addr <= mem2_addr + '1';
            END IF;
            IF (mem_en(3) = '1') THEN
               mem3_addr <= mem3_addr + '1';
            END IF;
            IF (mem_en(4) = '1') THEN
               mem4_addr <= mem4_addr + '1';
            END IF;
            IF (mem_en(5) = '1') THEN
               mem5_addr <= mem5_addr + '1';
            END IF;
            IF (mem_en(6) = '1') THEN
               mem6_addr <= mem6_addr + '1';
            END IF;
         END IF;
      END IF;
   END PROCESS;

   FOR_EACH_INPUT : FOR i IN 0 TO 3 GENERATE
   
      SIGNAL P_n135    : STD_LOGIC_VECTOR(23 DOWNTO 0);
      SIGNAL P_n90     : STD_LOGIC_VECTOR(22 DOWNTO 0);
      SIGNAL P_n45     : STD_LOGIC_VECTOR(23 DOWNTO 0);
      SIGNAL P_45      : STD_LOGIC_VECTOR(22 DOWNTO 0);
      SIGNAL P_90      : STD_LOGIC_VECTOR(22 DOWNTO 0);
      SIGNAL P_135     : STD_LOGIC_VECTOR(22 DOWNTO 0);
      SIGNAL SUM00_reg : STD_LOGIC_VECTOR(24 DOWNTO 0);
      SIGNAL SUM01_reg : STD_LOGIC_VECTOR(24 DOWNTO 0);
      SIGNAL SUM02_reg : STD_LOGIC_VECTOR(23 DOWNTO 0);
      SIGNAL SUM10_reg : STD_LOGIC_VECTOR(25 DOWNTO 0);
      SIGNAL SUM11_reg : STD_LOGIC_VECTOR(23 DOWNTO 0);
   
   BEGIN
      --phase -180
      --phase -135
      phase_n135 : mult_n1448
      PORT MAP(
         CLK => clk,
         A   => mem1_din(12*i+11 DOWNTO 12*i),
         P   => P_n135
      );
      --phase -90
      PROCESS(clk)
      BEGIN
         IF RISING_EDGE(clk) THEN
            P_n90 <= (NOT (mem2_din(12*i+11 DOWNTO 12*i) & B"000_0000_0000")) + '1';--equal to *(-2048)
         END IF;
      END PROCESS;
      --phase -45
      phase_n45 : mult_n1448
      PORT MAP(
         CLK => clk,
         A   => mem3_din(12*i+11 DOWNTO 12*i),
         P   => P_n45
      );
      --phase 0
      --phase 45
      phase_45 : mult_1448
      PORT MAP(
         CLK => clk,
         A   => mem5_din(12*i+11 DOWNTO 12*i),
         P   => P_45
      );
      --phase 90
      PROCESS(clk)
      BEGIN
         IF RISING_EDGE(clk) THEN
            P_90 <= mem6_din(12*i+11 DOWNTO 12*i) & B"000_0000_0000";--equal to *(-2048)
         END IF;
      END PROCESS;
      --phase 135
      phase_1448 : mult_1448
      PORT MAP(
         CLK => clk,
         A   => mem7_din(12*i+11 DOWNTO 12*i),
         P   => P_135
      );
      
      PROCESS(clk)
      BEGIN
         IF RISING_EDGE(clk) THEN
            SUM00_reg                      <= (P_n135(23)& P_n135) +  (P_n90(22) & P_n90(22) & P_n90);
            SUM01_reg                      <= (P_n45(23)& P_n45) + (P_45(22) & P_45(22) & P_45);
            SUM02_reg                      <= (P_90(22) & P_90) + (P_135(22)& P_135);
            SUM10_reg                      <= (SUM00_reg(24)& SUM00_reg) + (SUM01_reg(24)& SUM01_reg);
            SUM11_reg                      <= SUM02_reg;
            SUM2_reg(27*i+26 DOWNTO 27*i)  <= (SUM11_reg(23)& SUM11_reg(23)& SUM11_reg(23)& SUM11_reg) + (SUM10_reg(25)& SUM10_reg);
         END IF;
      END PROCESS;
      
      
   END GENERATE FOR_EACH_INPUT;
   

   
END ARCHITECTURE rtl;
