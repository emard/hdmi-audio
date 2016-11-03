-- HDMI-audio test

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.ALL;
use ieee.numeric_std.all;

entity hdmiaudio_reverseu16 is
generic
(
  C_audio_islands: boolean := false;
  C_hdmi_ddr: boolean := true;
  C_hdmi_generic_serializer: boolean := true
);
port
(
  clk_50MHz: in std_logic;

  -- USB
  usb_reset_n, usb_tx: in std_logic;
  usb_si: in std_logic; -- USB_NEWFRAME for atari800
  usb_io1: in std_logic; -- USB_NEWFRAME for nes
  usb_cs_n: out std_logic; -- USB_VNC_MODE_N
  
  -- External RS232 TTL 3.3V
  dp: out std_logic;  -- FPGA TX to external RX
  dn: in std_logic; -- FPGA RX to external TX

  -- HDMI
  --hdmi_sda: inout std_logic;
  --hdmi_scl: out std_logic;
  --hdmi_hec: out std_logic;
  --hdmi_cec: inout std_logic;

  -- for generic differential output
  hdmi_dp, hdmi_dn: out std_logic_vector(2 downto 0);
  hdmi_clkp, hdmi_clkn: out std_logic
);
end;

architecture struct of hdmiaudio_reverseu16 is
  constant C_resolution_x: integer := 640;
  signal clk_pixel, clk_pixel_shift: std_logic;

  signal S_vga_r, S_vga_g, S_vga_b: std_logic_vector(7 downto 0);
  signal S_vga_vsync, S_vga_hsync: std_logic;
  signal S_vga_vblank, S_vga_blank: std_logic;
  signal S_audio: std_logic_vector(11 downto 0);
  signal S_audio_enable: std_logic;

  signal S_hdmi_pd0, S_hdmi_pd1, S_hdmi_pd2: std_logic_vector(9 downto 0);
  signal S_hdmi_d0, S_hdmi_d1, S_hdmi_d2: std_logic;
  signal S_hdmi_d, S_hdmi_dn: std_logic_vector(3 downto 0);
  signal S_hdmi_clk, S_hdmi_clkn: std_logic;
  signal S_hdmi_bits: std_logic_vector(39 downto 0);
  signal S_hdmi_ddr: std_logic_vector(7 downto 0);
  signal tmds_d, tmds_dn: std_logic_vector(7 downto 0);

  signal reset        : std_logic;
  signal clock_stable : std_logic;

  signal JOY1: std_logic_vector(4 downto 0);
  signal JOY2: std_logic_vector(4 downto 0);

  signal joy_report: std_logic_vector(71 downto 0);

  signal R_pixel_blink: std_logic_vector(25 downto 0) := (others => '0');
  signal R_pixel_shift_blink: std_logic_vector(28 downto 0) := (others => '0');
  signal S_vga_fetch_next: std_logic;
  signal S_osd_pixel: std_logic;
  signal S_osd_green: std_logic_vector(7 downto 0); -- OSD byte signal

  signal R_beep: std_logic_vector(14 downto 0);
begin
  G_vendorspec_hdmi:
  if C_hdmi_ddr generate
    clkgen_125_25: entity work.clk_50M_125M_25M
    port map
    (
      inclk0 => clk_50MHz,   --  50 MHz input from board
      c0 => clk_pixel_shift, -- 125 MHz
      c1 => clk_pixel,       --  25 MHz
      locked => clock_stable
    );
  end generate;

  G_generic_serializer:
  if not C_hdmi_ddr generate
    clkgen_250_25: entity work.pll_50M_250M_25M_83M333
    port map
    (
      inclk0 => clk_50MHz,    --  50 MHz input from board
      c0 => clk_pixel_shift,  -- 250 MHz
      c1 => clk_pixel,        --  25 MHz
      c2 => open              --  83.333 MHz
    );
  end generate;

  reset <= not clock_stable;

  -- the user input device
  hid: entity work.vnc2hid
  generic map
  (
    C_clock_freq => 25000000,
    C_baud_rate => 115200
  )
  port map
  (
    CLK => clk_pixel,
    RESET_N => '1',
    USB_TX => USB_TX,
    HID_REPORT => joy_report,
    NEW_VNC2_MODE_N => usb_cs_n,
    NEW_FRAME => usb_io1
  );
  dp <= usb_tx; -- debug serial traffic to external rs232


  -- VGA video generator - pixel clock synchronous
  vgabitmap: entity work.vga
  generic map -- workaround for wrong video size
  (
    C_resolution_x => C_resolution_x
  )
  port map
  (
      clk_pixel => clk_pixel,
      test_picture => '1', -- shows test picture when VGA is disabled (on startup)
      fetch_next => S_vga_fetch_next,
      line_repeat => open,
      red_byte    => (others => '0'), -- framebuffer inputs not used
      green_byte  => (others => '0'), -- rgb signal is synchronously generated
      blue_byte   => (others => '0'), -- and replaced
      beam_x => open,
      beam_y => open,
      vga_r => S_vga_r,
      vga_g => S_vga_g,
      vga_b => S_vga_b,
      vga_hsync => S_vga_hsync,
      vga_vsync => S_vga_vsync,
      vga_blank => S_vga_blank, -- '1' when outside of horizontal or vertical graphics area
      vga_vblank => S_vga_vblank -- '1' when outside of vertical graphics area (used for vblank interrupt)
  );

  -- OSD overlay for the green channel
  I_osd: entity work.osd
  generic map -- workaround for wrong video size
  (
    C_resolution_x => C_resolution_x
  )
  port map
  (
    clk_pixel => clk_pixel,
    vsync => S_vga_vsync,
    fetch_next => S_vga_fetch_next,
    --probe_in(63 downto 48) => R_pixel_shift_blink(R_pixel_shift_blink'high downto R_pixel_shift_blink'high-15),
    --probe_in(29 downto 25) => Joy2,
    --probe_in(24 downto 20) => Joy1,
    --probe_in(15 downto 0) => R_pixel_blink(R_pixel_blink'high downto R_pixel_blink'high-15),
    probe_in(63 downto 0) => joy_report(71 downto 8),
    osd_out => S_osd_pixel
  );
  S_osd_green <= (others => S_osd_pixel);

  process(clk_pixel)
  begin
    if rising_edge(clk_pixel) then
      R_pixel_blink <= R_pixel_blink+1;
    end if;
  end process;

  process(clk_pixel_shift)
  begin
    if rising_edge(clk_pixel_shift) then
      R_pixel_shift_blink <= R_pixel_shift_blink+1;
    end if;
  end process;

  -- beep generator (sawtooth)
  process(clk_pixel)
  begin
    if rising_edge(clk_pixel) then
      R_beep <= R_beep+1;
    end if;
  end process;
  -- press left button to mute the beep sound
  S_audio <= R_beep(R_beep'high downto R_beep'high-11) when true
             else (others => '0');

  S_audio_enable <= '1' when C_audio_islands else '0';
  -- HDMI
  hdmi_out: entity work.av_hdmi
  generic map
  (
    FREQ => 25000000,
    FS => 48000,
    CTS => 25000,
    N => 6144
  )
  port map
  (
    I_CLK_PIXEL    => clk_pixel,
    I_R            => S_vga_r,
    I_G	           => S_vga_g or S_osd_green,
    I_B            => S_vga_b,
    I_BLANK        => S_vga_blank,
    I_HSYNC        => not S_vga_hsync,
    I_VSYNC        => not S_vga_vsync,
    I_AUDIO_ENABLE => S_audio_enable, -- '1' to enable audio islands
    I_AUDIO_PCM_L  => S_audio & "0000",
    I_AUDIO_PCM_R  => S_audio & "0000",
    O_TMDS_PD0      => S_HDMI_PD0,
    O_TMDS_PD1      => S_HDMI_PD1,
    O_TMDS_PD2      => S_HDMI_PD2
  );

  -- S_hdmi_bits <= S_HDMI_PD2 & S_HDMI_PD1 & S_HDMI_PD0; -- this would be normal bit order, but
  -- generic serializer follows vendor specific serializer style
  S_hdmi_bits <=  "0000011111" &
                  S_HDMI_PD2(0) & S_HDMI_PD2(1) & S_HDMI_PD2(2) & S_HDMI_PD2(3) & S_HDMI_PD2(4) & S_HDMI_PD2(5) & S_HDMI_PD2(6) & S_HDMI_PD2(7) & S_HDMI_PD2(8) & S_HDMI_PD2(9) &
                  S_HDMI_PD1(0) & S_HDMI_PD1(1) & S_HDMI_PD1(2) & S_HDMI_PD1(3) & S_HDMI_PD1(4) & S_HDMI_PD1(5) & S_HDMI_PD1(6) & S_HDMI_PD1(7) & S_HDMI_PD1(8) & S_HDMI_PD1(9) &
                  S_HDMI_PD0(0) & S_HDMI_PD0(1) & S_HDMI_PD0(2) & S_HDMI_PD0(3) & S_HDMI_PD0(4) & S_HDMI_PD0(5) & S_HDMI_PD0(6) & S_HDMI_PD0(7) & S_HDMI_PD0(8) & S_HDMI_PD0(9);

  G_hdmi_sdr: if not C_hdmi_ddr generate
    generic_serializer_inst: entity work.serializer_generic
    PORT MAP
    (
        tx_in => S_hdmi_bits,
        tx_inclock => CLK_PIXEL_SHIFT, -- NOTE: generic serializer needs CLK_PIXEL x10
        tx_syncclock => CLK_PIXEL,
        tx_out => tmds_d(3 downto 0)
    );
    hdmi_clkp <=     tmds_d(3);
    hdmi_clkn <= not tmds_d(3);
    hdmi_dp(2 downto 0) <=     tmds_d(2 downto 0);
    hdmi_dn(2 downto 0) <= not tmds_d(2 downto 0);
  end generate;

  G_hdmi_ddr: if C_hdmi_ddr generate
    generic_serializer_inst_p: entity work.serializer_generic
    GENERIC MAP
    (
      C_output_bits => 2
    )
    PORT MAP
    (
      tx_in => S_hdmi_bits,
      tx_inclock => CLK_PIXEL_SHIFT, -- NOTE: generic serializer needs CLK_PIXEL x10
      tx_syncclock => CLK_PIXEL,
      tx_out => tmds_d(7 downto 0)
    );
    generic_serializer_inst_n: entity work.serializer_generic
    GENERIC MAP
    (
      C_output_bits => 2
    )
    PORT MAP
    (
      tx_in => not S_hdmi_bits,
      tx_inclock => CLK_PIXEL_SHIFT, -- NOTE: generic serializer needs CLK_PIXEL x10
      tx_syncclock => CLK_PIXEL,
      tx_out => tmds_dn(7 downto 0)
    );
    ddio_inst: entity work.altddio_out1
    PORT MAP
    (
      outclock => CLK_PIXEL_SHIFT,
      datain_l => tmds_d(7) & tmds_dn(7) & tmds_d(5) & tmds_dn(5) & tmds_d(3) & tmds_dn(3) & tmds_d(1) & tmds_dn(1),
      datain_h => tmds_d(6) & tmds_dn(6) & tmds_d(4) & tmds_dn(4) & tmds_d(2) & tmds_dn(2) & tmds_d(0) & tmds_dn(0),
      dataout  => S_hdmi_ddr
    );
    hdmi_clkp <= S_hdmi_ddr(7);
    hdmi_clkn <= S_hdmi_ddr(6);
    hdmi_dp(2) <= S_hdmi_ddr(5);
    hdmi_dn(2) <= S_hdmi_ddr(4);
    hdmi_dp(1) <= S_hdmi_ddr(3);
    hdmi_dn(1) <= S_hdmi_ddr(2);
    hdmi_dp(0) <= S_hdmi_ddr(1);
    hdmi_dn(0) <= S_hdmi_ddr(0);
  end generate;

end struct;
