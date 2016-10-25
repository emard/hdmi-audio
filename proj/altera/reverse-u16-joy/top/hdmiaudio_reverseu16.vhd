-- HDMI-audio test

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.ALL;
use ieee.numeric_std.all;

entity hdmiaudio_reverseu16 is
generic
(
  C_audio_islands: boolean := false;
  C_generic_hdmi: boolean := false
);
port
(
  clk_50MHz: in std_logic;

  -- USB
  usb_reset_n, usb_tx: in std_logic;
  usb_si: in std_logic; -- USB_NEWFRAME
  usb_cs_n: out std_logic; -- USB_VNC_MODE_N

  -- HDMI
  --hdmi_sda: inout std_logic;
  --hdmi_scl: out std_logic;
  --hdmi_hec: out std_logic;
  --hdmi_cec: inout std_logic;

  -- for vendor-specific serializer
  hdmi_d0, hdmi_d1, hdmi_d2: out std_logic;
  hdmi_clk: out std_logic
  -- for generic serializer
  --hdmi_dp, hdmi_dn: out std_logic_vector(2 downto 0);
  --hdmi_clkp, hdmi_clkn: out std_logic
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

  signal S_hdmi_d0, S_hdmi_d1, S_hdmi_d2: std_logic;
  signal S_hdmi_clk: std_logic;

  signal reset        : std_logic;
  signal clock_stable : std_logic;

  signal JOY1: std_logic_vector(4 downto 0);
  signal JOY2: std_logic_vector(4 downto 0);

  signal R_pixel_blink: std_logic_vector(25 downto 0) := (others => '0');
  signal R_pixel_shift_blink: std_logic_vector(28 downto 0) := (others => '0');
  signal S_vga_fetch_next: std_logic;
  signal S_osd_pixel: std_logic;
  signal S_osd_green: std_logic_vector(7 downto 0); -- OSD byte signal

  signal R_beep: std_logic_vector(14 downto 0);
begin
  G_vendorspec_hdmi:
  if not C_generic_hdmi generate
    clkgen_125_25: entity work.clk_50M_125M_25M
    port map
    (
      inclk0 => clk_50MHz,   --  50 MHz input from board
      c0 => clk_pixel_shift, -- 125 MHz
      c1 => clk_pixel,       --  25 MHz
      locked => clock_stable
    );
    hdmi_clk <= S_hdmi_clk;
    hdmi_d0  <= S_hdmi_d0;
    hdmi_d1  <= S_hdmi_d1;
    hdmi_d2  <= S_hdmi_d2;
  end generate;

  G_generic_hdmi:
  if C_generic_hdmi generate
    clkgen_250_25: entity work.pll_50M_250M_25M_83M333
    port map
    (
      inclk0 => clk_50MHz,    --  50 MHz input from board
      c0 => clk_pixel_shift,  -- 250 MHz
      c1 => clk_pixel,        --  25 MHz
      c2 => open              --  83.333 MHz
    );
    --hdmi_clkp <=      S_hdmi_clk;
    --hdmi_clkn <= not  S_hdmi_clk;
    --hdmi_dp   <=     (S_hdmi_d2 & S_hdmi_d1 & S_hdmi_d0);
    --hdmi_dn   <= not (S_hdmi_d2 & S_hdmi_d1 & S_hdmi_d0);
  end generate;

  reset <= not clock_stable;

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
    probe_in(63 downto 48) => R_pixel_shift_blink(R_pixel_shift_blink'high downto R_pixel_shift_blink'high-15),
    probe_in(29 downto 25) => Joy2,
    probe_in(24 downto 20) => Joy1,
    probe_in(15 downto 0) => R_pixel_blink(R_pixel_blink'high downto R_pixel_blink'high-15),
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
    C_generic_serializer => C_generic_hdmi,
    FREQ => 25000000,
    FS => 48000,
    CTS => 25000,
    N => 6144
  )
  port map
  (
    I_CLK_PIXEL_x5 => clk_pixel_shift,
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
    O_TMDS_D0      => S_HDMI_D0,
    O_TMDS_D1      => S_HDMI_D1,
    O_TMDS_D2      => S_HDMI_D2,
    O_TMDS_CLK     => S_HDMI_CLK
  );
  
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
    KEYBOARD_SCAN => (others => '0'),
    KEYBOARD_RESPONSE => open,
    CONSOL_START => open,
    CONSOL_SELECT => open,
    CONSOL_OPTION => open,
    RESET_BUTTON => open,
    FKEYS => open,
    JOY1_n => JOY1,
    JOY2_n => JOY2,
    CTL_KEYS => open,
    CTL_KEYS_PREV => OPEN,
    NEW_VNC2_MODE_N => usb_cs_n,
    NEW_FRAME => usb_si
  );

end struct;
