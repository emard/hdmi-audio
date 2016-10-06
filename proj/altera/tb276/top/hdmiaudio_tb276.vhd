-- HDMI-audio test

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.ALL;
use ieee.numeric_std.all;

entity hdmiaudio_tb276 is
port
(
  clk_25m: in std_logic;
  led: out std_logic_vector(7 downto 0);
  --gpio: inout std_logic_vector(47 downto 0);
  --hdmi_sda: inout std_logic;
  --hdmi_scl: out std_logic;
  --hdmi_hec: out std_logic;
  hdmi_d: out std_logic_vector(2 downto 0);
  hdmi_clk: out std_logic;
  --hdmi_cec: inout std_logic;
  btn_left, btn_right: in std_logic
);
end;

architecture struct of hdmiaudio_tb276 is
  signal clk_pixel, clk_pixel_shift: std_logic;
 
  signal S_vga_r, S_vga_g, S_vga_b: std_logic_vector(7 downto 0);
  signal S_vga_vsync, S_vga_hsync: std_logic;
  signal S_vga_vblank, S_vga_blank: std_logic;
  signal S_audio: std_logic_vector(11 downto 0);
  
  signal reset        : std_logic;
  signal clock_stable : std_logic;
  signal dip_switch   : std_logic_vector(7 downto 0) := (others => '0');
  -- alias  audio_select : std_logic_vector(2 downto 0) is sw(10 downto 8);
begin
  clkgen: entity work.clk_25M_125M_25M
  port map(
      inclk0 => clk_25m, c0 => clk_pixel_shift, c1 => clk_pixel,
      locked => clock_stable
  );

  reset <= not clock_stable;

  -- VGA video generator - pixel clock synchronous
  vgabitmap: entity work.vga
  port map
  (
      clk_pixel => clk_pixel,
      test_picture => '1', -- shows test picture when VGA is disabled (on startup)
      fetch_next => open,
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

  -- some debugging with LEDs
  led(0) <= btn_left;
  led(1) <= btn_right;
  --led(2) <= not gpio(3);
  --led(3) <= not gpio(4);
  --led(4) <= (not gpio(5)) or (not gpio(6));
  led(5) <= S_vga_r(1); -- when game works, changing color on
  led(6) <= S_vga_g(1); -- large area of the screen should
  led(7) <= S_vga_b(1); -- also be "visible" on RGB indicator LEDs
  
  -- buzz generator from vblank
  S_audio(11) <= S_vga_vblank;

  -- HDMI
  hdmi_out: entity work.av_hdmi
  port map
  (
    I_CLK_PIXEL_x5 => clk_pixel_shift,
    I_CLK_PIXEL    => clk_pixel,
    I_R            => S_vga_r,
    I_G	           => S_vga_g,
    I_B            => S_vga_b,
    I_BLANK        => S_vga_blank,
    I_HSYNC        => not S_vga_hsync,
    I_VSYNC        => not S_vga_vsync,
    I_AUDIO_PCM_L  => S_audio & "0000",
    I_AUDIO_PCM_R  => S_audio & "0000",
    O_TMDS_D0      => HDMI_D(0),
    O_TMDS_D1      => HDMI_D(1),
    O_TMDS_D2      => HDMI_D(2),
    O_TMDS_CLK     => HDMI_CLK
  );

end struct;
