-- HDMI-audio test

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.ALL;
use ieee.numeric_std.all;

entity hdmiaudio_reverseu16 is
generic
(
  C_generic_hdmi: boolean := true
);
port
(
  clk_50MHz: in std_logic;
  --hdmi_sda: inout std_logic;
  --hdmi_scl: out std_logic;
  --hdmi_hec: out std_logic;
  --hdmi_cec: inout std_logic;
  --hdmi_d: out std_logic_vector(2 downto 0);
  --hdmi_clk: out std_logic
  hdmi_dp, hdmi_dn: out std_logic_vector(2 downto 0);
  hdmi_clkp, hdmi_clkn: out std_logic
);
end;

architecture struct of hdmiaudio_reverseu16 is
  signal clk_pixel, clk_pixel_shift: std_logic;
 
  signal S_vga_r, S_vga_g, S_vga_b: std_logic_vector(7 downto 0);
  signal S_vga_vsync, S_vga_hsync: std_logic;
  signal S_vga_vblank, S_vga_blank: std_logic;
  signal S_audio: std_logic_vector(11 downto 0);

  signal hdmi_d: std_logic_vector(2 downto 0);
  signal hdmi_clk: std_logic;

  signal reset        : std_logic;
  signal clock_stable : std_logic;
  signal R_pixel_blink: std_logic_vector(25 downto 0) := (others => '0');
  signal R_pixel_shift_blink: std_logic_vector(27 downto 0) := (others => '0');
  signal R_beep: std_logic_vector(14 downto 0);
begin
  G_vendorspec_hdmi:
  if not C_generic_hdmi generate
  --clkgen_125_25: entity work.clk_25M_125M_25M
  --port map(
  --    inclk0 => clk_50MHz, c0 => clk_pixel_shift, c1 => clk_pixel,
  --    locked => clock_stable
  --);
  end generate;

  G_generic_hdmi:
  if C_generic_hdmi generate
    clkgen: entity work.pll_50M_250M_25M_83M333
    port map(
      inclk0 => clk_50MHz,    --  50 MHz input from board
      c0 => clk_pixel_shift,  -- 250 MHz
      c1 => clk_pixel,        --  25 MHz
      c2 => open              --  83.333 MHz
    );
  end generate;

  reset <= not clock_stable;

  -- VGA video generator - pixel clock synchronous
  vgabitmap: entity work.vga
  generic map -- workaround for wrong video size
  (
    C_resolution_x => 640,
    C_hsync_front_porch => 18
  )
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
    I_G	           => S_vga_g,
    I_B            => S_vga_b,
    I_BLANK        => S_vga_blank,
    I_HSYNC        => not S_vga_hsync,
    I_VSYNC        => not S_vga_vsync,
    I_AUDIO_ENABLE => '0', -- press right button to enable audio
    I_AUDIO_PCM_L  => S_audio & "0000",
    I_AUDIO_PCM_R  => S_audio & "0000",
    O_TMDS_D0      => HDMI_D(0),
    O_TMDS_D1      => HDMI_D(1),
    O_TMDS_D2      => HDMI_D(2),
    O_TMDS_CLK     => HDMI_CLK
  );
  
  hdmi_clkp <= hdmi_clk;
  hdmi_clkn <= not hdmi_clk;
  hdmi_dp <= hdmi_d;
  hdmi_dn <= not hdmi_d;

end struct;
