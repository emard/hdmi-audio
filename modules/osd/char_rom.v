module char_rom
(
  input wire clock,
  input wire [11:0] addr,
  output wire [7:0] data
);
  parameter [9:0] char_rom_len = 128*5; // char ROM byte len

  reg [7:0] R_data;

  (* synthesis, rom_block = "ROM_CELL XYZ01" *)
  reg [7:0] disp_rom [char_rom_len-1:0];

  initial
    begin
      $readmemh("chars_5x7.vhex", disp_rom);
    end
  
  always@(posedge clock)
    begin
      R_data <= disp_rom[addr];
    end

  assign data = R_data;
endmodule
