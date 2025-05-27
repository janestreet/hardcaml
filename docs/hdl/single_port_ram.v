module single_port_ram (
  input clock, write_enable,
  input [2:0] address,
  input [7:0] write_data,
  output reg [7:0] read_data
);

  reg [7:0] mem[0:7];

  always @(posedge clock) begin
    if (write_enable)
      mem[address] <= write_data;
    read_data <= mem[address];
  end

endmodule
