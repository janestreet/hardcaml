module sync_fifo (
  input clock, clear, write, read,
  input [15:0] data_in,
  output reg [15:0] data_out,
  output full, empty
);

  reg [2:0] wptr, rptr;
  reg [15:0] mem[0:7];

  assign full = (wptr + 1) == rptr;
  assign empty = wptr == rptr;

  wire write_incr = write & !full;
  wire read_incr = read & !empty;

  always @(posedge clock) begin
    if (clear) begin
      rptr <= 0;
      wptr <= 0;
    end else begin
      if (write_incr) wptr <= wptr + 1;
      if (read_incr) rptr <= rptr + 1;
    end
    if (write_incr) mem[wptr] <= data_in;
    if (read_incr) data_out <= mem[rptr];
  end

endmodule
