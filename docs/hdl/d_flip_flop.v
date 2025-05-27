module d_flip_flop (
  input clock, clear, reset, enable, d,
  output reg q
);

  always @(posedge clock, posedge reset) begin
    if (reset)
      q <= 1'b1;
    else if (clear)
      q <= 1'b0;
    else if (enable)
      q <= d;
  end

endmodule
