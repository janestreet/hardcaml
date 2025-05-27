module t_flip_flop (
  input clock, reset_n, t,
  output reg q
);

  always @(posedge clock, negedge reset_n) begin
    if (!reset_n)
      q <= 1'b0;
    else if (t)
      q <= ~q;
  end

endmodule
