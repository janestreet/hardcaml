module bidirectional_shift_reg #(
  parameter N = 4
) (
  input clock, clear, enable, dir, d,
  output reg [N-1:0] q
);

  always @(posedge clock) begin
    if (clear)
      q <= 0;
    else
      if (enable) 
        if (dir)
          q <= { d, q[N-1:1] };
        else
          q <= { q[N-2:0], d };
  end 

endmodule
