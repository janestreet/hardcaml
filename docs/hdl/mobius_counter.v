module mobius_counter #(
  parameter N = 4
) (
  input clock, clear,
  output reg [N-1:0] q
);

  always @(posedge clock) begin
    if (clear)
      q <= 0;
    else
      q <= { ~q[0], q[N-1:1] };
  end 

endmodule



