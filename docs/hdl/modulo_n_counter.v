module modulo_n_counter #(
  parameter width = 4,
  parameter n = 12
) (
  input clock, clear, increment,
  output reg [width-1:0] q
);

  always @(posedge clock)
  begin
    if (clear) 
      q <= 0;
    else if (increment)
      if (q == (n-1)) q <= 0;
      else q <= q + 1;
  end

endmodule 
