module priority_encoder (
  input [3:0] sel,
  input [7:0] a, b, c, d,
  output reg [7:0] q
);

  always @* begin
    if (sel[3]) q <= d;
    else if (sel[2]) q <= c;
    else if (sel[1]) q <= b;
    else q <= a;
  end

endmodule


