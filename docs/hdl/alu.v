module alu (
  input [3:0] op,
  input [7:0] a, b,
  output reg [7:0] q
);

always @* begin
    case (op)
      0: q <= a + b;
      1: q <= a - b;
      2: q <= a * b;
      3: q <= a << 1;
      4: q <= a >> 1;
      5: q <= a & b;
      6: q <= a | b;
      7: q <= a ^ b;
      8: q <= ~a;
      9: q <= a < b;
      10: q <= a == b;
      default: q <= 0;
    endcase
  end

endmodule
