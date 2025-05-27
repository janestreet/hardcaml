module mux4 (
  input [1:0] address,
  input [7:0] a, b, c, d,
  output reg [7:0] q
);
   always @* begin
      case (address)
        3'd0: q <= a;
        3'd1: q <= b;
        3'd2: q <= c;
        default: q <= d;
      endcase
   end

endmodule
