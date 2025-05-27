module rom (
  input [2:0] address,
  output reg [6:0] q
);

  always @* begin
    case (address)
      3'd0: q <= 7'd0;
      3'd1: q <= 7'd10;
      3'd2: q <= 7'd20;
      3'd3: q <= 7'd30;
      3'd4: q <= 7'd40;
      3'd5: q <= 7'd50;
      3'd6: q <= 7'd60;
      default: q <= 7'd70;
    endcase
  end

endmodule
