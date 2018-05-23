module test (
  input clock,
  input reset,
  input [1:0] a,
  input [2:0] b,
  output reg [3:0] c,
  output reg [4:0] d
);

  always @(posedge clock or posedge reset) begin
    if (reset) begin
      c <= 0;
      d <= 0;
    end else begin
      c <= a + b;
      d <= a * b;
    end
  end

endmodule

