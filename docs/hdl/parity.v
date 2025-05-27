module parity (
  input [3:0] d,
  output q
);

  assign q = ^d;

endmodule
