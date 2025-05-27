module sequence_detector (
  input clock, clear, d,
  output reg detect
);

  localparam 
    S1 = 0, 
    S10 = 1, 
    S101 = 2;

  reg [1:0] state;

  always @(posedge clock) 
    if (clear) begin 
      state <= S1;
      detect <= 0;
    end else begin
      case (state)
        S1: 
          if (d) state <= S10;
          else state <= S1;
        S10: 
          if (d) state <= S1;
          else state <= S101;
        S101: 
          state <= S1;
      endcase

      detect <= (state == S101) & d;
    end

endmodule 
