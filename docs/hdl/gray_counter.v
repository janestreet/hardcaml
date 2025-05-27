module gray_counter #(
  parameter N = 4
) (
  input clock, clear,
  output reg [N-1:0] q
);
  reg [N-1:0] count;

   always @(posedge clock) begin
     if (clear) begin
        count <= 0;
        q <= 0;
      end else begin
        count <= count + 1;
        q <= { count[N-1], count[N-1:1] ^ count[N-2:0] };
      end
   end

endmodule
