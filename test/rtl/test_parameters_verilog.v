module test_parameters_verilog
  #(
    parameter an_int = 0,
    parameter a_bool = 1'b0,
    parameter a_string = "hello",
    parameter a_real = 0.0,
    parameter a_bit = 1'b0,
    parameter a_bit_vector = 4'b0000,
    parameter a_std_logic = 1'bX,
    parameter a_std_ulogic = 1'bX,
    parameter a_std_logic_vector = 4'b0000,
    parameter a_std_ulogic_vector = 4'b0000
  )
  (
    input a,
    output [1:0] b
  );

  initial begin
    $write("VERILOG\n");
    $write("an_int              %d\n", an_int);
    $write("a_bool              %b\n", a_bool);
    $write("a_string            %s\n", a_string);
    $write("a_real              %f\n", a_real);
    $write("a_bit               %b\n", a_bit);
    $write("a_bit_vector        %b\n", a_bit_vector);
    $write("a_std_logic         %b\n", a_std_logic);
    $write("a_std_ulogic        %b\n", a_std_ulogic);
    $write("a_std_logic_vector  %b\n", a_std_logic_vector);
    $write("a_std_ulogic_vector %b\n", a_std_ulogic_vector);
  end

  assign b = {a,a};
endmodule
