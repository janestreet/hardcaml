module test_jk_flip_flop;

  reg clock=0, j, k;
  wire q;

  jk_flip_flop dut (.clock(clock), .j(j), .k(k), .q(q));

  always #5 clock <= ~clock;

  initial begin
    $monitor("%t j=%b k=%b q=%b", $time, j, k, q);
    @(posedge clock);
    j <= 1;
    k <= 0;
    @(posedge clock);
    j <= 0;
    k <= 1;
    @(posedge clock);
    j <= 1;
    k <= 1;
    @(posedge clock);
    @(posedge clock);
    j <= 0;
    k <= 0;
    @(posedge clock);
    @(posedge clock);
    $finish;
  end

endmodule

module test_t_flip_flop;

  reg clock=0, reset_n = 1, t=0;
  wire q;

  t_flip_flop dut (.clock(clock), .reset_n(reset_n), .t(t), .q(q));

  always #5 clock <= ~clock;

  initial begin
    $monitor("%t reset_n=%b t=%b q=%b", $time, reset_n, t, q);

    #3;
    reset_n <= 0;
    #3;
    reset_n <= 1;
    #3;

    repeat (20) begin
      @(posedge clock);
      t <= $random;
    end

    $finish;
  end

endmodule

module test_d_flip_flop;

  reg clock=0, reset = 0, clear = 0, enable = 0, d=0;
  wire q;

  d_flip_flop dut (.clock(clock), .reset(reset), .clear(clear), .enable(enable), .d(d), .q(q));

  always #5 clock <= ~clock;

  initial begin
    $monitor("%t reset=%b clear=%b en=%b d=%b q=%b", $time, reset, clear, enable, d, q);

    reset <= 0;
    #3;
    reset <= 1;
    #3;
    reset <= 0;
    #3;

    @(posedge clock);
    clear <= 1;
    @(posedge clock);
    clear <= 0;

    repeat (20) begin
      @(posedge clock)
      enable <= $random;
      d <= $random;
    end

    $finish;
  end

endmodule

module test_ring_counter;

  reg clock=0, clear = 0;
  wire [3:0] q;

  ring_counter #(.N(4)) dut (.clock(clock), .clear(clear), .q(q));

  always #5 clock <= ~clock;

  initial begin
    $monitor("%t clear=%b q=%b", $time, clear, q);

    @(posedge clock);
    clear <= 1;
    @(posedge clock);
    clear <= 0;

    repeat (20) begin
      @(posedge clock);
    end

    $finish;
  end
endmodule

module test_mobius_counter;

  reg clock=0, clear = 0;
  wire [3:0] q;

  mobius_counter #(.N(4)) dut (.clock(clock), .clear(clear), .q(q));

  always #5 clock <= ~clock;

  initial begin
    $monitor("%t clear=%b q=%b", $time, clear, q);

    @(posedge clock);
    clear <= 1;
    @(posedge clock);
    clear <= 0;

    repeat (20) begin
      @(posedge clock);
    end

    $finish;
  end
endmodule

module test_single_port_ram;

  reg clock=0, write_enable;
  reg [2:0] address;
  reg [7:0] write_data;
  wire [7:0] read_data;

  single_port_ram
  dut (.clock(clock), .write_enable(write_enable), .address(address),
       .write_data(write_data), .read_data(read_data) );

  always #5 clock <= ~clock;

  initial begin
    $monitor("%t we=%b a=%h d=%h q=%h", $time, write_enable, address, write_data, read_data);
    repeat(20) begin
      @(posedge clock);
      write_enable <= $random;
      address <= $random;
      write_data <= $random;
    end
    $finish;
  end

endmodule

module test_sequence_detector;

  reg clock=0, clear=0, d;
  wire detect;

  sequence_detector dut (.clock(clock), .clear(clear), .d(d), .detect(detect));

  always #5 clock <= ~clock;

  wire [19:0] data = 20'b0110101110100010101;
  integer i = 0;

  initial begin
    $monitor("%t clear=%b d=%b detect=%b", $time, clear, d, detect);
    @(posedge clock);
    clear <= 1;
    @(posedge clock);
    clear <= 0;
    repeat(20) begin
      @(posedge clock);
      d <= data[i];
      i = i + 1;
    end
    $finish;
  end

endmodule

module test_modulo_n_counter;

  reg clock=0, clear=0, increment=0;
  wire [2:0] q;

  modulo_n_counter
    #(.width(3), .n(5))
    dut (.clock(clock), .clear(clear), .increment(increment), .q(q));

  always #5 clock <= ~clock;

  initial begin
    $monitor("%t clear=%b inc=%b q=%b", $time, clear, increment, q);
    @(posedge clock);
    clear <= 1;
    @(posedge clock);
    clear <= 0;
    repeat(20) begin
      @(posedge clock);
      increment <= $random;
    end
    $finish;
  end

endmodule

module test_gray_counter;

  reg clock=0, clear=0;
  wire [3:0] q;

  gray_counter
    #(.N(4))
    dut (.clock(clock), .clear(clear), .q(q));

  always #5 clock <= ~clock;

  initial begin
    $monitor("%t clear=%b q=%b", $time, clear, q);
    @(posedge clock);
    clear <= 1;
    @(posedge clock);
    clear <= 0;
    repeat(20) begin
      @(posedge clock);
    end
    $finish;
  end

endmodule

module test_bidirectional_shift_reg;

  reg clock=0, clear=0, enable=0, dir=0, d=0;
  wire [7:0] q;

  bidirectional_shift_reg
    #(.N(8))
    dut (.clock(clock), .clear(clear), .enable(enable), .dir(dir), .d(d), .q(q));

  always #5 clock <= ~clock;

  initial begin
    $monitor("%t clr=%b en=%b dir=%b d=%b q=%b", $time, clear, enable, dir, d, q);
    @(posedge clock);
    clear <= 1;
    @(posedge clock);
    clear <= 0;
    enable <= 1;
    dir <= 0;
    d <= 1;
    repeat(6) begin
      @(posedge clock);
    end
    dir <= 1;
    d <= 1;
    repeat(6) begin
      @(posedge clock);
    end
    $finish;
  end

endmodule

