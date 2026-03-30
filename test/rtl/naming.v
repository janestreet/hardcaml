module \a%sillyname
(
  // Are these names technically the same?
  input \foo ,
  // Yes, they are - this will not compile and clashes with the extended identifier version.
  //input foo

  // What about reserved words?
  input \module ,

  // Cases sensitive
  input \Foo
);

endmodule

module inst;

    wire foo;

    wire \module , \Foo ;

    \a%sillyname \the_a%fillygame
      ( .foo(foo), .\module (\module ), .\Foo (\Foo ));

  initial begin
    $dumpfile("names.vcd");
    $dumpvars(0);
  end
endmodule
