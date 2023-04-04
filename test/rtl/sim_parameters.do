vlog test_parameters_verilog.v
vcom test_parameters_vhdl.vhd
vlog ../bin/test_instantiate_verilog.v
vcom ../bin/test_instantiate_vhdl.vhd

vsim test_parameter_instantiation_verilog
run
vsim test_parameter_instantiation_vhdl
run
quit -f
