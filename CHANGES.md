# v0.11

* Initial Jane Street public release
* Verion numbering synchronised with Jane Street releases
* Large scale refactoring of library and APIs
* Addition of test suite
* Merge and refactor hardcaml_examples into repository

# 1.2.0

* Reorganise and merge a bunch of modules (esp _ext_.ml) which were split out
  previously due to js\_of\_ocaml but is no longer needed.
* Replace oasis with ocamlbuild and topkg

# 1.1.1

* add out_port_next function to simulator - update on out_port reverts to old behaviour
* add dynamic simulation plugin back end registering (for llvmsim)
* fix vpi cosim module search path

# 1.1.0

* rework simulation so we get the correct output values (in all cases) after cycle
* various simulation hook points added to correctly support waveforms/combining etc
* add `Recipe` module - generates statemchines from imperative style descriptions
