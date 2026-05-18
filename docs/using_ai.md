# 8. Using AI with Hardcaml

# Using AI to write Hardcaml

At Jane Street we have found that AI models are able to parse Hardcaml, as well as being
able to write testbenches quite well when given a few hand-crafted prompts.

The following prompts linked under each section can be provided to AI models (e.g. via
AGENTS.md files or context bundles) to help them write better Hardcaml code.

## Available Prompts

### Hardcaml Design Guidelines

This [prompt](http://github.com/janestreet/hardcaml_agents_docs/hardcaml_design_guideline.md) covers Hardcaml module
conventions including Config modules, ppx\_hardcaml, Signal and Always API usage, naming
with `let%hw` / `let%hw_var`, operator best practices, mux2 chaining, and common pitfalls
to avoid.

### Hardcaml Simulation Guidelines

This [prompt](http://github.com/janestreet/hardcaml_agents_docs/hardcaml_simulation_guideline.md) covers the Bits
API, Hardcaml\_lws (for writing concurrent simulation tasks), Cyclesim\_harness usage,
Step testbench patterns, and conventions for instantiating expect tests over
configurations.

### Hardcaml Common Libraries

This [prompt](http://github.com/janestreet/hardcaml_agents_docs/hardcaml_common_libraries.md) covers commonly used
Hardcaml libraries such as AXI Stream.
