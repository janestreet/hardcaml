# Installing the opensource release

Hardcaml can be installed with opam. I recommend the following
packages to get started

```
opam install hardcaml hardcaml_waveterm ppx_hardcaml
```

This will install the core Hardcaml library, a waveform viewer which
is helpful for building tests, and the hardcaml ppx.

To access the lastet packages or the most up-to-date version you may want
to try the Jane Street bleeding edge opam repository.

```
opam repo add janestreet-bleeding https://ocaml.janestreet.com/opam-repository
```

# Basic usage

Hardcaml can be used with utop (or another OCaml top level)

```
#require "hardcaml"
open Hardcaml
```

This provides a great environment to learn the `Bits` API. The following
utop command makes things nicer to play with

```
#install_printer Bits.pp
```

# Dune

The following is an example dune file for linking to Hardcaml

```
(library
  (name my_hardcaml_lib)
  (libraries base hardcaml)
  (preprocess (pps ppx_jane ppx_hardcaml)))
```
