# 1.2 Installing the Opensource Release

Hardcaml can be installed with opam. We highly recommend using Hardcaml with OxCaml (a
bleeding-edge OCaml compiler), which includes some Jane Street compiler extensions and
maintains the latest version of Hardcaml. Note that when looking at Hardcaml GitHub
repositories, the OxCaml version is in a branch named `with-extensions`.

Install [opam, the OxCaml compiler, and some basic developer
tools](https://oxcaml.org/get-oxcaml/) to get started.

For additional information on setting up the OCaml toolchain and editor support, see [Real
World OCaml](https://dev.realworldocaml.org/install.html).

We recommend the following Hardcaml packages to get started

```
opam install hardcaml hardcaml_waveterm ppx_hardcaml core utop dune
```
This will install the core Hardcaml library, a waveform viewer which is helpful for
building tests, and the Hardcaml PPX, as well as an OCaml standard library and build
system.


# Basic Usage

Hardcaml can be used with utop (or another OCaml top level). 

```
#require "hardcaml"
open Base
open Hardcaml
```

This provides a great environment to learn the `Bits` API. The following
utop command makes things nicer to play with

```
#install_printer Bits.pp
```

Note that we have opened the [Base](https://opensource.janestreet.com/base/) standard
library here - all examples given in this documentation will assume that it has been
opened.

# Dune

The following is an example Dune file for linking to Hardcaml. Note that `ppx_hardcaml`
has to be explicitly specified as a preprocessor when writing Hardcaml sources.

```
(library
  (name my_hardcaml_lib)
  (libraries base hardcaml)
  (preprocess (pps ppx_jane ppx_hardcaml)))
```

# Reporting Bugs

If you encounter any bugs or issues with the open-source Hardcaml release, please report
them on the [Hardcaml GitHub](https://github.com/janestreet/hardcaml).
