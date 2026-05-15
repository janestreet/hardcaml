# 5.9 Variant Interfaces

<!--
```ocaml
# open Base
# open Stdio
# open Hardcaml
# open Signal
# Hardcaml.Caller_id.set_mode Disabled
- : unit = ()
```
-->


# Variant Interfaces

The PPX deriver `[@@deriving hardcaml_variants]` defines a variant type where each case
wraps a different Hardcaml interface. A generated `Make` functor is applied with a chosen
case to produce a standard `Interface.S` module. This is useful when a hardware module
needs one of several interface shapes, chosen once at build time. Specializing to an
`Interface.S` allows the interface to be used in a module's I/O interface.

This is an elaboration-time mechanism — there are no tag bits or muxes in the generated
hardware. The choice is fixed when you apply the `Make` functor. Passing a value of the
wrong case will raise during circuit construction, preventing synthesis. For runtime
selection use `Signal.mux` or [Enums](enums.md).

## Definition

Each variant case carries one payload of the form `'a Module.t` where `Module` satisfies
`Interface.S`.

```ocaml
module Narrow = struct
  type 'a t =
    { addr : 'a [@bits 16]
    ; data : 'a [@bits 8]
    }
  [@@deriving hardcaml]
end

module Wide = struct
  type 'a t =
    { addr : 'a [@bits 32]
    ; data : 'a [@bits 64]
    }
  [@@deriving hardcaml]
end

module Bus = struct
  type 'a t =
    | Narrow of 'a Narrow.t
    | Wide of 'a Wide.t
  [@@deriving hardcaml_variants]
end
```

## Generated API

The deriver generates the following signature for the `Bus` example above:

```
(* Enum of variant cases *)
module Kind : sig
  type t =
    | Narrow
    | Wide
  [@@deriving sexp]
end

(* The full module type produced by Make *)
module type S = sig
  val kind : Kind.t

  include Interface.S with type 'a t = 'a t

  val narrow_exn : 'a t -> 'a Narrow.t
  val wide_exn : 'a t -> 'a Wide.t

  val map_variants
    :  'a t
    -> narrow:('a Narrow.t -> 'b Narrow.t)
    -> wide:('a Wide.t -> 'b Wide.t)
    -> 'b t
end

(* Functor to instantiate for a specific kind *)
module Make (_ : sig
    val kind : Kind.t
end) : S
```

In addition, `compare`, `equal`, and `sexp_of` derivations are included at the top level.

The extractor functions pull out the inner value, raising if it is the wrong case:

```ocaml
# let msg = Bus.Narrow { addr = 42; data = 7 };;
val msg : int Bus.t = Bus.Narrow {Narrow.addr = 42; data = 7}
# Bus.narrow_exn msg;;
- : int Narrow.t = {Narrow.addr = 42; data = 7}
```

Applying `Make` produces a full `Interface.S`.

```ocaml
module Narrow_bus = Bus.Make (struct let kind = Bus.Kind.Narrow end)
module Wide_bus = Bus.Make (struct let kind = Bus.Kind.Wide end)
```

```ocaml
# Narrow_bus.to_list Narrow_bus.port_names_and_widths;;
- : (string * int) list = [("addr", 16); ("data", 8)]
# Wide_bus.to_list Wide_bus.port_names_and_widths;;
- : (string * int) list = [("addr", 32); ("data", 64)]
```

## Mismatched variants

All values must match the configured kind. Mismatches raise immediately, so circuit
construction fails rather than producing silently wrong hardware.

```ocaml
# let narrow_val : int Bus.t = Narrow { addr = 1; data = 2 };;
val narrow_val : int Wide_bus.t = Bus.Narrow {Narrow.addr = 1; data = 2}
# Narrow_bus.map narrow_val ~f:(fun x -> x + 1);;
- : int Wide_bus.t = Bus.Narrow {Narrow.addr = 2; data = 3}
```

```ocaml
# let wide_val : int Bus.t = Wide { addr = 100; data = 200 };;
val wide_val : int Wide_bus.t = Bus.Wide {Wide.addr = 100; data = 200}
# Narrow_bus.map wide_val ~f:(fun x -> x + 1);;
Exception: ("mismatched tag" (kind Narrow) (t (Wide ((addr _) (data _)))))
```

```ocaml
# Narrow_bus.map2 narrow_val wide_val ~f:(fun a b -> a + b);;
Exception: ("mismatched tag" (kind Narrow) (t (Wide ((addr _) (data _)))))
```

## map_variants

`map_variants` operates on a value without committing to a specific kind. It takes a
labeled function for each case.

```ocaml
# let transform (msg : int Bus.t) : int Bus.t =
    Bus.map_variants msg
      ~narrow:(fun n -> Narrow.map n ~f:(fun x -> x * 2))
      ~wide:(fun w -> Wide.map w ~f:(fun x -> x + 100));;
val transform : int Wide_bus.t -> int Wide_bus.t = <fun>
# transform (Narrow { addr = 5; data = 3 });;
- : int Wide_bus.t = Bus.Narrow {Narrow.addr = 10; data = 6}
# transform (Wide { addr = 5; data = 3 });;
- : int Wide_bus.t = Bus.Wide {Wide.addr = 105; data = 103}
```
