open! Core

module Scope : sig
  type t =
    | Begin
    | Fork
    | Function
    | Module
    | Task
  [@@deriving sexp_of]
end

module Var_type : sig
  type t =
    | Event
    | Integer
    | Parameter
    | Real
    | Realtime
    | Reg
    | Supply0
    | Supply1
    | Time
    | Tri
    | Triand
    | Trior
    | Trireg
    | Tri0
    | Tri1
    | Wand
    | Wire
    | Wor
  [@@deriving sexp_of]
end

module Time_unit : sig
  type t =
    | S
    | Ms
    | Us
    | Ns
    | Ps
    | Fs
  [@@deriving sexp_of]
end

module Bit : sig
  type t =
    | V0
    | V1
    | Vx
    | Vz
  [@@deriving sexp_of]
end

module Reference : sig
  type t =
    { ref_name : string
    ; lindex : int
    ; rindex : int
    }
  [@@deriving sexp_of]
end

module Value_change : sig
  type t =
    | Scalar_value of Bit.t * string
    | Vector_value of Bit.t list * string
    | Real_value of float * string
  [@@deriving sexp_of]
end

module Var_decl : sig
  type t =
    { var_type : Var_type.t
    ; var_size : int
    ; var_id : string
    ; var_ref : Reference.t
    }
  [@@deriving sexp_of]
end

module Declaration : sig
  type t =
    | Comment of string
    | Date of string
    | Enddefinitions
    | Scope of Scope.t * string
    | Timescale of int * Time_unit.t
    | Upscope
    | Var of Var_decl.t
    | Version of string
  [@@deriving sexp_of]
end

module Simulation_command : sig
  type t =
    | Sim_dumpall of Value_change.t list
    | Sim_dumpoff of Value_change.t list
    | Sim_dumpon of Value_change.t list
    | Sim_dumpvars of Value_change.t list
    | Sim_comment of string
    | Sim_time of int
    | Sim_value_change of Value_change.t
  [@@deriving sexp_of]
end

type t =
  { declarations : Declaration.t list
  ; simulation_commands : Simulation_command.t list
  }
[@@deriving sexp_of]
