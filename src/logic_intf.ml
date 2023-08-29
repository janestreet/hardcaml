open Base

module type Std_logic = sig
  type t =
    | U (** Uninitialized *)
    | X (** Unknown *)
    | L0 (** Logic 0 *)
    | L1 (** Logic 1 *)
    | Z (** High impedance *)
    | W (** Weak - neither prefer 0 or 1 *)
    | L (** Weak - prefer 0 *)
    | H (** Weak - prefer 1 *)
    | Don't_care (** Dont care *)
  [@@deriving compare, enumerate, sexp, variants]

  include Equal.S with type t := t

  (** Provide the index of [t] in textual order.  When passing a std_logic parameter from
      verilog to vhdl, we need to encode this type into an integer.  For example, L1 =
      4'd3. *)
  val to_int : t -> int

  (** The OCaml [char] used in [of_char] and [to_char] is the same as used in VHDL. *)
  val of_char_exn : char -> t

  val to_char : t -> char
end

module type Four_state = sig
  type t =
    | X
    | Z
    | L0
    | L1
  [@@deriving compare, enumerate, sexp, variants]

  include Equal.S with type t := t

  (** Provide the index of [t] in textual order. *)
  val to_int : t -> int

  val of_char_exn : char -> t
  val to_char : t -> char
end

module type Logic = sig
  module type Std_logic = Std_logic
  module type Four_state = Four_state

  module Std_logic : Std_logic
  module Four_state : Four_state
  module Std_logic_vector : Bits_list.Comb with type t = Std_logic.t list
  module Bit_vector : Bits_list.Comb with type t = int list
  module Four_state_vector : Bits_list.Comb with type t = Four_state.t list
end
