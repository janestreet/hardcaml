include (
  Base :
    module type of struct
    include Base
  end
  with module Filename := Caml.Filename
  with module Marshal := Caml.Marshal)

let ( ** ) = Caml.( ** )
let ( @@ ) = Caml.( @@ )
let ( mod ) = Caml.( mod )
let at_exit = Caml.at_exit
let ceil = Caml.ceil
let floor = Caml.floor

module Out_channel = Stdio.Out_channel
module In_channel = Stdio.In_channel

let concat = String.concat
let error_s = Or_error.error_s
let fprintf = Out_channel.fprintf
let ok_exn = Or_error.ok_exn
let printf = Out_channel.printf
let sprintf = Printf.sprintf
let print_s sexp = Stdio.print_endline (sexp |> Sexp.to_string_hum)

module type Unstable = sig
  type t [@@deriving compare, sexp]
end

module type T_with_sexp_of = sig
  type t [@@deriving sexp_of]
end

module type T1_with_sexp_of = sig
  type 'a t [@@deriving sexp_of]
end
