include (
  Base :
    module type of struct
    include Base
  end
  with module Filename := Caml.Filename
  with module Int := Base.Int
  with module Int32 := Base.Int32
  with module Int64 := Base.Int64
  with module Map := Base.Map
  (* we want the [Map] from [map.ml] *)
  with module Marshal := Caml.Marshal
  with module Nativeint := Base.Nativeint)

module Int = struct
  include Base.Int

  let add = ( + )
  let logand = ( land )
  let lognot = lnot
  let logor = ( lor )
  let logxor = ( lxor )
  let mul = ( * )
  let sub = ( - )
end

module Int32 = struct
  include Base.Int32

  let add = ( + )
  let logand = ( land )
  let lognot = lnot
  let logor = ( lor )
  let logxor = ( lxor )
  let mul = ( * )
  let sub = ( - )
end

module Int64 = struct
  include Base.Int64

  let add = ( + )
  let logand = ( land )
  let lognot = lnot
  let logor = ( lor )
  let logxor = ( lxor )
  let mul = ( * )
  let sub = ( - )
end

module Nativeint = struct
  include Base.Nativeint

  let add = ( + )
  let logand = ( land )
  let lognot = lnot
  let logor = ( lor )
  let logxor = ( lxor )
  let mul = ( * )
  let sub = ( - )
end

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
