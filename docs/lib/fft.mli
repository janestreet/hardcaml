val fft : Complex.t array -> Complex.t array
val ifft : Complex.t array -> Complex.t array
val test : unit -> unit

open Hardcaml

module Dcomplex : sig
  type 'a t =
    { re : 'a
    ; im : 'a
    }
  [@@deriving hardcaml]
end

module Write_port : sig
  type 'a t =
    { enable : 'a
    ; address : 'a
    ; data : 'a Dcomplex.t
    }
  [@@deriving hardcaml]
end

module I : sig
  type 'a t =
    { clock : 'a
    ; clear : 'a
    ; start : 'a
    ; write : 'a Write_port.t
    ; read_address : 'a
    }
  [@@deriving hardcaml]
end

module O : sig
  type 'a t =
    { done_ : 'a
    ; data_out : 'a Dcomplex.t
    }
  [@@deriving hardcaml]
end

val create : Interface.Create_fn(I)(O).t
