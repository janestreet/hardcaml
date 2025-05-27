open Hardcaml

module Jk_flip_flop : sig
  type fn = clock:Signal.t -> j:Signal.t -> k:Signal.t -> Signal.t

  val jk_flip_flop_1 : fn
  val jk_flip_flop_2 : fn
  val test : fn -> Hardcaml_waveterm.Waveform.t
end

module T_flip_flop : sig
  type fn = clock:Signal.t -> reset_n:Signal.t -> t:Signal.t -> Signal.t

  val t_flip_flop_1 : fn
  val t_flip_flop_2 : fn
  val test : fn -> Hardcaml_waveterm.Waveform.t
end

module D_flip_flop : sig
  type fn =
    clock:Signal.t
    -> reset:Signal.t
    -> clear:Signal.t
    -> enable:Signal.t
    -> d:Signal.t
    -> Signal.t

  val d_flip_flop : fn
end

module Ring_counter : sig
  type fn = n:int -> clock:Signal.t -> clear:Signal.t -> Signal.t

  val ring_counter : fn
  val test : unit -> Hardcaml_waveterm.Waveform.t
end

module Mobius_counter : sig
  type fn = n:int -> clock:Signal.t -> clear:Signal.t -> Signal.t

  val mobius_counter : fn
  val test : unit -> Hardcaml_waveterm.Waveform.t
end

module Modulo_n_counter : sig
  type config =
    { width : int
    ; n : int
    }

  type fn =
    config:config -> clock:Signal.t -> clear:Signal.t -> increment:Signal.t -> Signal.t

  val modulo_n_counter_1 : fn
  val modulo_n_counter_2 : fn
  val test : fn -> Hardcaml_waveterm.Waveform.t
end

module Gray_counter : sig
  type fn = n:int -> clock:Signal.t -> clear:Signal.t -> Signal.t

  val gray_counter_1 : fn
  val gray_counter_2 : fn
  val test : fn -> Hardcaml_waveterm.Waveform.t
end

module Bidirectional_shift_reg : sig
  type fn =
    n:int
    -> clock:Signal.t
    -> clear:Signal.t
    -> enable:Signal.t
    -> dir:Signal.t
    -> d:Signal.t
    -> Signal.t

  val bidirectional_shift_reg_1 : fn
  val bidirectional_shift_reg_2 : fn
  val test : fn -> Hardcaml_waveterm.Waveform.t
end

module Single_port_ram : sig
  val single_port_ram
    :  clock:Signal.t
    -> address:Signal.t
    -> write_enable:Signal.t
    -> write_data:Signal.t
    -> Signal.t
end

module Sync_fifo : sig
  type t =
    { data_out : Signal.t
    ; full : Signal.t
    ; empty : Signal.t
    }

  val sync_fifo
    :  clock:Signal.t
    -> clear:Signal.t
    -> write:Signal.t
    -> read:Signal.t
    -> data_in:Signal.t
    -> t
end
