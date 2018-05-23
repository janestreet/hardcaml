module B : Hardcaml.Comb.S with type t = Hardcaml.Bits.t

type exarray =
  { mutable len : int
  ; mutable data : B.t array }
val make : unit -> exarray
val extend : exarray -> unit
val set : exarray -> int -> B.t -> unit
val get : exarray -> int -> B.t
val length : exarray -> int
val data : exarray -> B.t array

type wave =
  { name  : string
  ; nbits : int
  ; data  : exarray }
type waves = wave array

val wrap : B.t Hardcaml.Cyclesim.Api.cyclesim -> B.t Hardcaml.Cyclesim.Api.cyclesim * wave array

module Gui :
sig
  val render_1 :
    int * int ->
    int * int ->
    int -> Dom_html.canvasRenderingContext2D Js.t -> B.t array -> unit
  val render_n :
    ('a -> string) ->
    int * int ->
    int * int -> int -> Dom_html.canvasRenderingContext2D Js.t -> 'a array -> unit

  val mk_wave_table : #Dom.node Js.t -> int -> int -> wave array -> unit
end

