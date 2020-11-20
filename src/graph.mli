(** Write circuit as graph. *)

open Base

val write_dot_rank : Stdio.Out_channel.t -> Circuit.t -> unit

(** write a GDL (graph description language) file of the given circuit *)
val write_gdl
  :  ?names:bool
  -> ?widths:bool
  -> ?consts:bool
  -> ?clocks:bool
  -> Stdio.Out_channel.t
  -> Circuit.t
  -> unit

(** launch aisee3 to visualize the given circuit *)
val aisee3
  :  ?args:string
  -> ?names:bool
  -> ?widths:bool
  -> ?consts:bool
  -> ?clocks:bool
  -> Circuit.t
  -> unit
[@@deprecated "[since 2017-11] aisee3 is no longer available."]
