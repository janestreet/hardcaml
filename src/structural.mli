(** Hardware generation API that includes tri-states - used for toplevel module
    generation.

    The API is very similar to {!module:Signal} except for instantiation which is done
    differently.
*)

include Structural_intf.Structural (** @inline *)
