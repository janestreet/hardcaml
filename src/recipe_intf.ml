(** Hardware generation in an imperative style *)

(** create sequential hardware designs using [if], [while] and [assignment] *)

open! Import

module type Same = sig
  type var
  type 'a recipe
  type 'a same

  val smap : f:(var -> Signal.t) -> var same -> Signal.t same
  val szip : var same -> Signal.t same -> (var * Signal.t) list
  val newVar : unit -> var same recipe
  val read : var same -> Signal.t same recipe
  val rewrite : (Signal.t same -> Signal.t same) -> var same -> var same -> unit recipe
  val apply : (Signal.t same -> Signal.t same) -> var same -> unit recipe
  val set : var same -> Signal.t same -> unit recipe

  val ifte
    :  (Signal.t same -> Signal.t)
    -> var same
    -> 'a recipe
    -> 'b recipe
    -> unit recipe

  val while_ : (Signal.t same -> Signal.t) -> var same -> 'a recipe -> 'a recipe
end

module type Recipe = sig
  type var
  type inp
  type env
  type 'a recipe

  module Monad : sig
    val return : 'a -> 'a recipe
    val bind : 'a recipe -> ('a -> 'b recipe) -> 'b recipe
    val ( >>= ) : 'a recipe -> ('a -> 'b recipe) -> 'b recipe
    val ( >> ) : 'a recipe -> 'b recipe -> 'b recipe
  end

  (** skip 1 cycle *)
  val skip : unit recipe

  (** skip n cycles *)
  val wait : int -> unit recipe

  (** Perform recipes in parallel.  [comb_fin] controls the finish signal generation.  When
      false and extra cycle is taken after the recipes complete to generate the [fin]
      signal.  Otherwise extra combinatorial logic is generated to ensure the [fin] signal
      toggles on the same cycle as the last recipe to complete. *)
  val par : ?comb_fin:bool -> 'a recipe list -> 'a list recipe

  val par2 : ?comb_fin:bool -> 'a recipe -> 'b recipe -> ('a * 'b) recipe
  val ( ||| ) : 'a recipe -> 'b recipe -> ('a * 'b) recipe

  (** [cond c t f]  performs [t] if [c] is high, otherwise performs [f] *)
  val cond : Signal.t -> 'a recipe -> 'b recipe -> unit recipe

  (** [iter c t] perform [t] while [c] is high *)
  val iter : Signal.t -> 'a recipe -> 'a recipe

  (** perform recipe forever *)
  val forever : 'a recipe -> 'a recipe

  (** wait until [t] is low *)
  val waitWhile : Signal.t -> unit recipe

  (** wait until [t] is high *)
  val waitUntil : Signal.t -> unit recipe

  (** follow recipe and get result *)
  val follow : Signal.t -> 'a recipe -> Signal.t * 'a

  (** create an new [n] bit register *)
  val newVar : ?name:string -> int -> var recipe

  (** read value of register *)
  val readVar : var -> Signal.t recipe

  (** assign list of registers - takes 1 cycle *)
  val assign : (var * Signal.t) list -> unit recipe

  (** write register with value *)
  val writeVar : var -> Signal.t -> unit recipe

  (** modify current value of resgiter *)
  val modifyVar : (Signal.t -> Signal.t) -> var -> unit recipe

  (** read a register, modify value, write a second register *)
  val rewriteVar : (Signal.t -> Signal.t) -> var -> var -> unit recipe

  module type Same = Same with type var := var with type 'a recipe := 'a recipe

  module Same (X : Interface.Pre) : Same with type 'a same = 'a X.t
  module SVar : Same with type 'a same = 'a
  module SList : Same with type 'a same = 'a list
  module SArray : Same with type 'a same = 'a array
  module STuple2 : Same with type 'a same = 'a * 'a
  module STuple3 : Same with type 'a same = 'a * 'a * 'a
end
