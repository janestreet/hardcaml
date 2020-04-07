open! Import

(* This allows the creation of sequential hardware designs using [if], [while] and
   [assignments]. *)
module type Same = sig
  type var
  type 'a recipe
  type 'a same

  val smap : f:(var -> Signal.t) -> var same -> Signal.t same
  val szip : var same -> Signal.t same -> (var * Signal.t) list
  val new_var : unit -> var same recipe
  val read : var same -> Signal.t same recipe
  val rewrite : (Signal.t same -> Signal.t same) -> var same -> var same -> unit recipe
  val apply : f:(Signal.t same -> Signal.t same) -> var same -> unit recipe
  val set : var same -> Signal.t same -> unit recipe

  val if_
    :  (Signal.t same -> Signal.t)
    -> var same
    -> then_:'a recipe
    -> else_:'b recipe
    -> unit recipe

  val while_ : (Signal.t same -> Signal.t) -> var same -> do_:'a recipe -> 'a recipe
end

module type Recipe = sig
  type var
  type 'a t

  include Monad.S with type 'a t := 'a t

  (** skip 1 cycle *)
  val skip : unit t

  (** skip n cycles *)
  val wait : int -> unit t

  (** Perform ts in parallel.  [comb_fin] controls the finish signal generation.  When
      false and extra cycle is taken after the ts complete to generate the [fin]
      signal.  Otherwise extra combinatorial logic is generated to ensure the [fin] signal
      toggles on the same cycle as the last t to complete. *)
  val par : ?comb_fin:bool -> 'a t list -> 'a list t

  val par2 : ?comb_fin:bool -> 'a t -> 'b t -> ('a * 'b) t
  val ( ||| ) : 'a t -> 'b t -> ('a * 'b) t

  (** [cond c t f]  performs [t] if [c] is high, otherwise performs [f] *)
  val cond : Signal.t -> 'a t -> 'b t -> unit t

  (** [iter c t] perform [t] while [c] is high *)
  val iter : Signal.t -> 'a t -> 'a t

  (** perform t forever *)
  val forever : 'a t -> 'a t

  (** wait until [t] is low *)
  val wait_while : Signal.t -> unit t

  (** wait until [t] is high *)
  val wait_until : Signal.t -> unit t

  (** follow t and get result *)
  val follow : clock:Signal.t -> enable:Signal.t -> Signal.t -> 'a t -> Signal.t * 'a

  (** create an new [n] bit register *)
  val new_var : ?name:string -> int -> var t

  (** read value of register *)
  val read_var : var -> Signal.t t

  (** assign list of registers - takes 1 cycle *)
  val assign : (var * Signal.t) list -> unit t

  (** write register with value *)
  val write_var : var -> Signal.t -> unit t

  (** modify current value of resgiter *)
  val modify_var : (Signal.t -> Signal.t) -> var -> unit t

  (** read a register, modify value, write a second register *)
  val rewrite_var : (Signal.t -> Signal.t) -> var -> var -> unit t

  module type Same = Same with type var := var with type 'a recipe := 'a t

  module Same (X : Interface.Pre) : Same with type 'a same = 'a X.t
  module SVar : Same with type 'a same = 'a
  module SList : Same with type 'a same = 'a list
  module SArray : Same with type 'a same = 'a array
  module STuple2 : Same with type 'a same = 'a * 'a
  module STuple3 : Same with type 'a same = 'a * 'a * 'a
end
