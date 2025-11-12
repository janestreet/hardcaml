open! Core0

type ('a, 'b) t2 = ('a, 'b) Comb.with_valid2 =
  { valid : 'a
  ; value : 'b
  }
[@@deriving sexp, bin_io, equal ~localize, compare ~localize]

module T = struct
  type 'a t = ('a, 'a) t2 [@@deriving sexp, bin_io, equal ~localize, compare ~localize]

  let valid { valid; value = _ } = valid
  let value { valid = _; value } = value

  let value_with_default
    (type t)
    (module Comb : Comb.S with type t = t)
    { valid; value }
    ~default
    =
    Comb.mux2 valid value default
  ;;

  let map x ~f = { valid = f x.valid; value = f x.value }
  let map2 x y ~f = { valid = f x.valid y.valid; value = f x.value y.value }

  let iter x ~f =
    f x.valid;
    f x.value
  ;;

  let iter2 x y ~f =
    f x.valid y.valid;
    f x.value y.value
  ;;

  let to_list { valid; value } = [ valid; value ]
  let map_valid { valid; value } ~f = { valid = f valid; value }
  let map_value { valid; value } ~f = { valid; value = f value }

  let map_value2 (type t) (module Comb : Comb.S with type t = t) x y ~f =
    { valid = Comb.( &: ) x.valid y.valid; value = f x.value y.value }
  ;;

  let and_then (type t) (module Comb : Comb.S with type t = t) { valid; value } ~f =
    let result = f value in
    { valid = Comb.( &: ) valid result.valid; value = result.value }
  ;;

  let and_then2 (type t) (module Comb : Comb.S with type t = t) x y ~f =
    let result = f x.value y.value in
    { valid = Comb.(x.valid &: y.valid &: result.valid); value = result.value }
  ;;

  let to_option { valid; value } = Option.some_if (Bits.to_bool valid) value
  let wave_formats = { value = Signal.Type.default_wave_format; valid = Wave_format.Bit }

  let port_names_and_widths_dynamic ~nbits =
    { value = "value", nbits; valid = "valid", 1 }
  ;;
end

include T

module Fields = struct
  module type S = sig
    type 'a value
    type nonrec 'a t = 'a t value

    include Interface.S with type 'a t := 'a t

    val value_with_default
      :  (module Comb.S with type t = 'a)
      -> 'a t
      -> default:'a value
      -> 'a value

    val value : 'a t -> 'a value
  end

  module Make (M : Interface.Pre) = struct
    type 'a value = 'a M.t

    module Pre = struct
      type nonrec 'a t = 'a t M.t [@@deriving equal ~localize, compare ~localize, sexp_of]

      let map t ~f = M.map ~f:(map ~f) t [@nontail]
      let iter (t : 'a t) ~(f : 'a -> unit) = M.iter ~f:(iter ~f) t [@nontail]
      let map2 a b ~f = M.map2 a b ~f:(map2 ~f) [@nontail]
      let iter2 a b ~f = M.iter2 a b ~f:(iter2 ~f) [@nontail]

      let port_names_and_widths =
        M.map M.port_names_and_widths ~f:(fun (n, w) ->
          { value = n ^ "$value", w; valid = n ^ "$valid", 1 })
      ;;

      let to_list t = M.map t ~f:to_list |> M.to_list |> List.concat
    end

    include Pre
    include Interface.Make (Pre)

    let value_with_default (type a) (module Comb : Comb.S with type t = a) t ~default =
      M.map2 t default ~f:(fun { value; valid } default -> Comb.mux2 valid value default)
    ;;

    let value t = M.map t ~f:value
  end

  module M (X : T1) = struct
    module type S = S with type 'a value = 'a X.t
  end

  module Include = struct
    module type S = sig
      type 'a value

      module With_valid : S with type 'a value = 'a value
    end

    module type F = functor (X : Interface.Pre) -> S with type 'a value := 'a X.t

    module Make (X : Interface.Pre) = struct
      module With_valid = Make (X)
    end
  end
end

module Wrap = struct
  module type S = sig
    type 'a value
    type nonrec 'a t = ('a, 'a value) t2

    include Interface.S with type 'a t := ('a, 'a value) t2

    val value_with_default
      :  (module Comb.S with type t = 'a)
      -> 'a t
      -> default:'a value
      -> 'a value
  end

  module Make (M : Interface.Pre) = struct
    type 'a value = 'a M.t

    module Pre = struct
      type nonrec 'a t = ('a, 'a M.t) t2
      [@@deriving equal ~localize, compare ~localize, sexp_of]

      let map t ~f = { valid = f t.valid; value = M.map ~f t.value }

      let iter (t : 'a t) ~(f : 'a -> unit) =
        f t.valid;
        M.iter ~f t.value
      ;;

      let map2 a b ~f = { valid = f a.valid b.valid; value = M.map2 ~f a.value b.value }

      let iter2 a b ~f =
        f a.valid b.valid;
        M.iter2 ~f a.value b.value
      ;;

      let port_names_and_widths =
        { valid = "valid", 1
        ; value = M.map M.port_names_and_widths ~f:(fun (n, w) -> "value$" ^ n, w)
        }
      ;;

      let to_list t = t.valid :: M.to_list t.value
    end

    include Pre
    include Interface.Make (Pre)

    let value_with_default
      (type a)
      (module Comb : Comb.S with type t = a)
      { valid; value }
      ~default
      =
      let module I = Interface.Make (M) in
      let module Comb = I.Make_comb (Comb) in
      Comb.mux2 valid value default
    ;;
  end

  module M (X : T1) = struct
    type nonrec 'a t = ('a, 'a X.t) t2

    module type S = S with type 'a value = 'a X.t
  end

  module Include = struct
    module type S = sig
      type 'a value

      module With_valid : S with type 'a value = 'a value
    end

    module type F = functor (X : Interface.Pre) -> S with type 'a value := 'a X.t

    module Make (X : Interface.Pre) = struct
      module With_valid = Make (X)
    end
  end
end

let port_names = { valid = "valid"; value = "value" }
let port_widths ~width = { valid = 1; value = width }

module Vector (X : sig
    val width : int
  end) =
struct
  type 'a t = 'a T.t

  include Interface.Make (struct
      include T

      let port_names_and_widths =
        map2 port_names (port_widths ~width:X.width) ~f:(fun name width -> name, width)
      ;;
    end)
end

let apply_name_prefix_to_field f prefix t field_name = f t (prefix ^ "$" ^ field_name)

module Of_signal = struct
  let wires ?(named = false) ?from width =
    let wires =
      match from with
      | None -> port_widths ~width |> map ~f:Signal.wire
      | Some x ->
        let from_value_width = Signal.width x.value in
        if from_value_width <> width
        then
          raise_s
            [%message
              "The [value] width in [from] does not match the provided [width]."
                (from : Signal.t t option)
                (from_value_width : int)
                (width : int)]
        else map x ~f:Signal.wireof
    in
    if named then map2 wires port_names ~f:Signal.( -- ) else wires
  ;;

  let __ppx_auto_name t name =
    map2 t port_names ~f:(apply_name_prefix_to_field Signal.__ppx_auto_name name)
  ;;
end

module Of_always = struct
  let wire f ~width =
    port_widths ~width |> map ~f:(fun width -> Always.Variable.wire ~default:(f width) ())
  ;;

  let reg ?enable ?initialize_to ?reset_to ?clear ?clear_to spec ~width =
    port_widths ~width
    |> map ~f:(fun width ->
      Always.Variable.reg ?enable ?initialize_to ?reset_to ?clear ?clear_to spec ~width)
  ;;

  let __ppx_auto_name t name =
    map2 t port_names ~f:(apply_name_prefix_to_field Always.Variable.__ppx_auto_name name)
  ;;
end
