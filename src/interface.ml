open! Import
include Interface_intf

module Create_fn (I : S) (O : S) = struct
  type 'a t = 'a I.t -> 'a O.t

  let sexp_of_t _ _ =
    [%message "" ~inputs:(I.t : (string * int) I.t) ~outputs:(O.t : (string * int) O.t)]
  ;;
end

module Ast = struct
  module rec Ast : sig
    type t = Field.t list [@@deriving sexp_of]
  end = struct
    type t = Field.t list [@@deriving sexp_of]
  end

  and Field : sig
    type t =
      { name : string
      ; type_ : Type.t
      ; sequence : Sequence.t option
      ; doc : string option
      }
    [@@deriving sexp_of]
  end = struct
    type t =
      { name : string
      ; type_ : Type.t
      ; sequence : Sequence.t option
      ; doc : string option
      }
    [@@deriving sexp_of]
  end

  and Type : sig
    type t =
      | Signal of
          { bits : int
          ; rtlname : string
          }
      | Module of
          { name : string
          ; ast : Ast.t
          }
    [@@deriving sexp_of]
  end = struct
    type t =
      | Signal of
          { bits : int
          ; rtlname : string
          }
      | Module of
          { name : string
          ; ast : Ast.t
          }
    [@@deriving sexp_of]
  end

  and Sequence : sig
    module Kind : sig
      type t =
        | Array
        | List
      [@@deriving sexp_of]
    end

    type t =
      { kind : Kind.t
      ; length : int
      }
    [@@deriving sexp_of]
  end = struct
    module Kind = struct
      type t =
        | Array
        | List
      [@@deriving sexp_of]
    end

    type t =
      { kind : Kind.t
      ; length : int
      }
    [@@deriving sexp_of]
  end

  type t = Ast.t [@@deriving sexp_of]
end

module Make (X : Pre) : S with type 'a t := 'a X.t = struct
  include X

  let port_names = map t ~f:fst
  let port_widths = map t ~f:snd
  let to_list_rev x = to_list x |> List.rev
  let to_alist x = to_list (map2 port_names x ~f:(fun name x -> name, x))

  let of_alist x =
    map port_names ~f:(fun name ->
      match List.Assoc.find x name ~equal:String.equal with
      | Some x -> x
      | None ->
        raise_s
          [%message
            "[Interface_extended.of_alist] Field not found in interface"
              ~missing_field_name:(name : string)
              ~input:(x : (string * _) list)
              ~interface:(port_widths : int X.t)])
  ;;

  let zip a b = map2 a b ~f:(fun a b -> a, b)
  let zip3 a b c = map2 (zip a b) c ~f:(fun (a, b) c -> a, b, c)
  let zip4 a b c d = map2 (zip a b) (zip c d) ~f:(fun (a, b) (c, d) -> a, b, c, d)

  let zip5 a b c d e =
    map2 (zip3 a b c) (zip d e) ~f:(fun (a, b, c) (d, e) -> a, b, c, d, e)
  ;;

  let map3 a b c ~f = map ~f:(fun (a, b, c) -> f a b c) (zip3 a b c)
  let map4 a b c d ~f = map ~f:(fun (a, b, c, d) -> f a b c d) (zip4 a b c d)
  let map5 a b c d e ~f = map ~f:(fun (a, b, c, d, e) -> f a b c d e) (zip5 a b c d e)
  let iter3 a b c ~f = ignore @@ map3 ~f a b c
  let iter4 a b c d ~f = ignore @@ map4 ~f a b c d
  let iter5 a b c d e ~f = ignore @@ map5 ~f a b c d e

  let equal equal_a t1 t2 =
    With_return.with_return (fun r ->
      iter2 t1 t2 ~f:(fun a1 a2 -> if not (equal_a a1 a2) then r.return false);
      true)
  ;;

  let fold a ~init ~f =
    let init = ref init in
    iter a ~f:(fun a -> init := f !init a);
    !init
  ;;

  let fold2 a b ~init ~f = fold (zip a b) ~init ~f:(fun c (a, b) -> f c a b)

  let scan a ~init ~f =
    let acc = ref init in
    map a ~f:(fun a ->
      let acc', field = f !acc a in
      acc := acc';
      field)
  ;;

  let scan2 a b ~init ~f = scan (zip a b) ~init ~f:(fun c (a, b) -> f c a b)

  let offsets ?(rev = false) () =
    let rec loop fields ~offset =
      match fields with
      | [] -> []
      | (name, width) :: fields -> (name, offset) :: loop fields ~offset:(offset + width)
    in
    loop (if rev then to_list_rev t else to_list t) ~offset:0 |> of_alist
  ;;

  let of_interface_list ts =
    List.fold
      (List.rev ts)
      ~init:(map t ~f:(fun _ -> []))
      ~f:(fun ac t -> map2 t ac ~f:(fun h t -> h :: t))
  ;;

  let to_interface_list t =
    let lengths = map t ~f:List.length in
    let distinct_lengths = fold lengths ~init:(Set.empty (module Int)) ~f:Set.add in
    match Set.to_list distinct_lengths with
    | [] -> []
    | [ length ] ->
      let rec loop length t =
        if length = 0
        then []
        else map t ~f:List.hd_exn :: loop (length - 1) (map t ~f:List.tl_exn)
      in
      loop length t
    | _ ->
      raise_s
        [%message
          "[Interface_extended.to_interface_list] field list lengths must be the same"
            (lengths : int t)]
  ;;

  module Make_comb (Comb : Comb.S) = struct
    type comb = Comb.t [@@deriving sexp_of]
    type t = Comb.t X.t [@@deriving sexp_of]

    let widths t = map t ~f:Comb.width

    let assert_widths x =
      iter2 (widths x) t ~f:(fun actual_width (port_name, expected_width) ->
        if actual_width <> expected_width
        then
          raise_s
            [%message
              "Port width mismatch in interface"
                (port_name : string)
                (expected_width : int)
                (actual_width : int)])
    ;;

    let of_int i = map port_widths ~f:(fun b -> Comb.of_int ~width:b i)
    let of_ints i = map2 port_widths i ~f:(fun width -> Comb.of_int ~width)
    let const = of_int
    let consts = of_ints

    let pack ?(rev = false) t =
      if rev then to_list t |> Comb.concat_msb else to_list_rev t |> Comb.concat_msb
    ;;

    let unpack ?(rev = false) comb =
      let rec loop fields ~offset =
        match fields with
        | [] -> []
        | (name, width) :: fields ->
          (name, Comb.select comb (offset + width - 1) offset)
          :: loop fields ~offset:(offset + width)
      in
      loop (if rev then to_list_rev t else to_list t) ~offset:0 |> of_alist
    ;;

    let mux s l = map ~f:(Comb.mux s) (of_interface_list l)
    let mux2 s h l = mux s [ l; h ]
    let concat l = map ~f:Comb.concat_msb (of_interface_list l)

    let distribute_valids (ts : (comb, t) With_valid.t2 list) =
      List.map ts ~f:(fun { valid; value } ->
        map value ~f:(fun value -> { With_valid.valid; value }))
    ;;

    let collect_valids (t : comb With_valid.t X.t) =
      { With_valid.valid =
          (match to_list t with
           | { valid; _ } :: _ -> valid
           | [] -> raise_s [%message "[priority_select] interface has no fields"])
      ; value = map t ~f:(fun { valid = _; value } -> value)
      }
    ;;

    let priority_select ?branching_factor (ts : (comb, t) With_valid.t2 list)
      : (comb, t) With_valid.t2
      =
      if List.is_empty ts
      then raise_s [%message "[priority_select] requires at least one input"];
      let ts = distribute_valids ts in
      let t = map (of_interface_list ts) ~f:(Comb.priority_select ?branching_factor) in
      collect_valids t
    ;;

    let priority_select_with_default
          ?branching_factor
          (ts : (comb, t) With_valid.t2 list)
          ~(default : t)
      =
      if List.is_empty ts
      then
        raise_s [%message "[priority_select_with_default] requires at least one input"];
      let ts = distribute_valids ts in
      map2 (of_interface_list ts) default ~f:(fun t default ->
        Comb.priority_select_with_default ?branching_factor t ~default)
    ;;

    let onehot_select ?branching_factor (ts : (comb, t) With_valid.t2 list) =
      if List.is_empty ts
      then raise_s [%message "[onehot_select] requires at least one input"];
      let ts = distribute_valids ts in
      map (of_interface_list ts) ~f:(fun t -> Comb.onehot_select ?branching_factor t)
    ;;
  end

  module type Comb = Comb with type 'a interface := 'a t

  module Of_bits = Make_comb (Bits)

  module Of_signal = struct
    include Make_comb (Signal)

    let assign t1 t2 = iter2 t1 t2 ~f:Signal.assign
    let ( <== ) = assign

    let wires ?(named = false) ?from () =
      let wires =
        match from with
        | None -> map port_widths ~f:Signal.wire
        | Some x -> map x ~f:Signal.wireof
      in
      if named then map2 wires port_names ~f:Signal.( -- ) else wires
    ;;

    let inputs () = wires () ~named:true
    let outputs t = wires () ~from:t ~named:true

    let apply_names ?(prefix = "") ?(suffix = "") ?(naming_op = Signal.( -- )) t =
      map2 t port_names ~f:(fun s n -> naming_op s (prefix ^ n ^ suffix))
    ;;
  end
end

module Make_enums (Enum : Interface_intf.Enum) = struct
  module Make_pre (M : sig
      val t : string * int
    end) =
  struct
    type 'a t = 'a [@@deriving sexp_of]

    let to_list t = [ t ]
    let map t ~f = f t
    let map2 a b ~f = f a b
    let iter a ~f = f a
    let iter2 a b ~f = f a b
    let t = M.t
  end

  let num_enums = List.length Enum.all

  module Binary = struct
    let width = Int.ceil_log2 (List.length Enum.all)

    module Pre = Make_pre (struct
        let t = "binary_variant", width
      end)

    include Pre
    include Make (Pre)

    let of_enum (type a) (module Comb : Comb.S with type t = a) enum =
      Comb.of_int ~width (Enum.Variants.to_rank enum)
    ;;

    let to_enum =
      List.map Enum.all ~f:(fun variant -> Enum.Variants.to_rank variant, variant)
      |> Map.of_alist_exn (module Int)
    ;;

    let to_enum t = Map.find_exn to_enum (Bits.to_int t)

    let mux (type a) (module Comb : Comb.S with type t = a) ~(default : a) selector cases
      =
      let out_cases = Array.create ~len:num_enums default in
      List.iter cases ~f:(fun (enum, value) ->
        out_cases.(Enum.Variants.to_rank enum) <- value);
      Comb.mux selector (Array.to_list out_cases)
    ;;

    module For_testing = struct
      let set t enum = t := of_enum (module Bits) enum
      let get t = to_enum !t
    end
  end

  module One_hot = struct
    let width = List.length Enum.all

    module Pre = Make_pre (struct
        let t = "ont_hot_variant", width
      end)

    include Pre
    include Make (Pre)

    let of_enum (type a) (module Comb : Comb.S with type t = a) enum =
      Comb.of_int ~width (1 lsl Enum.Variants.to_rank enum)
    ;;

    let to_enum =
      List.map Enum.all ~f:(fun variant -> 1 lsl Enum.Variants.to_rank variant, variant)
      |> Map.of_alist_exn (module Int)
    ;;

    let to_enum t = Map.find_exn to_enum (Bits.to_int t)

    let mux (type a) (module Comb : Comb.S with type t = a) ~(default : a) selector cases
      =
      let out_cases = Array.create ~len:num_enums default in
      List.iter cases ~f:(fun (enum, value) ->
        out_cases.(Enum.Variants.to_rank enum) <- value);
      List.map2_exn
        (Comb.bits_lsb selector)
        (Array.to_list out_cases)
        ~f:(fun valid value -> { With_valid.valid; value })
      |> Comb.onehot_select
    ;;

    module For_testing = struct
      let set t enum = t := of_enum (module Bits) enum
      let get t = to_enum !t
    end
  end
end

module Empty = struct
  type 'a t = None [@@deriving sexp_of]

  include Make (struct
      type nonrec 'a t = 'a t [@@deriving sexp_of]

      let t = None
      let iter _ ~f:_ = ()
      let iter2 _ _ ~f:_ = ()
      let map _ ~f:_ = None
      let map2 _ _ ~f:_ = None
      let to_list _ = []
    end)
end

module type S_with_ast = sig
  include S

  val ast : Ast.t
end
