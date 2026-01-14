open! Core0

include struct
  open Interface_intf

  module type Pre_partial = Pre_partial
  module type Pre = Pre
  module type S = S
  module type S_Of_signal = S_Of_signal
  module type Ast = Ast
  module type Empty = Empty
  module type Comb = Comb
end

module Create_fn (I : S) (O : S) = struct
  type t = Signal.t I.t -> Signal.t O.t

  let sexp_of_t _ =
    [%message
      ""
        ~inputs:(I.port_names_and_widths : (string * int) I.t)
        ~outputs:(O.port_names_and_widths : (string * int) O.t)]
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

type 'a with_valid = 'a Comb_intf.with_valid
type ('a, 'b) with_valid2 = ('a, 'b) Comb_intf.with_valid2

module Make_with_wave_formats (X : sig
    include Pre

    val wave_formats : Wave_format.t t
  end) : S with type 'a t := 'a X.t = struct
  include X

  type tag = int [@@deriving equal ~localize, compare ~localize]

  let port_names = map port_names_and_widths ~f:fst
  let port_widths = map port_names_and_widths ~f:snd
  let const c = map port_names ~f:(Fn.const c)
  let to_list_rev x = to_list x |> List.rev
  let zip a b = map2 a b ~f:(fun a b -> a, b)
  let zip3 a b c = map2 (zip a b) c ~f:(fun (a, b) c -> a, b, c)
  let zip4 a b c d = map2 (zip a b) (zip c d) ~f:(fun (a, b) (c, d) -> a, b, c, d)

  let zip5 a b c d e =
    map2 (zip3 a b c) (zip d e) ~f:(fun (a, b, c) (d, e) -> a, b, c, d, e)
  ;;

  let map3 a b c ~f = map ~f:(fun (a, b, c) -> f a b c) (zip3 a b c) [@nontail]
  let map4 a b c d ~f = map ~f:(fun (a, b, c, d) -> f a b c d) (zip4 a b c d) [@nontail]

  let map5 a b c d e ~f =
    map ~f:(fun (a, b, c, d, e) -> f a b c d e) (zip5 a b c d e) [@nontail]
  ;;

  let iter3 a b c ~f = ignore @@ map3 ~f a b c
  let iter4 a b c d ~f = ignore @@ map4 ~f a b c d
  let iter5 a b c d e ~f = ignore @@ map5 ~f a b c d e

  let fold a ~init ~f =
    let init = ref init in
    iter a ~f:(fun a -> init := f !init a);
    !init
  ;;

  let fold2 a b ~init ~f = fold (zip a b) ~init ~f:(fun c (a, b) -> f c a b) [@nontail]
  let sum_of_port_widths = fold port_widths ~init:0 ~f:( + )

  let scan t ~init ~f =
    (* slightly contorted implementation which avoids depending on the ordering of map. *)
    let result = map port_names ~f:(fun _ -> ref None) in
    let acc = ref init in
    iter2 t result ~f:(fun t result ->
      let acc', field = f !acc t in
      acc := acc';
      result := Some field);
    map2 port_names result ~f:(fun name x ->
      match !x with
      | Some x -> x
      | None -> raise_s [%message "[Interface.Make.scan] missing result" (name : string)])
  ;;

  let scan2 a b ~init ~f = scan (zip a b) ~init ~f:(fun c (a, b) -> f c a b) [@nontail]
  let tags = scan port_names ~init:0 ~f:(fun acc _ -> acc + 1, acc)
  let to_alist x = to_list (map2 tags x ~f:(fun tag x -> tag, x))
  let field_by_tag t tag = List.Assoc.find (to_alist t) tag ~equal:Int.equal

  let of_alist x =
    map tags ~f:(fun tag ->
      match List.Assoc.find x tag ~equal:Int.equal with
      | Some x -> x
      | None ->
        let missing_field_name = field_by_tag port_names tag in
        raise_s
          [%message
            "[Interface.Make.of_alist] Field not provided"
              (missing_field_name : string option)
              ~interface:(port_widths : int X.t)])
  ;;

  module Unsafe_assoc_by_port_name = struct
    let to_alist x = to_list (map2 port_names x ~f:(fun name x -> name, x))

    let of_alist (x : (string * _) list) =
      (* Assert there are no ports with duplicate names. *)
      if not
           (List.is_empty
              (List.find_all_dups (fst (List.unzip x)) ~compare:String.compare))
      then raise_s [%message "[of_alist] Duplicate tags"];
      map port_names ~f:(fun name ->
        match List.Assoc.find x name ~equal:String.equal with
        | Some x -> x
        | None ->
          raise_s
            [%message
              "[Interface.Make.of_alist] Field not found in interface"
                ~missing_field_name:(name : string)
                ~input:(x : (string * _) list)
                ~interface:(port_widths : int X.t)])
    ;;
  end

  let offsets ?(rev = false) () =
    let rec loop fields ~offset =
      match fields with
      | [] -> []
      | (tag, width) :: fields -> (tag, offset) :: loop fields ~offset:(offset + width)
    in
    loop
      (if rev then to_list_rev (zip tags port_widths) else to_list (zip tags port_widths))
      ~offset:0
    |> of_alist
  ;;

  let of_interface_list ts =
    List.fold
      (List.rev ts)
      ~init:(map port_names ~f:(fun _ -> []))
      ~f:(fun ac t -> map2 t ac ~f:(fun h t -> h :: t))
  ;;

  let to_interface_list t =
    let lengths = map t ~f:(List.length :> _ -> _) in
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
          "[Interface.Make.to_interface_list] field list lengths must be the same"
            (lengths : int t)]
  ;;

  module All (M : Monad.S) = struct
    let all (t : _ M.t t) =
      let%map.M l = M.all (to_list t) in
      of_alist (List.zip_exn (to_list tags) l)
    ;;
  end

  let or_error_all t =
    let open All (Or_error) in
    all t
  ;;

  module Make_comb (Comb : Comb.S) = struct
    type comb = Comb.t [@@deriving sexp_of]
    type t = Comb.t X.t [@@deriving sexp_of]
    type bits_t = Bits.t X.t [@@deriving sexp_of]

    let widths t = map t ~f:Comb.width

    let assert_widths x =
      iter2
        (widths x)
        port_names_and_widths
        ~f:(fun actual_width (port_name, expected_width) ->
          if actual_width <> expected_width
          then
            raise_s
              [%message
                "Port width mismatch in interface"
                  (port_name : string)
                  (expected_width : int)
                  (actual_width : int)])
    ;;

    let ( of_int_trunc
        , of_unsigned_int
        , of_signed_int
        , of_ints_trunc
        , of_unsigned_ints
        , of_signed_ints )
      =
      let map1 f i = map port_widths ~f:(fun width -> f ~width i) in
      let map2 f = map2 port_widths ~f:(fun width -> f ~width) in
      ( map1 Comb.of_int_trunc
      , map1 Comb.of_unsigned_int
      , map1 Comb.of_signed_int
      , map2 Comb.of_int_trunc
      , map2 Comb.of_unsigned_int
      , map2 Comb.of_signed_int )
    ;;

    let zero () = of_unsigned_int 0

    let pack ?(rev = false) t =
      assert_widths t;
      if rev then to_list t |> Comb.concat_msb else to_list_rev t |> Comb.concat_msb
    ;;

    let unpack ?(rev = false) comb =
      if Comb.width comb <> sum_of_port_widths
      then
        raise_s
          [%message
            "Unpack called with an incorrect width"
              ~expected_width:(sum_of_port_widths : int)
              ~actual_width:(Comb.width comb : int)];
      let rec loop fields ~offset =
        match fields with
        | [] -> []
        | (tag, width) :: fields ->
          (tag, Comb.select comb ~high:(offset + width - 1) ~low:offset)
          :: loop fields ~offset:(offset + width)
      in
      loop
        (if rev
         then to_list_rev (zip tags port_widths)
         else to_list (zip tags port_widths))
        ~offset:0
      |> of_alist
    ;;

    let mux s l = map ~f:(Comb.mux s) (of_interface_list l)
    let mux2 s h l = mux s [ l; h ]
    let concat l = map ~f:Comb.concat_msb (of_interface_list l)

    let distribute_valids (ts : (comb, t) with_valid2 list) =
      List.map ts ~f:(fun { valid; value } ->
        map value ~f:(fun value : _ with_valid2 -> { valid; value }))
    ;;

    let collect_valids (t : comb with_valid X.t) : _ with_valid2 =
      { valid =
          (match to_list t with
           | { valid; _ } :: _ -> valid
           | [] -> raise_s [%message "[priority_select] interface has no fields"])
      ; value = map t ~f:(fun { valid = _; value } -> value)
      }
    ;;

    let priority_select ?branching_factor (ts : (comb, t) with_valid2 list)
      : (comb, t) with_valid2
      =
      if List.is_empty ts
      then raise_s [%message "[priority_select] requires at least one input"];
      let ts = distribute_valids ts in
      let t = map (of_interface_list ts) ~f:(Comb.priority_select ?branching_factor) in
      collect_valids t
    ;;

    let priority_select_with_default
      ?branching_factor
      (ts : (comb, t) with_valid2 list)
      ~(default : t)
      =
      if List.is_empty ts
      then raise_s [%message "[priority_select_with_default] requires at least one input"];
      let ts = distribute_valids ts in
      map2 (of_interface_list ts) default ~f:(fun t default ->
        Comb.priority_select_with_default ?branching_factor t ~default)
    ;;

    let onehot_select ?branching_factor (ts : (comb, t) with_valid2 list) =
      if List.is_empty ts
      then raise_s [%message "[onehot_select] requires at least one input"];
      let ts = distribute_valids ts in
      map (of_interface_list ts) ~f:(fun t -> Comb.onehot_select ?branching_factor t)
    ;;

    let validate t =
      let (_ : unit X.t) =
        map3 port_names port_widths t ~f:(fun port_name port_width signal ->
          if Comb.width signal <> port_width
          then (
            let signal_width = Comb.width signal in
            Or_error.error_s
              [%message
                "Interface validation failed!"
                  (port_name : string)
                  (port_width : int)
                  (signal_width : int)])
          else Ok ())
        |> or_error_all
        |> Or_error.ok_exn
      in
      ()
    ;;
  end

  module type Comb = Comb with type 'a interface := 'a t

  module Of_bits = Make_comb (Bits)

  let opt t =
    match t with
    | None -> const None
    | Some t -> map t ~f:(fun t -> Some t)
  ;;

  module Make_signal_functions (Signal : Signal.S) = struct
    include Make_comb (Signal)

    let assign t1 t2 = iter2 t1 t2 ~f:Signal.assign
    let ( <-- ) = assign

    let wires ?(named = false) ?from () =
      let wires =
        match from with
        | None -> map port_widths ~f:Signal.wire
        | Some x -> map x ~f:Signal.wireof
      in
      if named then map2 wires port_names ~f:Signal.( -- ) else wires
    ;;

    let reg ?enable ?initialize_to ?reset_to ?clear ?clear_to spec t =
      map4
        (opt initialize_to)
        (opt reset_to)
        (opt clear_to)
        t
        ~f:(fun initialize_to reset_to clear_to d ->
          Signal.reg ?enable ?initialize_to ?reset_to ?clear ?clear_to spec d)
    ;;

    let cut_through_reg ?initialize_to ?reset_to ?clear ?clear_to spec ~enable t =
      map4
        (opt initialize_to)
        (opt reset_to)
        (opt clear_to)
        t
        ~f:(fun initialize_to reset_to clear_to d ->
          Signal.cut_through_reg ?initialize_to ?reset_to ?clear ?clear_to spec ~enable d)
    ;;

    let pipeline ?attributes ?enable ?initialize_to ?reset_to ?clear ?clear_to spec ~n t =
      map4
        (opt initialize_to)
        (opt reset_to)
        (opt clear_to)
        t
        ~f:(fun initialize_to reset_to clear_to d ->
          Signal.pipeline
            ?attributes
            ?enable
            ?initialize_to
            ?reset_to
            ?clear
            ?clear_to
            ~n
            spec
            d)
    ;;

    let inputs () = wires () ~named:true
    let outputs t = wires () ~from:t ~named:true

    let apply_names ?(prefix = "") ?(suffix = "") ?(naming_op = Signal.( -- )) t =
      map2 t port_names ~f:(fun s n -> naming_op s (prefix ^ n ^ suffix))
    ;;

    let __ppx_auto_name t prefix =
      iter2 t wave_formats ~f:Signal.set_wave_format;
      apply_names ~prefix:(prefix ^ "$") t
    ;;
  end

  module Of_signal = struct
    include Make_signal_functions (Signal)
  end

  module Of_clocked_signal = struct
    include Make_signal_functions (Clocked_signal)

    let wires ?named arg =
      let output =
        let from =
          match arg with
          | `From from -> Some from
          | `Domains _ -> None
        in
        wires ?named ?from ()
      in
      match arg with
      | `Domains doms ->
        map2 doms output ~f:(fun dom signal ->
          Clocked_signal.Unsafe.set_domain signal ~dom)
      | `From _ -> output
    ;;

    let inputs doms = wires ~named:true (`Domains doms)
    let outputs t = wires ~named:true (`From t)

    let apply_names ?prefix ?suffix ?naming_op t =
      Of_signal.apply_names
        ?prefix
        ?suffix
        ?naming_op
        (X.map t ~f:(fun t ->
           Clocked_signal.unwrap_signal ~dom:(Clocked_signal.get_domain t) t))
      |> map2 t ~f:(fun t s ->
        Clocked_signal.to_clocked ~dom:(Clocked_signal.get_domain t) s)
    ;;
  end

  module Names_and_widths = struct
    let port_names_and_widths = to_list port_names_and_widths
    let port_names = to_list port_names
    let port_widths = to_list port_widths
    let tags = to_list tags
  end

  module Of_always = struct
    let assign dst src = map2 dst src ~f:Always.( <-- ) |> to_list |> Always.proc
    let ( <-- ) = assign
    let value t = map t ~f:(fun a -> a.Always.Variable.value)

    let reg ?enable ?initialize_to ?reset_to ?clear ?clear_to spec =
      map4
        (opt initialize_to)
        (opt reset_to)
        (opt clear_to)
        port_widths
        ~f:(fun initialize_to reset_to clear_to width ->
          Always.Variable.reg
            spec
            ?enable
            ?initialize_to
            ?reset_to
            ?clear
            ?clear_to
            ~width)
    ;;

    let cut_through_reg ?enable ?initialize_to ?reset_to ?clear ?clear_to spec =
      map4
        (opt initialize_to)
        (opt reset_to)
        (opt clear_to)
        port_widths
        ~f:(fun initialize_to reset_to clear_to width ->
          Always.Variable.cut_through_reg
            spec
            ?enable
            ?initialize_to
            ?reset_to
            ?clear
            ?clear_to
            ~width)
    ;;

    let wire f =
      map port_widths ~f:(fun width -> Always.Variable.wire ~default:(f width) ())
    ;;

    let apply_names ?prefix ?suffix ?naming_op t =
      ignore (Of_signal.apply_names ?prefix ?suffix ?naming_op (value t) : Signal.t t)
    ;;

    let __ppx_auto_name t prefix =
      iter2 (value t) wave_formats ~f:Signal.set_wave_format;
      apply_names ~prefix:(prefix ^ "$") t;
      t
    ;;
  end
end
[@@inline never]

module Make (X : Pre) : S with type 'a t := 'a X.t = Make_with_wave_formats (struct
    include X

    let wave_formats = X.map port_names_and_widths ~f:(Fn.const Wave_format.default)
  end)
[@@inline never]

module Update
    (Pre : Interface_intf.Pre)
    (M : sig
       val port_names_and_widths : (string * int) Pre.t
     end) =
struct
  module T = struct
    include Pre

    let port_names_and_widths = M.port_names_and_widths
  end

  include (T : Interface_intf.Pre with type 'a t = 'a T.t)
  include Make (T)
end

module Empty = struct
  type 'a t = Empty [@@deriving equal ~localize, compare ~localize, sexp_of]

  include Make (struct
      type nonrec 'a t = 'a t [@@deriving equal ~localize, compare ~localize, sexp_of]

      let port_names_and_widths = Empty
      let iter _ ~f:_ = ()
      let iter2 _ _ ~f:_ = ()
      let map _ ~f:_ = Empty
      let map2 _ _ ~f:_ = Empty
      let to_list _ = []
    end)
end

module Make_interface_with_conversion
    (Repr : S)
    (M : sig
       type 'a t [@@deriving equal ~localize, compare ~localize, sexp_of]

       val t_of_repr : 'a Repr.t -> 'a t
       val repr_of_t : 'a t -> 'a Repr.t
     end) =
struct
  type 'a t = 'a M.t [@@deriving equal ~localize, compare ~localize, sexp_of]

  include Make (struct
      type nonrec 'a t = 'a t [@@deriving equal ~localize, compare ~localize, sexp_of]

      let port_names_and_widths = M.t_of_repr Repr.port_names_and_widths
      let map t ~f = M.t_of_repr (Repr.map (M.repr_of_t t) ~f)
      let map2 a b ~f = M.t_of_repr (Repr.map2 (M.repr_of_t a) (M.repr_of_t b) ~f)
      let iter t ~f = Repr.iter (M.repr_of_t t) ~f
      let iter2 a b ~f = Repr.iter2 (M.repr_of_t a) (M.repr_of_t b) ~f
      let to_list t = Repr.to_list (M.repr_of_t t)
    end)
end

module type S_with_ast = sig
  include S

  val ast : Ast.t
end

module type S_with_clock_domains = sig
  include S

  val domains : Clock_domain.t t
end
