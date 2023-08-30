open Base
module Ast = Interface.Ast

type ('a, 'b) with_valid2 = ('a, 'b) Comb_intf.with_valid2

module type S_enum = Enum_intf.S_enum
module type S_enums = Enum_intf.S_enums

module Make_enums (Cases : Enum_intf.Cases) = struct
  module Cases = struct
    include Cases
    include Comparable.Make (Cases)
  end

  let to_rank =
    let mapping =
      List.mapi Cases.all ~f:(fun i x -> x, i) |> Map.of_alist_exn (module Cases)
    in
    fun x -> Map.find_exn mapping x
  ;;

  module Make_pre (M : sig
    val how : [ `Binary | `One_hot ]
  end) =
  struct
    let port_name, width =
      match M.how with
      | `Binary -> "binary_variant", Int.ceil_log2 (List.length Cases.all)
      | `One_hot -> "ont_hot_variant", List.length Cases.all
    ;;

    type 'a t = 'a [@@deriving sexp_of]

    let to_list t = [ t ]
    let map t ~f = f t
    let map2 a b ~f = f a b
    let iter a ~f = f a
    let iter2 a b ~f = f a b
    let port_names_and_widths = port_name, width

    let ast : Ast.t =
      [ { Ast.Field.name = port_name
        ; type_ = Signal { bits = width; rtlname = port_name }
        ; sequence = None
        ; doc = None
        }
      ]
    ;;

    let[@inline always] to_raw t = t

    let of_raw (type a) (module Comb : Comb.S with type t = a) (t : a) =
      if Comb.width t <> width
      then
        failwith
          [%string
            "Width mismatch. Enum expects %{width#Int}, but obtained %{Comb.width t#Int}"];
      t
    ;;

    let[@inline always] unwrap t = t

    module Unsafe = struct
      let wrap t = t
    end
  end

  module Make_interface (M : sig
    val how : [ `Binary | `One_hot ]

    val match_
      :  (module Comb.S with type t = 'a)
      -> ?default:'a
      -> 'a
      -> (Cases.t * 'a) list
      -> 'a
  end) =
  struct
    module Pre = Make_pre (M)
    include Pre
    include Interface.Make (Pre)

    let to_int_repr enum =
      match M.how with
      | `Binary -> to_rank enum
      | `One_hot -> 1 lsl to_rank enum
    ;;

    let of_enum (type a) (module Comb : Comb.S with type t = a) enum =
      Comb.of_int ~width (to_int_repr enum)
    ;;

    let to_enum =
      List.map Cases.all ~f:(fun variant -> to_int_repr variant, variant)
      |> Map.of_alist_exn (module Int)
    ;;

    let to_enum t =
      let x = Bits.to_int t in
      match Map.find to_enum x with
      | Some x -> Ok x
      | None ->
        Or_error.error_string
          (Printf.sprintf
             "Failed to convert bits %d back to an enum. Is it an undefined value?"
             x)
    ;;

    let to_enum_exn t = Or_error.ok_exn (to_enum t)
    let match_ = M.match_
    let ( ==: ) (type a) (module Comb : Comb.S with type t = a) = Comb.( ==: )

    module Of_signal = struct
      include Of_signal

      let ( ==: ) = ( ==: ) (module Signal)
      let of_enum = of_enum (module Signal)
      let of_raw = of_raw (module Signal)
      let match_ = match_ (module Signal)
      let is lhs rhs = lhs ==: of_enum rhs
    end

    module Of_bits = struct
      include Of_bits

      let ( ==: ) = ( ==: ) (module Bits)
      let of_enum = of_enum (module Bits)
      let of_raw = of_raw (module Bits)
      let match_ = match_ (module Bits)
      let is lhs rhs = lhs ==: of_enum rhs
    end

    module Make_comb (X : Comb.S) = struct
      include Make_comb (X)

      let ( ==: ) = ( ==: ) (module X)
      let of_enum = of_enum (module X)
      let of_raw = of_raw (module X)
      let match_ = match_ (module X)
      let is lhs rhs = lhs ==: of_enum rhs
    end

    module Of_always = struct
      include Of_always

      let all_cases = Set.of_list (module Cases) Cases.all

      let check_for_unhandled_cases cases =
        let handled_cases =
          List.fold
            cases
            ~init:(Set.empty (module Cases))
            ~f:(fun handled (case, _) ->
              if Set.mem handled case
              then raise_s [%message "Case specified multiple times!" (case : Cases.t)];
              Set.add handled case)
        in
        Set.diff all_cases handled_cases
      ;;

      let match_ ?default sel cases =
        let unhandled_cases = check_for_unhandled_cases cases in
        let default_cases =
          if Set.is_empty unhandled_cases
          then []
          else (
            match default with
            | None -> raise_s [%message "[default] not specified on non exhaustive cases"]
            | Some default ->
              List.map (Set.to_list unhandled_cases) ~f:(fun case -> case, default))
        in
        let cases =
          List.map (cases @ default_cases) ~f:(fun (case, x) -> Of_signal.of_enum case, x)
        in
        Always.switch (to_raw sel) cases
      ;;
    end

    (* Testbench functions. *)
    let sim_set t enum = t := of_enum (module Bits) enum
    let sim_set_raw t raw = t := raw
    let sim_get t = to_enum !t
    let sim_get_exn t = Or_error.ok_exn (sim_get t)
    let sim_get_raw t = !t
  end

  let num_enums = List.length Cases.all

  let raise_non_exhaustive_mux () =
    failwith "[mux] on enum cases not exhaustive, and [default] not provided"
  ;;

  module Binary = Make_interface (struct
    let how = `Binary

    let match_
      (type a)
      (module Comb : Comb.S with type t = a)
      ?(default : a option)
      selector
      cases
      =
      let out_cases = Array.create ~len:num_enums default in
      List.iter cases ~f:(fun (enum, value) -> out_cases.(to_rank enum) <- Some value);
      let cases =
        List.map (Array.to_list out_cases) ~f:(function
          | None -> raise_non_exhaustive_mux ()
          | Some case -> case)
      in
      Comb.mux selector cases
    ;;
  end)

  module One_hot = Make_interface (struct
    let how = `One_hot

    let match_
      (type a)
      (module Comb : Comb.S with type t = a)
      ?(default : a option)
      selector
      cases
      =
      let out_cases = Array.create ~len:num_enums default in
      List.iter cases ~f:(fun (enum, value) -> out_cases.(to_rank enum) <- Some value);
      let cases =
        List.map (Array.to_list out_cases) ~f:(function
          | None -> raise_non_exhaustive_mux ()
          | Some case -> case)
      in
      List.map2_exn (Comb.bits_lsb selector) cases ~f:(fun valid value : _ with_valid2 ->
        { valid; value })
      |> Comb.onehot_select
    ;;
  end)
end

module Make_binary (Cases : Enum_intf.Cases) = struct
  module X = Make_enums (Cases)
  include X.Binary

  let[@inline always] unwrap t = t
end
[@@inline always]

module Make_one_hot (Cases : Enum_intf.Cases) = struct
  module X = Make_enums (Cases)
  include X.One_hot

  let[@inline always] unwrap t = t
end
[@@inline always]
