open! Core0

module State = struct
  module Named = struct
    module T = struct
      type t =
        { name : string
        ; value : int
        }
      [@@deriving bin_io, equal, sexp_of, compare, hash]
    end

    include Comparator.Make (T)
    include T
  end

  type t =
    { name : string option
    ; value : int
    }
  [@@deriving sexp_of, equal]

  let of_named { Named.name; value } = { name = Some name; value }
end

module Case = struct
  module Positional = struct
    module T = struct
      type t =
        | Specified of int
        | Default
      [@@deriving equal, sexp_of, compare, hash]
    end

    include Comparator.Make (T)
    include T
  end

  module Positional_with_state = struct
    type t =
      | Specified of
          { position : int
          ; state : State.Named.t option
          }
      | Default of { states : State.Named.t list }
    [@@deriving bin_io, sexp_of]

    let to_string = function
      | Specified { state = Some state; _ } -> state.name
      | Specified { position; _ } -> Int.to_string position
      | Default { states } ->
        let names = List.map states ~f:(fun { name; _ } -> name) in
        (match names with
         | [] -> "default"
         | [ name ] -> name
         | names -> "(" ^ String.concat names ~sep:" | " ^ ")")
    ;;

    let to_positional = function
      | Default _ -> Positional.Default
      | Specified { position; _ } -> Specified position
    ;;

    let of_positional = function
      | Positional.Default -> Default { states = [] }
      | Specified position -> Specified { position; state = None }
    ;;
  end

  type t =
    | Positional of int
    | State of State.Named.t
    | Default
  [@@deriving bin_io, sexp_of, equal]

  let to_string = function
    | Positional position -> Int.to_string position
    | State state -> state.name
    | Default -> "default"
  ;;
end

module Cases = struct
  type t = Case.Positional_with_state.t list [@@deriving bin_io, sexp_of]

  let create ~non_default_cases ~default_states =
    Case.Positional_with_state.Default { states = default_states }
    :: List.mapi non_default_cases ~f:(fun position a ->
      match a with
      | `Positional -> Case.Positional_with_state.Specified { position; state = None }
      | `State state -> Specified { position; state = Some state })
  ;;

  let create_from_count ~non_default_case_count =
    Case.Positional_with_state.Default { states = [] }
    :: List.init non_default_case_count ~f:(fun position ->
      Case.Positional_with_state.Specified { position; state = None })
  ;;

  let to_case_list t = t
  let length t = List.length t
end

module Toggle = struct
  module T = struct
    type t =
      { bit : int
      ; on : bool
      }
    [@@deriving bin_io, sexp_of, compare, hash, equal]
  end

  include Comparator.Make (T)
  include T

  let to_string { bit; on } =
    let toggle = if on then "on" else "off" in
    [%string {|bit-%{bit#Int} %{toggle}|}]
  ;;

  let all_bits_up_to_excl up_to =
    let open List.Let_syntax in
    let%bind bit = List.init up_to ~f:Fn.id in
    let%map on = Bool.all in
    { bit; on }
  ;;
end

module Transition = struct
  type 'a t =
    { from : 'a
    ; to_ : 'a
    }
  [@@deriving bin_io, sexp_of, compare, equal, hash]

  let to_string t ~f = [%string {|%{f t.from} -> %{f t.to_}|}]
  let map t ~f = { from = f t.from; to_ = f t.to_ }

  module Value = struct
    module T = struct
      type nonrec t = int t [@@deriving sexp_of, compare, equal, hash]
    end

    include Comparator.Make (T)
    include T
  end

  module State = struct
    type nonrec t = State.t t [@@deriving sexp_of, equal]

    let to_string (t : t) =
      to_string t ~f:(fun (state : State.t) ->
        match state.name with
        | Some name -> name
        | None -> [%string {|"!unknown_state with value %{state.value#Int}"|}])
    ;;
  end
end

module Waiver = struct
  type 'a kind =
    | Only_expect of 'a list
    | Exclude of 'a list
    | None
  [@@deriving bin_io, sexp_of]

  type 'a t =
    { kind : 'a kind
    ; warn_on_waived_but_observed : bool
    }
  [@@deriving bin_io, sexp_of, fields ~getters]

  let create ?(warn_on_waived_but_observed = true) kind =
    { kind; warn_on_waived_but_observed }
  ;;

  let none () = create None ~warn_on_waived_but_observed:false

  let only_expect ?warn_on_waived_but_observed ls =
    create ?warn_on_waived_but_observed (Only_expect ls)
  ;;

  let exclude ?warn_on_waived_but_observed ls =
    create ?warn_on_waived_but_observed (Exclude ls)
  ;;

  let to_string t ~f =
    match t.kind with
    | Only_expect only -> [%string {|only %{List.to_string only ~f}|}]
    | Exclude exclude -> [%string {|exclude %{List.to_string exclude ~f}|}]
    | None -> "none"
  ;;

  let is_none { kind; _ } =
    match kind with
    | None -> true
    | _ -> false
  ;;

  let map' t ~f =
    let kind =
      match t.kind with
      | None -> None
      | Exclude ls -> Exclude (f ls)
      | Only_expect ls -> Only_expect (f ls)
    in
    { t with kind }
  ;;

  let map t ~f = map' t ~f:(List.map ~f)
  let concat_map t ~f = map' t ~f:(List.concat_map ~f)
  let filter_map t ~f = map' t ~f:(List.filter_map ~f)

  let waived_list t ~all ~equal =
    match t.kind with
    | None -> []
    | Exclude waived -> waived
    | Only_expect keep -> List.filter all ~f:(fun a -> not (List.mem keep a ~equal))
  ;;

  let join_kind a b ~equal =
    match a, b with
    | None, b -> b
    | a, None -> a
    | Only_expect a, Only_expect b -> Only_expect (a @ b)
    | Exclude a, Exclude b -> Exclude (a @ b)
    | Only_expect only, Exclude exclude | Exclude exclude, Only_expect only ->
      Only_expect (List.filter only ~f:(fun x -> not (List.mem exclude x ~equal)))
  ;;

  let join a b ~equal =
    let kind = join_kind a.kind b.kind ~equal in
    let warn_on_waived_but_observed =
      a.warn_on_waived_but_observed || b.warn_on_waived_but_observed
    in
    { kind; warn_on_waived_but_observed }
  ;;

  let join_multi ts ~equal =
    List.fold ts ~init:(none ()) ~f:(fun acc t -> join acc t ~equal)
  ;;
end

module Always_metadata = struct
  module Variable = struct
    module State_reg = struct
      type t =
        { creation_pos : Source_code_position.t
        ; states_by_value : State.Named.t Int.Map.t
        ; transitions_by_value : Int.Hash_set.t Int.Table.t
        ; initial_value : int
        }
      [@@deriving bin_io, sexp_of]
    end

    type t =
      | User_created of Source_code_position.t
      | State_machine_state of State_reg.t
    [@@deriving bin_io, sexp_of]
  end

  module Target = struct
    type t =
      { variable : Variable.t
      ; names : Name_and_loc.t list
      }
    [@@deriving bin_io, sexp_of]
  end

  module If = struct
    module Kind = struct
      type t =
        | If
        | Elif
        | When
        | Unless
        | Proc
      [@@deriving bin_io, sexp_of, to_string ~capitalize:"snake_case"]
    end

    type t =
      { creation_pos : Source_code_position.t
      ; target : Target.t
      ; kind : Kind.t
      }
    [@@deriving bin_io, sexp_of]
  end

  module Switch_cases = struct
    type t =
      { creation_pos : Source_code_position.t
      ; target : Target.t
      ; cases : Cases.t
      }
    [@@deriving bin_io, sexp_of]
  end

  module Switch_mux = struct
    type t =
      { creation_pos : Source_code_position.t
      ; target : Target.t
      ; case : Case.t
      }
    [@@deriving bin_io, sexp_of]
  end
end
