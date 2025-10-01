open! Core0
open Coverage_prim

type 'a coverage =
  { observed : 'a list
  ; unexpectedly_observed : 'a list
  ; not_covered : 'a list
  }

let map (coverage : _ coverage) ~f =
  let%tydi { observed; unexpectedly_observed; not_covered } = coverage in
  { observed = f observed
  ; unexpectedly_observed = f unexpectedly_observed
  ; not_covered = f not_covered
  }
;;

let map_coverage coverage ~f = map coverage ~f:(List.map ~f)
let filter_coverage coverage ~f = map coverage ~f:(List.filter ~f)

module type S = sig
  include Coverage.S

  type choice
  type coverage_unit

  val mark_choice : t -> choice -> unit
  val coverage : t -> coverage_unit coverage
  val id : t -> Signal_graph.Normalized_signal_uid.t
  val signal_names : t -> Name_and_loc.t list
  val call_stack : t -> Call_stack.t
end

module Covered_cases (Choice : sig
    type t [@@deriving sexp_of, equal, compare, hash]

    include Comparator.S with type t := t
  end) =
struct
  type choice = Choice.t

  type t =
    { observed : Choice.t Hash_set.t
    ; expected : Set.M(Choice).t
    ; ignored : Set.M(Choice).t
        (* Set of choices to never show in the [unexpectedly_observed] set. If any choice
           also shows up in the expected set, that will take precedence. *)
    ; unexpectedly_observed : Choice.t Hash_set.t
    }
  [@@deriving sexp_of]

  let create ?(ignored_setup = Set.empty (module Choice)) all waiver =
    let waived_list = Waiver.waived_list waiver ~all ~equal:Choice.equal in
    let expected =
      List.fold
        waived_list
        ~init:(Set.of_list (module Choice) all)
        ~f:(fun expected to_waive -> Set.remove expected to_waive)
    in
    let ignored =
      if Waiver.warn_on_waived_but_observed waiver
      then ignored_setup
      else
        List.fold waived_list ~init:ignored_setup ~f:(fun ignored waived ->
          Set.add ignored waived)
    in
    { observed = Hash_set.create (module Choice)
    ; expected
    ; ignored
    ; unexpectedly_observed = Hash_set.create (module Choice)
    }
  ;;

  let total_cases t = Set.length t.expected
  let covered_cases t = Hash_set.length t.observed
  let unexpectedly_observed_cases t = Hash_set.length t.unexpectedly_observed

  let mark_choice t a =
    if Set.mem t.expected a
    then Hash_set.add t.observed a
    else if Set.mem t.ignored a
    then ()
    else Hash_set.add t.unexpectedly_observed a
  ;;

  let coverage t =
    let observed, not_covered =
      Set.to_list t.expected |> List.partition_tf ~f:(fun c -> Hash_set.mem t.observed c)
    in
    { observed
    ; not_covered
    ; unexpectedly_observed = Hash_set.to_list t.unexpectedly_observed
    }
  ;;
end

module Make_coverage (M : sig
    module Covered_cases : sig
      type t
      type choice

      val mark_choice : t -> choice -> unit
      val coverage : t -> choice coverage
      val total_cases : t -> int
      val covered_cases : t -> int
      val unexpectedly_observed_cases : t -> int
    end

    type t

    val covered_cases : t -> Covered_cases.t
  end) =
struct
  open M

  type choice = M.Covered_cases.choice

  include Coverage.Make (struct
      include M

      let total_cases t = Covered_cases.total_cases (covered_cases t)

      let unexpectedly_observed_cases t =
        Covered_cases.unexpectedly_observed_cases (covered_cases t)
      ;;

      let covered_cases t = Covered_cases.covered_cases (covered_cases t)
    end)

  let coverage t = Covered_cases.coverage (covered_cases t)
  let mark_choice t = Covered_cases.mark_choice (covered_cases t)
end

module Elide_in_tests : sig
  type 'a t = 'a [@@deriving sexp_of]
end = struct
  type 'a t = 'a

  let sexp_of_t sexp_of_a t =
    if (* controlled by environment variable TESTING_FRAMEWORK *)
       am_running_test
    then sexp_of_string "<elided-in-tests>"
    else sexp_of_a t
  ;;
end

let filter_call_stack call_stack =
  let skipped_bottom_modules = [ "cyclesim.ml"; "circuit.ml" ] in
  let is_test_file filename =
    String.substr_index filename ~pattern:"test" |> Option.is_some
  in
  List.take_while call_stack ~f:(fun slot ->
    match slot with
    | None -> false
    | Some loc ->
      (not
         (List.mem
            ~equal:String.equal
            skipped_bottom_modules
            loc.Call_stack.Slot.filename))
      && not (is_test_file loc.filename))
;;

let call_stack_of_signal signal =
  let call_stack = Caller_id.call_stack_opt (Signal.Type.caller_id signal) in
  filter_call_stack call_stack
;;

let raise_unexpected_kind ~signal_kind metadata =
  raise_s
    [%message
      "Unexpected coverage metadata kind for"
        ~signal_kind
        (metadata : Coverage_metadata.Kind.t Elide_in_tests.t)]
;;

module Mux = struct
  module Debug_info = struct
    module Kind = struct
      type t =
        | Basic
        | If of Always_metadata.If.t
        | Switch of Always_metadata.Switch_mux.t
      [@@deriving sexp_of]
    end

    type t =
      { select_names : Name_and_loc.t list
      ; kind : Kind.t
      }
    [@@deriving sexp_of]
  end

  module T = struct
    module Covered_cases = Covered_cases (Int)

    type coverage_unit = int

    type t =
      { case_count : int
      ; covered_cases : Covered_cases.t
      ; id : Signal_graph.Normalized_signal_uid.t
      ; signal_names : Name_and_loc.t list
      ; call_stack : Call_stack.t
      ; debug_info : Debug_info.t
      }
    [@@deriving sexp_of, fields ~getters]
  end

  include T
  include Make_coverage (T)

  let signal_kind = "mux"

  let parse_kind signal =
    match Signal.Type.coverage_metadata signal with
    | None | Some { kind = None; _ } -> Debug_info.Kind.Basic
    | Some { kind = Some (If if_); _ } -> If if_
    | Some { kind = Some (Switch_mux switch); _ } -> Switch switch
    | Some { kind = Some kind; _ } -> raise_unexpected_kind ~signal_kind kind
  ;;

  let parse_waiver signal =
    match Signal.Type.coverage_metadata signal with
    | Some { waiver = Some (Mux waiver); _ } -> waiver
    | _ -> Waiver.none ()
  ;;

  let create id signal ~select ~case_count =
    let kind = parse_kind signal in
    let waiver = parse_waiver signal in
    let all = List.init case_count ~f:Fn.id in
    { case_count
    ; covered_cases = Covered_cases.create all waiver
    ; id
    ; signal_names = Signal.names_and_locs signal
    ; call_stack = call_stack_of_signal signal
    ; debug_info = { select_names = Signal.names_and_locs select; kind }
    }
  ;;

  let mark_choice t choice =
    let choice =
      (* If the index overflows, repeat the last case *)
      min choice (max 0 (t.case_count - 1))
    in
    Covered_cases.mark_choice t.covered_cases choice
  ;;
end

module Cases = struct
  module Debug_info = struct
    module Kind = struct
      type t =
        | Basic
        | Switch of Always_metadata.Switch_cases.t
      [@@deriving sexp_of]
    end

    type t =
      { select_names : Name_and_loc.t list
      ; kind : Kind.t
      }
    [@@deriving sexp_of]
  end

  module T = struct
    module Covered_cases = Covered_cases (Case.Positional)

    type coverage_unit = Case.Positional_with_state.t

    type t =
      { covered_cases : Covered_cases.t
      ; case_mapping : Case.Positional_with_state.t Map.M(Case.Positional).t
      ; case_count : int
      ; id : Signal_graph.Normalized_signal_uid.t
      ; signal_names : Name_and_loc.t list
      ; call_stack : Call_stack.t
      ; debug_info : Debug_info.t
      }
    [@@deriving sexp_of, fields ~getters]
  end

  include T
  include Make_coverage (T)

  let signal_kind = "cases"

  let parse_kind signal ~non_default_case_count =
    let default_cases = Cases.create_from_count ~non_default_case_count in
    match Signal.Type.coverage_metadata signal with
    | None | Some { kind = None; _ } -> Debug_info.Kind.Basic, default_cases
    | Some { kind = Some (Switch_cases switch); _ } ->
      let%tydi { cases; _ } = switch in
      assert (non_default_case_count + 1 = Cases.length cases);
      Switch switch, cases
    | Some { kind = Some kind; _ } ->
      raise_unexpected_kind ~signal_kind kind, default_cases
  ;;

  let convert_case_waiver (waiver : Case.t Waiver.t) state_to_position debug_info =
    Waiver.map waiver ~f:(function
      | Default -> Case.Positional.Default
      | Positional position -> Specified position
      | State waived_state ->
        (match Map.find state_to_position waived_state.name with
         | Some position -> Case.Positional.Specified position
         | None ->
           raise_s
             [%message
               "Waiver for unknown state case"
                 ~signal:(debug_info : Debug_info.t Elide_in_tests.t)
                 (waived_state.name : string)
                 ~states:(Map.keys state_to_position : string list)]))
  ;;

  let convert_state_waiver waiver state_to_position =
    Waiver.filter_map waiver ~f:(fun w ->
      let%map.Option position = Map.find state_to_position w in
      Case.Positional.Specified position)
  ;;

  let parse_waiver
    ~signal
    ~select
    (cases : Case.Positional_with_state.t list)
    ~non_default_case_count
    debug_info
    =
    let state_to_position =
      List.filter_map cases ~f:(function
        | Specified { state = Some state; position } -> Some (state.name, position)
        | _ -> None)
      |> Map.of_alist_exn (module String)
    in
    let default_waiver =
      let waiver =
        match Signal.Type.coverage_metadata select with
        | Some { kind = Some (Variable (State_machine_state { states_by_value; _ })); _ }
          when Map.length states_by_value = non_default_case_count ->
          Waiver.exclude [ Case.Default ]
        | _ -> Waiver.none ()
      in
      convert_case_waiver waiver state_to_position debug_info
    in
    let select_waiver =
      let waiver =
        match Signal.Type.coverage_metadata select with
        | Some { waiver = Some (Always_state { state; _ }); _ } -> state
        | _ -> Waiver.none ()
      in
      convert_state_waiver waiver state_to_position
    in
    let cases_waiver =
      let waiver =
        match Signal.Type.coverage_metadata signal with
        | Some { waiver = Some (Cases waiver); _ } -> waiver
        | _ -> Waiver.none ()
      in
      convert_case_waiver waiver state_to_position debug_info
    in
    Waiver.join_multi
      ~equal:Case.Positional.equal
      [ default_waiver; select_waiver; cases_waiver ]
  ;;

  let create id signal ~select ~non_default_case_count =
    let kind, cases = parse_kind signal ~non_default_case_count in
    let cases = Cases.to_case_list cases in
    let debug_info = { Debug_info.select_names = Signal.names_and_locs select; kind } in
    let waiver = parse_waiver ~signal ~select cases ~non_default_case_count debug_info in
    let all = List.map cases ~f:Case.Positional_with_state.to_positional in
    let case_mapping =
      List.map cases ~f:(fun case -> Case.Positional_with_state.to_positional case, case)
      |> Map.of_alist_exn (module Case.Positional)
    in
    { covered_cases = Covered_cases.create all waiver
    ; case_mapping
    ; case_count = List.length cases
    ; id
    ; signal_names = Signal.names_and_locs signal
    ; call_stack = call_stack_of_signal signal
    ; debug_info
    }
  ;;

  let map_case t case =
    match Map.find t.case_mapping case with
    | Some case -> case
    | None -> Case.Positional_with_state.of_positional case
  ;;

  let coverage t = coverage t |> map_coverage ~f:(map_case t)
end

module Reg = struct
  module T = struct
    module Covered_cases = Covered_cases (Toggle)

    type coverage_unit = Toggle.t

    type t =
      { covered_cases : Covered_cases.t
      ; bits : int
      ; id : Signal_graph.Normalized_signal_uid.t
      ; signal_names : Name_and_loc.t list
      ; call_stack : Call_stack.t
      }
    [@@deriving sexp_of, fields ~getters]
  end

  include T
  include Make_coverage (T)

  let parse_waiver signal =
    match Signal.Type.coverage_metadata signal with
    | Some { waiver = Some (Reg waiver); _ } -> waiver
    | _ -> Waiver.none ()
  ;;

  let create id signal ~bits =
    let waiver = parse_waiver signal in
    let all =
      List.concat_map Bool.all ~f:(fun on ->
        List.init bits ~f:(fun bit -> { Toggle.bit; on }))
    in
    { covered_cases = Covered_cases.create all waiver
    ; bits
    ; id
    ; signal_names = Signal.names_and_locs signal
    ; call_stack = call_stack_of_signal signal
    }
  ;;
end

module Always_state = struct
  module T = struct
    module Covered_cases = Covered_cases (Transition.Value)

    type coverage_unit = Transition.State.t

    type t =
      { covered_cases : Covered_cases.t
      ; states_by_value : State.Named.t Map.M(Int).t
      ; id : Signal_graph.Normalized_signal_uid.t
      ; signal_names : Name_and_loc.t list
      ; call_stack : Call_stack.t
      }
    [@@deriving sexp_of, fields ~getters]
  end

  include T
  include Make_coverage (T)

  let find_state_exn state values_by_name call_stack signal_names =
    match Map.find values_by_name state with
    | Some value -> value
    | None ->
      raise_s
        [%message
          "Waiver for unknown state case"
            (call_stack : Call_stack.t Elide_in_tests.t)
            (signal_names : Name_and_loc.t list Elide_in_tests.t)
            (state : string)
            ~states:(Map.keys values_by_name : string list)]
  ;;

  let convert_state_waiver waiver all_values values_by_name call_stack signal_names =
    Waiver.concat_map waiver ~f:(fun state ->
      let value = find_state_exn state values_by_name call_stack signal_names in
      List.concat_map all_values ~f:(fun other ->
        [ { Transition.to_ = value; from = other }; { to_ = other; from = value } ]))
  ;;

  let convert_transition_waiver waiver values_by_name call_stack signal_names =
    Waiver.map waiver ~f:(fun { Transition.to_; from } ->
      let to_ = find_state_exn to_ values_by_name call_stack signal_names in
      let from = find_state_exn from values_by_name call_stack signal_names in
      { Transition.to_; from })
  ;;

  let parse_waiver signal states_by_value call_stack signal_names =
    let all_values = Map.keys states_by_value in
    let values_by_name =
      Map.to_alist states_by_value
      |> List.map ~f:(fun (value, state) -> state.State.Named.name, value)
      |> Map.of_alist_exn (module String)
    in
    match Signal.Type.coverage_metadata signal with
    | Some { waiver = Some (Always_state { state; transition }); _ } ->
      let state_waiver =
        convert_state_waiver state all_values values_by_name call_stack signal_names
      in
      let transition_waiver =
        convert_transition_waiver transition values_by_name call_stack signal_names
      in
      Waiver.join state_waiver transition_waiver ~equal:(Transition.equal Int.equal)
    | _ -> Waiver.none ()
  ;;

  let create id signal (metadata : Always_metadata.Variable.State_reg.t) =
    let%tydi { states_by_value; transitions_by_value; initial_value; _ } = metadata in
    let signal_names = Signal.names_and_locs signal in
    let call_stack = call_stack_of_signal signal in
    let waiver = parse_waiver signal states_by_value call_stack signal_names in
    let ignored_setup =
      (* At the beginning of a Cyclesim run, state registers can go from 0->0 and/or
         0->initial state, don't ever report these as unexpectedly observed. *)
      [ { Transition.from = 0; to_ = 0 }; { from = 0; to_ = initial_value } ]
      |> Set.of_list (module Transition.Value)
    in
    let all =
      Hashtbl.to_alist transitions_by_value
      |> List.concat_map ~f:(fun (from, to_set) ->
        Hash_set.to_list to_set |> List.map ~f:(fun to_ -> { Transition.from; to_ }))
    in
    { covered_cases = Covered_cases.create all waiver ~ignored_setup
    ; states_by_value
    ; id
    ; signal_names
    ; call_stack
    }
  ;;

  let map_transition t transition =
    Transition.map transition ~f:(fun value ->
      let name =
        let%map.Option { name; _ } = Map.find t.states_by_value value in
        name
      in
      { State.name; value })
  ;;

  let coverage t = coverage t |> map_coverage ~f:(map_transition t)
end

type t =
  | Mux of Mux.t
  | Cases of Cases.t
  | Reg of Reg.t
  | Always_state of Always_state.t
[@@deriving sexp_of]

let fully_covered = function
  | Mux mux -> Mux.fully_covered mux
  | Cases cases -> Cases.fully_covered cases
  | Reg reg -> Reg.fully_covered reg
  | Always_state always_state -> Always_state.fully_covered always_state
;;

let unexpectedly_observed_cases = function
  | Mux mux -> Mux.unexpectedly_observed_cases mux
  | Cases cases -> Cases.unexpectedly_observed_cases cases
  | Reg reg -> Reg.unexpectedly_observed_cases reg
  | Always_state always_state -> Always_state.unexpectedly_observed_cases always_state
;;

let call_stack = function
  | Mux mux -> Mux.call_stack mux
  | Cases cases -> Cases.call_stack cases
  | Reg reg -> Reg.call_stack reg
  | Always_state always_state -> Always_state.call_stack always_state
;;

let maybe_create id (signal : Signal.t) =
  match signal with
  | Mux { select; cases; _ } ->
    Some (Mux (Mux.create id signal ~select ~case_count:(List.length cases)))
  | Cases { select; cases; _ } ->
    Some
      (Cases (Cases.create id signal ~select ~non_default_case_count:(List.length cases)))
  | Reg _ ->
    (match Signal.Type.coverage_metadata signal with
     | Some { kind = Some (Variable (State_machine_state metadata)); _ } ->
       Some (Always_state (Always_state.create id signal metadata))
     | _ -> Some (Reg (Reg.create id signal ~bits:(Signal.width signal))))
  | _ -> None
;;

module Grouped = struct
  type flat = t list

  type t =
    { muxes : Mux.t list
    ; cases : Cases.t list
    ; regs : Reg.t list
    ; always_states : Always_state.t list
    }
  [@@deriving sexp_of]

  let of_flat (flat : flat) =
    let t = { muxes = []; cases = []; regs = []; always_states = [] } in
    List.fold ~init:t flat ~f:(fun t a ->
      match a with
      | Mux mux -> { t with muxes = mux :: t.muxes }
      | Cases cases -> { t with cases = cases :: t.cases }
      | Reg reg -> { t with regs = reg :: t.regs }
      | Always_state always_state ->
        { t with always_states = always_state :: t.always_states })
  ;;
end
