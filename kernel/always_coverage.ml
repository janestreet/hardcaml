open! Core0
include Always_coverage_intf
module Type = Signal.Type

module Make (Prim : Always_prim.S) = struct
  open Prim

  let signal_type s = Signal.to_rep s |> fst

  let set_variable_metadata signal metadata =
    Type.update_coverage_metadata (signal_type signal) ~f:(fun m ->
      Coverage_metadata.set_kind m (Variable metadata))
  ;;

  let target_metadata (target : Variable.t) =
    { Coverage_prim.Always_metadata.Target.variable = target.internal.debug_info
    ; names = Signal.names_and_locs target.value
    }
  ;;

  let set_if_metadata signal (internal : If_internal.t) ~target =
    let%tydi { creation_pos; kind } = internal in
    Type.update_coverage_metadata (signal_type signal) ~f:(fun m ->
      Coverage_metadata.set_kind
        m
        (match kind with
         | Condition kind -> If { creation_pos; target = target_metadata target; kind }
         | One_hot_switch { state; _ } ->
           Switch_mux
             { creation_pos; target = target_metadata target; case = State state }))
  ;;

  let set_switch_cases_metadata signal (switch_internal : Switch_internal.t) cases ~target
    =
    let%tydi { creation_pos; states } = switch_internal in
    let cases =
      let non_default_cases =
        List.map cases ~f:(fun { internal; _ } ->
          match internal with
          | Basic -> `Positional
          | Always_state state -> `State state)
      in
      let default_states =
        List.fold
          non_default_cases
          ~init:(Set.of_list (module Coverage_prim.State.Named) states)
          ~f:(fun states c ->
            match c with
            | `Positional -> states
            | `State state -> Set.remove states state)
        |> Set.to_list
      in
      Coverage_prim.Cases.create ~non_default_cases ~default_states
    in
    Type.update_coverage_metadata (signal_type signal) ~f:(fun m ->
      Coverage_metadata.set_kind
        m
        (Switch_cases { creation_pos; target = target_metadata target; cases }))
  ;;

  let set_switch_mux_metadata
    signal
    (switch_internal : Switch_internal.t)
    (match_internal : Match_internal.t)
    ~target
    ~position
    =
    let%tydi { creation_pos; _ } = switch_internal in
    let case : Coverage_prim.Case.t =
      match match_internal with
      | Basic -> Positional position
      | Always_state state -> State state
    in
    Type.update_coverage_metadata (signal_type signal) ~f:(fun m ->
      Coverage_metadata.set_kind
        m
        (Switch_mux { creation_pos; target = target_metadata target; case }))
  ;;

  (* Module for computing cycle level state transitions from an always DSL. Transitions
     are tracked by walking the statements and computing the following.

     Possible starting states.

     Switches and if/else blocks restrict this set, but variable assignments do not, as we
     only care about transitions from the start to the end of the cycle and not in
     between. There is some complexity in this tracking to handle both restricted subsets
     of all possible starting states and explicitly known states.

     Possible ending states.

     We need to know all states that can be produced at the end of a cycle so that we can
     accurately compute the full set of possible starting states.

     Possible transitions.

     Conditional logic restricts which transitions can actually take place out of the
     possible starting and ending states, so we track these directly. We count no change
     to the starting state as its own 'transition'. At an assignment site we may not know
     the concrete set of possible starting states, as it maybe a restricted subset of the
     all possible starting state. We allow this in the transition type, and resolve it
     after we have walked all the statements.
  *)
  module Transition_tracking = struct
    module Transition = struct
      type t =
        | No_change of [ `Initial_excluding of Set.M(Int).t | `Value of int ]
        | Change of ([ `Initial_excluding of Set.M(Int).t | `Value of int ] * int)
      [@@deriving sexp_of]
    end

    module Starting_states = struct
      type t =
        { values : Set.M(Int).t
        ; initial : [ `No | `Excluding of Set.M(Int).t ]
        }
      [@@deriving sexp_of]

      let create () =
        { values = Set.empty (module Int); initial = `Excluding (Set.empty (module Int)) }
      ;;

      let of_value value = { values = Set.singleton (module Int) value; initial = `No }

      let remove_value t value =
        { values = Set.remove t.values value
        ; initial =
            (match t.initial with
             | `No -> `No
             | `Excluding set -> `Excluding (Set.add set value))
        }
      ;;

      let join a b =
        let values = Set.union a.values b.values in
        let initial =
          match a.initial, b.initial with
          | `No, `No -> `No
          | `No, `Excluding set | `Excluding set, `No -> `Excluding set
          | `Excluding s1, `Excluding s2 -> `Excluding (Set.inter s1 s2)
        in
        { values; initial }
      ;;
    end

    module Linked_list = Core.Doubly_linked

    type t =
      { possible_starting_states : Starting_states.t
      ; possible_ending_states : Set.M(Int).t
      ; transitions :
          (* The list of transitions can be large and we need to combine lists of them
             together frequently, so store them as a linked list *)
          Transition.t Linked_list.t
      }
    [@@deriving sexp_of]

    let create () =
      { possible_starting_states = Starting_states.create ()
      ; possible_ending_states = Set.empty (module Int)
      ; transitions = Linked_list.create ()
      }
    ;;

    let set_input_value t value =
      { t with possible_starting_states = Starting_states.of_value value }
    ;;

    let remove_input_value t value =
      { t with
        possible_starting_states =
          Starting_states.remove_value t.possible_starting_states value
      }
    ;;

    let insert_last linked a =
      ignore (Linked_list.insert_last linked a : Transition.t Linked_list.Elt.t)
    ;;

    let maybe_mark_assign t (metadata : Coverage_prim.State.Named.t option) =
      match metadata with
      | None ->
        (* We can't mark an assignment if we know which state is being assigned *)
        t
      | Some { value = new_value; _ } ->
        (* When we see an assignment, mark a transition from each of the possible starting
           states to the assigned state. Override any ending states with the assigned
           state, as we only care about the *last* assignment to happen. *)
        let transitions = Linked_list.create () in
        Set.iter t.possible_starting_states.values ~f:(fun value ->
          insert_last transitions (Transition.Change (`Value value, new_value)));
        (match t.possible_starting_states.initial with
         | `No -> ()
         | `Excluding set ->
           insert_last transitions (Change (`Initial_excluding set, new_value)));
        { t with
          transitions
        ; possible_ending_states = Set.singleton (module Int) new_value
        }
    ;;

    let maybe_mark_no_change t =
      (* If we've reached the bottom of a statement tree an there were no transitions,
         mark a no-change 'transition' for each of the possible starting states. *)
      if Linked_list.length t.transitions = 0
      then (
        let transitions = Linked_list.create () in
        Set.iter t.possible_starting_states.values ~f:(fun value ->
          insert_last transitions (Transition.No_change (`Value value)));
        (match t.possible_starting_states.initial with
         | `No -> ()
         | `Excluding set -> insert_last transitions (No_change (`Initial_excluding set)));
        { t with transitions; possible_ending_states = t.possible_starting_states.values })
      else t
    ;;

    let any_of t ts =
      (* When joining statement trees together (both sides of a if or arms of a case),
         combine all possible starting states, transitions, and ending states. *)
      List.fold
        ts
        ~init:t
        ~f:(fun acc { possible_starting_states; possible_ending_states; transitions } ->
          let joined_transitions = Linked_list.create () in
          Linked_list.transfer ~dst:joined_transitions ~src:acc.transitions;
          Linked_list.transfer ~dst:joined_transitions ~src:transitions;
          { possible_starting_states =
              Starting_states.join acc.possible_starting_states possible_starting_states
          ; possible_ending_states =
              Set.union acc.possible_ending_states possible_ending_states
          ; transitions = joined_transitions
          })
    ;;

    let is_target s ~target =
      Type.(Type.Uid.equal (uid (signal_type s)) (uid (signal_type target)))
    ;;

    let rec compute_transitions statements t ~target =
      match statements with
      | [] -> maybe_mark_no_change t
      | statement :: statements ->
        let t =
          match statement with
          | If { value = _, true_, false_; internal = { kind; _ } } ->
            let true_t, false_t =
              match kind with
              | Condition _ -> t, t
              | One_hot_switch { var; state = { value; _ } } ->
                if is_target var.value ~target
                then set_input_value t value, remove_input_value t value
                else t, t
            in
            let t = compute_transitions true_ true_t ~target in
            let f = compute_transitions false_ false_t ~target in
            any_of t [ f ]
          | Assign { value = { value = signal; _ }, _; internal = { state_metadata } } ->
            if is_target signal ~target then maybe_mark_assign t state_metadata else t
          | Switch { value = sel, cases; _ } ->
            let case_ts, default_t =
              List.fold
                cases
                ~init:([], t)
                ~f:(fun (case_ts, default_t) { value = _, always; internal } ->
                  let case_t, default_t =
                    match internal with
                    | Basic -> t, t
                    | Always_state { value; _ } ->
                      if is_target sel ~target
                      then set_input_value t value, remove_input_value default_t value
                      else t, t
                  in
                  let case_t = compute_transitions always case_t ~target in
                  case_t :: case_ts, default_t)
            in
            let default_t = compute_transitions [] default_t ~target in
            any_of default_t case_ts
        in
        compute_transitions statements t ~target
    ;;

    let mark_transitions
      t
      (state_reg : Coverage_prim.Always_metadata.Variable.State_reg.t)
      =
      let%tydi { transitions_by_value; initial_value; _ } = state_reg in
      let starting_states = Set.add t.possible_ending_states initial_value in
      let value_to_set v =
        match v with
        | `Initial_excluding excluding -> Set.diff starting_states excluding
        | `Value value -> Set.singleton (module Int) value
      in
      let add_transition ~from ~to_ =
        let prev_set =
          Hashtbl.update_and_return transitions_by_value from ~f:(function
            | None -> Hash_set.create (module Int)
            | Some set -> set)
        in
        Hash_set.add prev_set to_
      in
      Linked_list.iter t.transitions ~f:(function
        | No_change from_value ->
          Set.iter (value_to_set from_value) ~f:(fun from ->
            add_transition ~from ~to_:from)
        | Change (from_value, to_) ->
          Set.iter (value_to_set from_value) ~f:(fun from -> add_transition ~from ~to_))
    ;;

    let maybe_set_transitions_metadata statements ~target =
      (* Transition metadata is only used for Cyclesim coverage and can be expensive to
         compute, so skip it if we can. *)
      if Coverage_global_config.coverage_enabled ()
      then (
        match Type.coverage_metadata (signal_type target) with
        | Some { kind = Some (Variable (State_machine_state state_reg)); _ } ->
          let t = compute_transitions statements (create ()) ~target in
          mark_transitions t state_reg
        | _ -> ())
    ;;
  end

  let maybe_set_transitions_metadata = Transition_tracking.maybe_set_transitions_metadata
end
