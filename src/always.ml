open! Core0
include Always_intf
module Type = Signal.Type

module Make
    (Signal : Signal.S)
    (M : sig
       val validate_assignment : dst:Signal.t -> src:Signal.t -> unit
     end) =
struct
  open M
  module Always_prim = Always_prim.Make (Signal)
  module Always_coverage = Always_coverage.Make (Always_prim)

  let ( ==: ) = Signal.( ==: )
  let signal_type s = Signal.to_rep s |> fst

  include Always_prim

  module Variable_ = struct
    include Variable

    type internal = Internal.t

    let sexp_of_t { value; internal = _ } = [%message "" ~_:(value : Signal.t)]
    let uid t = Type.uid (signal_type t.internal.assigns_to_wire)
    let compare t1 t2 = Type.Uid.compare (uid t1) (uid t2)
    let equal = [%compare.equal: t]

    (* Explicitly use Comparator.Make because Comparator.make will force this functor to
       be applicative. *)
    include Comparator.Make (struct
        type nonrec t = t

        let compare = compare
        let sexp_of_t = sexp_of_t
      end)

    let wire' ~default debug_info =
      let info = Signal.info default in
      let wire = Signal.wire (Signal.width default) |> Signal.set_info ~info in
      { value = wire; internal = { assigns_to_wire = wire; default; debug_info } }
    ;;

    let wire ~(here : [%call_pos]) ~default () = wire' ~default (User_created here)

    let reg' ?enable ?initialize_to ?reset_to ?clear ?clear_to spec ~width debug_info =
      let info = Signal.Reg_spec.clock spec |> Signal.info in
      let wire = Signal.wire width |> Signal.set_info ~info in
      let reg = Signal.reg ?enable ?initialize_to ?reset_to ?clear ?clear_to spec wire in
      Always_coverage.set_variable_metadata reg debug_info;
      { value = reg; internal = { assigns_to_wire = wire; default = reg; debug_info } }
    ;;

    let reg
      ~(here : [%call_pos])
      ?enable
      ?initialize_to
      ?reset_to
      ?clear
      ?clear_to
      spec
      ~width
      =
      reg'
        ?enable
        ?initialize_to
        ?reset_to
        ?clear
        ?clear_to
        spec
        ~width
        (User_created here)
    ;;

    let pipeline'
      ?enable
      ?initialize_to
      ?reset_to
      ?clear
      ?clear_to
      spec
      ~width
      ~depth
      debug_info
      =
      if depth = 0
      then (
        (* use a wire - need to derive the default value *)
        match reset_to with
        | None -> wire' ~default:(Signal.zero width) debug_info
        | Some default -> wire ~default:(Signal.of_bits default) ())
      else (
        let r =
          reg' ?enable ?initialize_to ?reset_to ?clear ?clear_to spec ~width debug_info
        in
        (* delay the output by the pipeline length, minus 1 *)
        { r with
          value =
            Signal.pipeline
              ?enable
              ?initialize_to
              ?reset_to
              ?clear
              ?clear_to
              ~n:(depth - 1)
              spec
              r.value
        })
    ;;

    let pipeline
      ~(here : [%call_pos])
      ?enable
      ?initialize_to
      ?reset_to
      ?clear
      ?clear_to
      spec
      ~width
      ~depth
      =
      pipeline'
        ?enable
        ?initialize_to
        ?reset_to
        ?clear
        ?clear_to
        spec
        ~width
        ~depth
        (User_created here)
    ;;

    let __ppx_auto_name t name =
      (* Note the name provided by the ppx has been scoped already. *)
      ignore (Signal.( -- ) t.value name : Signal.t);
      t
    ;;

    module Expert = struct
      let map_value t ~f = { t with value = f t.value }
    end
  end

  module Assign_internal = struct
    include Assign_internal
  end

  type assign_internal = Assign_internal.t

  module If_internal = struct
    include If_internal
  end

  type if_internal = If_internal.t

  module Match_internal = struct
    include Match_internal
  end

  type match_internal = Match_internal.t

  module Switch_internal = struct
    include Switch_internal
  end

  type switch_internal = Switch_internal.t
  type always = t [@@deriving sexp_of]
  type 'a case = 'a * t list [@@deriving sexp_of]
  type 'a cases = 'a case list [@@deriving sexp_of]

  let[@cold] raise_cond_width_not_one signal =
    raise_s
      [%message.omit_nil
        "Condition in [Always.if_] and [Always.when_] must be 1 bit"
          ~variable_name:(Signal.names signal : string list)
          ~width:(Signal.width signal : int)]
  ;;

  let assert_cond_width_is_one signal =
    if not (Signal.width signal = 1) then raise_cond_width_not_one signal
  ;;

  let if_' sel on_true on_false internal =
    assert_cond_width_is_one sel;
    If { value = sel, on_true, on_false; internal }
  ;;

  let if_ ~(here : [%call_pos]) sel on_true on_false =
    if_' sel on_true on_false { kind = Condition If; creation_pos = here }
  ;;

  let elif ~(here : [%call_pos]) c t f =
    [ if_' c t f { kind = Condition Elif; creation_pos = here } ]
  ;;

  let else_ = Fn.id
  let when_' sel on_true debug_info = if_' sel on_true [] debug_info

  let when_ ~(here : [%call_pos]) sel on_true =
    if_' sel on_true [] { kind = Condition When; creation_pos = here }
  ;;

  let unless ~(here : [%call_pos]) sel on_false =
    if_' sel [] on_false { kind = Condition Unless; creation_pos = here }
  ;;

  let switch' sel cases internal = Switch { value = sel, cases; internal }

  let switch ~(here : [%call_pos]) sel cases =
    switch'
      sel
      (List.map cases ~f:(fun value -> { value; internal = Basic }))
      { creation_pos = here; states = [] }
  ;;

  let proc ~(here : [%call_pos]) s =
    if_' Signal.vdd s [] { kind = Condition Proc; creation_pos = here }
  ;;

  let assign ?state_metadata (a : Variable_.t) b =
    if Signal.width a.value <> Signal.width b
    then
      raise_s
        [%message
          "attempt to assign expression to [Always.variable] of different width"
            ~variable_name:(Signal.names a.value : string list)
            ~guared_variable_width:(Signal.width a.value : int)
            ~expression_width:(Signal.width b : int)
            ~expression:(b : Signal.t)];
    validate_assignment ~dst:a.value ~src:b;
    Assign { value = a, b; internal = { state_metadata } }
  ;;

  let ( <-- ) a b = assign a b

  let ( <--. ) (a : Variable_.t) b =
    let info = Signal.info a.value in
    a <-- (Signal.of_int_trunc ~width:(Signal.width a.value) b |> Signal.set_info ~info)
  ;;

  let ( <-:. ) (a : Variable_.t) b =
    let info = Signal.info a.value in
    a <-- (Signal.of_unsigned_int ~width:(Signal.width a.value) b |> Signal.set_info ~info)
  ;;

  let ( <-+. ) (a : Variable_.t) b =
    let info = Signal.info a.value in
    a <-- (Signal.of_signed_int ~width:(Signal.width a.value) b |> Signal.set_info ~info)
  ;;

  let incr ?(by = 1) (a : Variable_.t) = a <-- Signal.(a.value +:. by)
  let decr ?(by = 1) (a : Variable_.t) = a <-- Signal.(a.value -:. by)
  let list_of_set s = Set.fold s ~init:[] ~f:(fun l e -> e :: l)

  let rec find_targets set statements =
    List.fold statements ~init:set ~f:(fun set statement ->
      match statement with
      | Assign { value = variable, _; _ } -> Set.add set variable
      | If { value = _, t, f; _ } ->
        let set = find_targets set t in
        find_targets set f
      | Switch { value = _, cases; _ } ->
        List.fold cases ~init:set ~f:(fun set { value; _ } ->
          find_targets set (snd value)))
  ;;

  let filter_by_target variable statements =
    let rec loops statements = List.filter_map statements ~f:loop
    and loop statement =
      match statement with
      | Assign { value = v, _; _ } ->
        if Variable_.equal v variable then Some statement else None
      | If { value = s, t, f; internal } ->
        let t = loops t in
        let f = loops f in
        if List.is_empty t && List.is_empty f
        then None
        else Some (If { value = s, t, f; internal })
      | Switch { value = sel, cases; internal } ->
        let cases =
          List.filter_map cases ~f:(fun { value = m, c; internal } ->
            let c = loops c in
            if List.is_empty c then None else Some { value = m, c; internal })
        in
        if List.is_empty cases
        then None
        else Some (Switch { value = sel, cases; internal })
    in
    loops statements
  ;;

  let rec compile_mux statements ~default ~target =
    match statements with
    | [] -> default
    | statement :: statements ->
      let default =
        match statement with
        | If { value = s, t, f; internal } ->
          assert_cond_width_is_one s;
          let t = compile_mux t ~default ~target in
          let f = compile_mux f ~default ~target in
          let signal = Signal.mux s [ f; t ] in
          Always_coverage.set_if_metadata signal internal ~target;
          signal
        | Assign { value = _, d; internal = _ } -> d
        | Switch { value = sel, cases; internal = switch_internal } ->
          (* Encode the matches as a linear chain of mux's. Can be used when the matches
             are not constants. *)
          let rec build_generic t ~position =
            match t with
            | [] -> default
            | { value = mtch, case; internal = match_internal } :: t ->
              let signal =
                Signal.mux
                  (sel ==: mtch)
                  [ build_generic t ~position:(position + 1)
                  ; compile_mux case ~default ~target
                  ]
              in
              Always_coverage.set_switch_mux_metadata
                signal
                switch_internal
                match_internal
                ~target
                ~position;
              signal
          in
          (* Encode the matches using [cases]. *)
          let build_constant_cases cases =
            let signal =
              Signal.cases
                ~default
                sel
                (List.map cases ~f:(fun { value = match_with, always; _ } ->
                   match_with, compile_mux always ~default ~target))
            in
            Always_coverage.set_switch_cases_metadata signal switch_internal cases ~target;
            signal
          in
          let constant_cases =
            List.for_all cases ~f:(fun { value = match_with, _; _ } ->
              Type.is_const (signal_type match_with))
          in
          if constant_cases
          then build_constant_cases cases
          else build_generic cases ~position:0
      in
      compile_mux statements ~default ~target
  ;;

  let compile statements =
    let targets = list_of_set (find_targets (Set.empty (module Variable_)) statements) in
    (* Collect all the statements that assign to a particular variable and compile them
       into a mux. *)
    List.iter targets ~f:(fun target ->
      let statements = filter_by_target target statements in
      Always_coverage.maybe_set_transitions_metadata statements ~target:target.value;
      Signal.( <-- )
        target.internal.assigns_to_wire
        (compile_mux statements ~default:target.internal.default ~target))
  ;;

  module State_machine = struct
    type 'a t =
      { current : Signal.t
      ; is : 'a -> Signal.t
      ; set_next : 'a -> always
      ; switch : here:[%call_pos] -> ?default:always list -> 'a cases -> always
      }
    [@@deriving sexp_of]

    module Encoding = struct
      type t =
        | Binary
        | Gray
        | Onehot
      [@@deriving sexp_of, enumerate]

      let to_string t = [%sexp (t : t)] |> Sexp.to_string
    end

    module type State = sig
      type t [@@deriving compare ~localize, enumerate, sexp_of]
    end

    let apply_statemachine_attributes attributes ~state =
      let attributes =
        Option.value
          ~default:
            [ (* We choose a pretty aggressive default which is to code ALL statemachines
                 as one-hot by default. The experiments we performed showed better
                 performance and smaller area (more registers but fewer CLBs overall) for
                 both very small and very large statemachines. Which is not to say this is
                 the final word on the best possible design choice - users may still want
                 to selectively configure certain statemachines to a different encoding. *)
              Rtl_attribute.Vivado.fsm_encoding `one_hot
            ]
          attributes
      in
      List.iter attributes ~f:(fun attr ->
        ignore (Signal.add_attribute state attr : Signal.t))
    ;;

    let create
      (type a)
      ~(here : [%call_pos])
      ?(encoding = Encoding.Binary)
      ?(auto_wave_format = true)
      ?attributes
      ?(enable = Signal.vdd)
      ?(unreachable = ([] : a list))
      (module State : State with type t = a)
      reg_spec
      =
      let module State = struct
        include State

        let equal a b = compare a b = 0
        let all = List.filter all ~f:(Fn.non (List.mem unreachable ~equal))

        include Comparator.Make (State)

        let to_string s = Sexp.to_string [%sexp (s : State.t)]
        let name_by_index = Array.of_list_map all ~f:to_string

        type metadata =
          { i : int
          ; signal : Signal.t
          ; metadata : Coverage_prim.State.Named.t
          }
      end
      in
      let nstates = List.length State.all in
      let ls = if nstates = 1 then 1 else Int.ceil_log2 nstates in
      let state_bits i =
        match encoding with
        | Binary -> Bits.of_int_trunc ~width:ls i
        | Gray -> Bits.binary_to_gray (Bits.of_int_trunc ~width:ls i)
        | Onehot ->
          let nstates' = if nstates = 1 then 1 else nstates - 1 in
          Bits.(
            select (binary_to_onehot (of_int_trunc ~width:ls i)) ~high:nstates' ~low:0)
      in
      let states =
        List.mapi State.all ~f:(fun i state ->
          let bits = state_bits i in
          let signal = Bits.to_constant bits |> Signal.of_constant in
          ( state
          , { State.i
            ; signal
            ; metadata = { name = State.to_string state; value = Bits.to_int_trunc bits }
            } ))
      in
      let states_by_value =
        List.map states ~f:(fun (_, { metadata; _ }) -> metadata.value, metadata)
        |> Map.of_alist_exn (module Int)
      in
      let var =
        let debug_info : Coverage_prim.Always_metadata.Variable.t =
          State_machine_state
            { creation_pos = here
            ; states_by_value
            ; transitions_by_value = Hashtbl.create (module Int)
            ; initial_value =
                (match encoding with
                 | Binary | Gray -> 0
                 | Onehot -> 1)
            }
        in
        match encoding with
        | Binary | Gray -> Variable_.reg' reg_spec ~enable ~width:ls debug_info
        | Onehot ->
          Variable_.reg' (* must be reset to get into state 0 *)
            reg_spec
            ~clear_to:(Signal.one nstates)
            ~reset_to:(Bits.one nstates)
            ~enable
            ~width:nstates
            debug_info
      in
      apply_statemachine_attributes attributes ~state:var.value;
      let find_state name state =
        match List.Assoc.find states state ~equal:[%compare.equal: State.t] with
        | Some x -> x
        | None ->
          raise_s
            [%message
              (String.concat [ "[Always.State_machine."; name; "] got unknown state" ])
                ~_:(state : State.t)]
      in
      let state_signal name s = (find_state name s).signal in
      let set_next s =
        let%tydi { signal; metadata; _ } = find_state "set_next" s in
        assign var signal ~state_metadata:metadata
      in
      let current = var.value in
      let switch ~(here : [%call_pos]) ?default cases =
        let cases =
          List.filter_map cases ~f:(fun (state, impl) ->
            if List.mem unreachable state ~equal:State.equal
            then
              if List.is_empty impl
              then
                (* Lazy programmer convenience - you can speficy the state, but it must
                   have an empty implementation. *)
                None
              else
                raise_s
                  [%message
                    "[Always.State_machine.switch] unreachable state provided with a \
                     non-empty implementation"
                      (state : State.t)]
            else Some (state, impl))
        in
        let rec unique set = function
          | [] -> set
          | (state, _) :: tl ->
            if Set.mem set state
            then
              raise_s
                [%message
                  "[Always.State_machine.switch] got repeated state" ~_:(state : State.t)];
            unique (Set.add set state) tl
        in
        let all_states = Set.of_list (module State) State.all in
        let case_states = unique (Set.empty (module State)) cases in
        let unknown_states = Set.diff case_states all_states in
        if not (Set.is_empty unknown_states)
        then
          raise_s
            [%message
              "[Always.State_machine.switch] got unknown states"
                ~_:(unknown_states : Set.M(State).t)];
        let unhandled_states = Set.diff all_states case_states in
        let cases =
          match default with
          | None ->
            if not (Set.is_empty unhandled_states)
            then
              raise_s
                [%message
                  "[Always.State_machine.switch] without [~default] had unhandled states"
                    ~_:(unhandled_states : Set.M(State).t)];
            cases
          | Some default ->
            cases
            @ (Set.to_list unhandled_states |> List.map ~f:(fun state -> state, default))
        in
        let cases = List.map cases ~f:(fun (s, c) -> find_state "switch" s, c) in
        match encoding with
        | Binary | Gray ->
          let states = List.map cases ~f:(fun ({ metadata; _ }, _) -> metadata) in
          switch'
            current
            (List.map cases ~f:(fun ({ signal; metadata; _ }, c) ->
               { value = signal, c; internal = Always_state metadata }))
            { creation_pos = here; states }
        | Onehot ->
          proc
            (List.map cases ~f:(fun ({ i; metadata; _ }, c) ->
               when_'
                 current.Signal.:(i)
                 c
                 { creation_pos = here; kind = One_hot_switch { var; state = metadata } }))
      in
      let is s =
        match encoding with
        | Binary | Gray -> state_signal "is" s ==: current
        | Onehot -> var.value.Signal.:((find_state "is" s).i)
      in
      if auto_wave_format
      then (
        let wave_format =
          let rec state_map i =
            if i = nstates
            then []
            else (state_bits i, State.name_by_index.(i)) :: state_map (i + 1)
          in
          Wave_format.Map (state_map 0)
        in
        ignore (Signal.(var.value --$ wave_format) : Signal.t));
      { current; is; set_next; switch }
    ;;

    let __ppx_auto_name t name =
      ignore (Signal.( -- ) t.current name : Signal.t);
      t
    ;;
  end

  module Variable = struct
    type internal = Variable_.internal

    type t = Variable_.t =
      { value : Signal.t
      ; internal : Variable_.Internal.t
      }

    let sexp_of_t = Variable_.sexp_of_t
    let value = Variable_.value
    let wire = Variable_.wire
    let reg = Variable_.reg
    let pipeline = Variable_.pipeline

    let cut_through_reg ?enable ?initialize_to ?reset_to ?clear ?clear_to spec ~width =
      let r =
        Variable_.reg ?enable ?initialize_to ?reset_to ?clear ?clear_to spec ~width
      in
      let w = Variable_.wire ~default:r.value () in
      compile [ r <-- w.value ];
      w
    ;;

    let __ppx_auto_name = Variable_.__ppx_auto_name

    module Expert = struct
      let map_value = Variable_.Expert.map_value
    end
  end
end

include
  Make
    (Signal)
    (struct
      let validate_assignment ~dst:_ ~src:_ = ()
    end)

module Clocked = struct
  include
    Make
      (Clocked_signal)
      (struct
        let validate_assignment ~dst ~src =
          Clocked_signal.Expert.validate_assign_in_always ~dst ~src
        ;;
      end)

  let set_variable_dom (t : Variable.t) ~dom =
    { Variable.value = Clocked_signal.Unsafe.set_domain ~dom t.value
    ; internal =
        { assigns_to_wire =
            Clocked_signal.Unsafe.set_domain ~dom t.internal.assigns_to_wire
        ; default = Clocked_signal.Unsafe.set_domain ~dom t.internal.default
        ; debug_info = t.internal.debug_info
        }
    }
  ;;

  module Variable_overrides = struct
    let wire ~default ~dom =
      let default =
        Clocked_signal.ensure_domain ~name:"default" ~required_domain:dom default
      in
      Variable.wire ~default () |> set_variable_dom ~dom
    ;;
  end
end
