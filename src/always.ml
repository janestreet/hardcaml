open Base

let ( ==: ) = Signal.( ==: )

module Variable = struct
  module Internal = struct
    type t =
      { assigns_to_wire : Signal.t
      ; default : Signal.t
      }
  end

  type internal = Internal.t

  type t =
    { value : Signal.t
    ; internal : Internal.t
    }
  [@@deriving fields ~getters]

  let sexp_of_t { value; internal = _ } = [%message "" ~_:(value : Signal.t)]
  let uid t = Signal.uid t.internal.assigns_to_wire
  let compare t1 t2 = Signal.Type.Uid.compare (uid t1) (uid t2)
  let equal = [%compare.equal: t]

  include (val Comparator.make ~compare ~sexp_of_t)

  let wire ~default () =
    let wire = Signal.wire (Signal.width default) in
    { value = wire; internal = { assigns_to_wire = wire; default } }
  ;;

  let reg ?enable ?initialize_to ?reset_to ?clear ?clear_to spec ~width =
    let wire = Signal.wire width in
    let reg = Signal.reg ?enable ?initialize_to ?reset_to ?clear ?clear_to spec wire in
    { value = reg; internal = { assigns_to_wire = wire; default = reg } }
  ;;

  let pipeline ?enable ?initialize_to ?reset_to ?clear ?clear_to spec ~width ~depth =
    if depth = 0
    then (
      (* use a wire - need to derive the default value *)
      match reset_to with
      | None -> wire ~default:(Signal.zero width) ()
      | Some default -> wire ~default ())
    else (
      let r = reg ?enable ?initialize_to ?reset_to ?clear ?clear_to spec ~width in
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

  let __ppx_auto_name t name =
    (* Note the name provided by the ppx has been scoped already. *)
    ignore (Signal.( -- ) t.value name : Signal.t);
    t
  ;;
end

type t =
  | Assign of Variable.t * Signal.t
  | If of Signal.t * t list * t list
  | Switch of Signal.t * (Signal.t * t list) list
[@@deriving sexp_of]

type always = t [@@deriving sexp_of]
type 'a case = 'a * t list [@@deriving sexp_of]
type 'a cases = 'a case list [@@deriving sexp_of]

let if_ sel on_true on_false = If (sel, on_true, on_false)
let elif c t f = [ if_ c t f ]
let else_ = Fn.id
let when_ sel on_true = if_ sel on_true []
let unless sel on_false = if_ sel [] on_false
let switch sel cases = Switch (sel, cases)
let proc s = if_ Signal.vdd s []

let ( <-- ) (a : Variable.t) b =
  if Signal.width a.value <> Signal.width b
  then
    raise_s
      [%message
        "attempt to assign expression to [Always.variable] of different width"
          ~variable_name:(Signal.names a.value : string list)
          ~guared_variable_width:(Signal.width a.value : int)
          ~expression_width:(Signal.width b : int)
          ~expression:(b : Signal.t)];
  Assign (a, b)
;;

let ( <--. ) (a : Variable.t) b =
  a <-- Signal.of_int_trunc ~width:(Signal.width a.value) b
;;

let ( <-:. ) (a : Variable.t) b =
  a <-- Signal.of_unsigned_int ~width:(Signal.width a.value) b
;;

let ( <-+. ) (a : Variable.t) b =
  a <-- Signal.of_signed_int ~width:(Signal.width a.value) b
;;

let incr ?(by = 1) (a : Variable.t) = a <-- Signal.(a.value +:. by)
let decr ?(by = 1) (a : Variable.t) = a <-- Signal.(a.value -:. by)
let list_of_set s = Set.fold s ~init:[] ~f:(fun l e -> e :: l)

let rec find_targets set statements =
  List.fold statements ~init:set ~f:(fun set statement ->
    match statement with
    | Assign (variable, _) -> Set.add set variable
    | If (_, t, f) ->
      let set = find_targets set t in
      find_targets set f
    | Switch (_, cases) ->
      List.fold cases ~init:set ~f:(fun set case -> find_targets set (snd case)))
;;

let filter_by_target variable statements =
  let rec loops statements = List.filter_map statements ~f:loop
  and loop statement =
    match statement with
    | Assign (v, _) -> if Variable.equal v variable then Some statement else None
    | If (s, t, f) ->
      let t = loops t in
      let f = loops f in
      if List.is_empty t && List.is_empty f then None else Some (If (s, t, f))
    | Switch (sel, cases) ->
      let cases =
        List.filter_map cases ~f:(fun (m, c) ->
          let c = loops c in
          if List.is_empty c then None else Some (m, c))
      in
      if List.is_empty cases then None else Some (Switch (sel, cases))
  in
  loops statements
;;

let rec compile_mux statements ~default =
  match statements with
  | [] -> default
  | statement :: statements ->
    let default =
      match statement with
      | If (s, t, f) ->
        let s = Signal.any_bit_set s in
        let t = compile_mux t ~default in
        let f = compile_mux f ~default in
        Signal.mux s [ f; t ]
      | Assign (_, d) -> d
      | Switch (sel, cases) ->
        (* Encode the matches as a linear chain of mux's. Can be used when the matches are
           not constants. *)
        let rec build_generic = function
          | [] -> default
          | (mtch, case) :: t ->
            Signal.mux (sel ==: mtch) [ build_generic t; compile_mux case ~default ]
        in
        (* Encode the matches using [cases]. *)
        let build_constant_cases cases =
          Signal.cases
            ~default
            sel
            (List.map cases ~f:(fun (match_with, always) ->
               match_with, compile_mux always ~default))
        in
        let constant_cases =
          List.for_all cases ~f:(fun (match_with, _) -> Signal.Type.is_const match_with)
        in
        if constant_cases then build_constant_cases cases else build_generic cases
    in
    compile_mux statements ~default
;;

let compile statements =
  let targets = list_of_set (find_targets (Set.empty (module Variable)) statements) in
  List.iter targets ~f:(fun target ->
    let statements = filter_by_target target statements in
    Signal.( <-- )
      target.internal.assigns_to_wire
      (compile_mux statements ~default:target.internal.default))
;;

module State_machine = struct
  type 'a t =
    { current : Signal.t
    ; is : 'a -> Signal.t
    ; set_next : 'a -> always
    ; switch : ?default:always list -> 'a cases -> always
    }
  [@@deriving sexp_of]

  module Encoding = struct
    type t =
      | Binary
      | Gray
      | Onehot
    [@@deriving sexp_of]

    let to_string t = [%sexp (t : t)] |> Sexp.to_string
  end

  module type State = sig
    type t [@@deriving compare, enumerate, sexp_of]
  end

  let apply_statemachine_attributes attributes ~state =
    let attributes =
      Option.value
        ~default:
          [ (* We choose a pretty aggressive default which is to code ALL statemachines as
               one-hot by default. The experiments we performed showed better performance
               and smaller area (more registers but fewer CLBs overall) for both very
               small and very large statemachines. Which is not to say this is the final
               word on the best possible design choice - users may still want to
               selectively configure certain statemachines to a different encoding. *)
            Rtl_attribute.Vivado.fsm_encoding `one_hot
          ]
        attributes
    in
    List.iter attributes ~f:(fun attr ->
      ignore (Signal.add_attribute state attr : Signal.t))
  ;;

  let create
    (type a)
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

      let name_by_index =
        Array.of_list_map all ~f:(fun s -> Sexp.to_string [%sexp (s : State.t)])
      ;;
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
        Bits.(select (binary_to_onehot (of_int_trunc ~width:ls i)) ~high:nstates' ~low:0)
    in
    let state_signal i = state_bits i |> Bits.to_constant |> Signal.of_constant in
    let states = List.mapi State.all ~f:(fun i s -> s, (i, state_signal i)) in
    let var =
      match encoding with
      | Binary | Gray -> Variable.reg reg_spec ~enable ~width:ls
      | Onehot ->
        Variable.reg (* must be reset to get into state 0 *)
          reg_spec
          ~clear_to:(Signal.one nstates)
          ~reset_to:(Signal.one nstates)
          ~enable
          ~width:nstates
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
    let state_val name s = snd (find_state name s) in
    let set_next s = var <-- state_val "set_next" s in
    let current = var.value in
    let switch ?default cases =
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
      match encoding with
      | Binary | Gray ->
        switch current (List.map cases ~f:(fun (s, c) -> state_val "switch" s, c))
      | Onehot ->
        proc
          (List.map cases ~f:(fun (s, c) ->
             let i, _ = find_state "switch" s in
             when_ current.Signal.:(i) c))
    in
    let is s =
      match encoding with
      | Binary | Gray -> state_val "is" s ==: current
      | Onehot -> var.value.Signal.:(fst (find_state "is" s))
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
