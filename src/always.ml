open! Import

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
  [@@deriving fields]

  let sexp_of_t { value; internal = _ } = [%message "" ~_:(value : Signal.t)]
  let uid t = Signal.uid t.internal.assigns_to_wire
  let compare t1 t2 = Signal.Uid.compare (uid t1) (uid t2)
  let equal = [%compare.equal: t]

  include (val Comparator.make ~compare ~sexp_of_t)

  let wire ~default =
    let wire = Signal.wire (Signal.width default) in
    { value = wire; internal = { assigns_to_wire = wire; default } }
  ;;

  let reg spec ~enable ~width =
    let wire = Signal.wire width in
    let reg = Signal.reg spec ~enable wire in
    { value = reg; internal = { assigns_to_wire = wire; default = reg } }
  ;;

  let pipeline ~depth (spec : Reg_spec.t) ~enable ~width =
    if depth = 0
    then
      if (* use a wire - need to derive the default value *)
        Signal.is_empty spec.reg_reset_value
      then wire ~default:(Signal.zero width)
      else wire ~default:spec.reg_reset_value
    else (
      let r = reg spec ~enable ~width in
      (* delay the output by the pipeline length, minus 1 *)
      { r with value = Signal.pipeline ~n:(depth - 1) spec ~enable r.value })
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
          ~guared_variable_width:(Signal.width a.value : int)
          ~expression_width:(Signal.width b : int)
          ~expression:(b : Signal.t)];
  Assign (a, b)
;;

let ( <--. ) (a : Variable.t) b = a <-- Signal.of_int ~width:(Signal.width a.value) b
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
        let s = Signal.reduce ~f:Signal.( |: ) (Signal.bits_msb s) in
        let t = compile_mux t ~default in
        let f = compile_mux f ~default in
        Signal.mux s [ f; t ]
      | Assign (_, d) -> d
      | Switch (sel, cases) ->
        (* This implementation encodes the matches in a linear fashion, which could lead
           to long timing paths.  By analysing the [mtch] values for the whole switch
           statement we could choose a more optimal solution.  Considerations.

           - Are all [mtch] values constants?
           - Are there linear runs of values? ie 0,1,2,3,4,5,6,7
           - Is the set of values very sparse?
           - Should we add an implementation hint?
           - http://www.sunburst-design.com/papers/CummingsSNUG2005Israel_SystemVerilog_UniquePriority.pdf *)
        let rec build = function
          | [] -> default
          | (mtch, case) :: t ->
            Signal.mux (sel ==: mtch) [ build t; compile_mux case ~default ]
        in
        build cases
    in
    compile_mux statements ~default
;;

let compile statements =
  let targets = list_of_set (find_targets (Set.empty (module Variable)) statements) in
  List.iter targets ~f:(fun target ->
    let statements = filter_by_target target statements in
    Signal.( <== )
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

  let create
        ?(encoding = Encoding.Binary)
        (type a)
        (module State : State with type t = a)
        reg_spec
        ~enable
    =
    let module State = struct
      include State
      include Comparator.Make (State)
    end
    in
    let nstates = List.length State.all in
    let ls = if nstates = 1 then 1 else Int.ceil_log2 nstates in
    let state_bits i =
      match encoding with
      | Binary -> Signal.of_int ~width:ls i
      | Gray ->
        Signal.of_bit_string
          (Bits.binary_to_gray (Bits.of_int ~width:ls i) |> Bits.to_bstr)
      | Onehot ->
        let nstates' = if nstates = 1 then 1 else nstates - 1 in
        Signal.of_bit_string
          Bits.(select (binary_to_onehot (of_int ~width:ls i)) nstates' 0 |> to_bstr)
    in
    let states = List.mapi State.all ~f:(fun i s -> s, (i, state_bits i)) in
    let var =
      match encoding with
      | Binary | Gray -> Variable.reg reg_spec ~enable ~width:ls
      | Onehot ->
        Variable.reg
          { reg_spec with
            (* must be reset to get into state 0 *)
            reg_clear_value = Signal.one nstates
          ; reg_reset_value = Signal.one nstates
          }
          ~enable
          ~width:nstates
    in
    let find_state name state =
      match List.Assoc.find states state ~equal:[%compare.equal: State.t] with
      | Some x -> x
      | None ->
        raise_s
          [%message
            (concat [ "[Always.State_machine."; name; "] got unknown state" ])
              ~_:(state : State.t)]
    in
    let state_val name s = snd (find_state name s) in
    let set_next s = var <-- state_val "set_next" s in
    let current = var.value in
    let switch ?default cases =
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
             when_ (Signal.bit current i) c))
    in
    let is s =
      match encoding with
      | Binary | Gray -> state_val "is" s ==: current
      | Onehot -> Signal.bit var.value (fst (find_state "is" s))
    in
    { current; is; set_next; switch }
  ;;
end
