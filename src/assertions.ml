open! Base

type t' =
  { assertions : Signal.t Map.M(String).t
  ; asserted : (string, int list) Hashtbl.t
  }

type t = t' option

module Violated_or_not = struct
  type t =
    | Violated of int list
    | Not_violated
  [@@deriving sexp_of]
end

let circuit_config_with_assertions
  ?(config = Circuit.Config.default_for_simulations)
  scope
  =
  if Scope.trace_properties scope
  then { config with assertions = Scope.assertion_manager scope; port_checks = Relaxed }
  else config
;;

let trace' sim assertion_manager =
  let cycle_no = ref 0 in
  let assertions = Assertion_manager.finalize assertion_manager in
  let asserted = Hashtbl.create (module String) in
  let clear_violated_assertions () =
    cycle_no := 0;
    Hashtbl.clear asserted
  in
  let asserts =
    Map.to_alist assertions
    |> List.map ~f:(fun (port_name, _) ->
         port_name, Cyclesim.out_port sim ~clock_edge:Before port_name)
  in
  let check_assertions () =
    List.iter asserts ~f:(fun (name, bits) ->
      if Bits.is_gnd !bits then Hashtbl.add_multi asserted ~key:name ~data:!cycle_no);
    Int.incr cycle_no
  in
  let sim =
    Cyclesim.Private.modify
      sim
      [ After, Reset, clear_violated_assertions
      ; After, Before_clock_edge, check_assertions
      ]
  in
  Some { assertions; asserted }, sim
;;

let trace sim assertion_manager =
  match assertion_manager with
  | None -> None, sim
  | Some assertion_manager -> trace' sim assertion_manager
;;

let results t =
  Option.map t ~f:(fun t ->
    Map.mapi t.assertions ~f:(fun ~key ~data:_ ->
      match Hashtbl.find t.asserted key with
      | Some cycles -> Violated_or_not.Violated (List.rev cycles)
      | None -> Violated_or_not.Not_violated))
;;

let sexp_of_t t = [%sexp (results t : Violated_or_not.t Map.M(String).t option)]

let check_assertion_width name assertion =
  if Signal.width assertion <> 1
  then
    raise_s
      [%message "Assertion signals must be 1 bit" (name : string) (assertion : Signal.t)]
;;

let add scope name assertion =
  check_assertion_width name assertion;
  Option.iter (Scope.assertion_manager scope) ~f:(fun assertion_manager ->
    let name = Scope.name scope name in
    Assertion_manager.add assertion_manager name assertion)
;;

module Always = struct
  let add scope name assertion =
    check_assertion_width name assertion;
    let assertion_var = Always.Variable.wire ~default:(Signal.one 1) in
    add scope name (Always.Variable.value assertion_var);
    Always.( <-- ) assertion_var assertion
  ;;
end
