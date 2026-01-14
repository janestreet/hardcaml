open! Core0
open Coverage_prim

module Config = struct
  type t =
    { hide_unstable : bool
    ; verbose : bool
    ; compact : bool
    }
  [@@deriving fields ~getters]
end

let indent_unit = 4

let output_line ?(indent = 0) oc string =
  let indent = List.init (indent * indent_unit) ~f:(fun _ -> ' ') |> String.of_list in
  Out_channel.output_line oc (indent ^ string)
;;

module Heading_level = struct
  type t =
    | H1
    | H2
    | H3

  let border_count = function
    | H1 -> 8
    | H2 -> 6
    | H3 -> 4
  ;;
end

let output_heading oc heading level =
  let border_count = Heading_level.border_count level in
  let border = List.init border_count ~f:(fun _ -> '=') |> String.of_list in
  output_line oc [%string {|%{border} %{heading} %{border}|}];
  Out_channel.newline oc
;;

module Coverage_stats = struct
  type t =
    { tested : int
    ; full : int
    }

  let compute (type a) (module M : Coverage.S with type t = a) coverage =
    { tested = List.length coverage
    ; full = List.count coverage ~f:(fun c -> M.fully_covered c)
    }
  ;;

  let percent { tested; full } = Percent.of_mult (Int.to_float full /. Int.to_float tested)

  let print_precent oc t ~name =
    if t.tested <> 0 then output_line oc [%string {|%{name}: %{(percent t)#Percent}|}]
  ;;

  let print_full oc t ~name ~unit =
    print_precent oc t ~name;
    output_line oc [%string {|%{unit} tested: %{t.tested#Int}|}];
    output_line oc [%string {|%{unit} with full coverage: %{t.full#Int}|}]
  ;;
end

let output_list oc list ~to_string ~title ~indent =
  let list_to_string l = List.map l ~f:to_string |> String.concat ~sep:" " in
  if not (List.length list = 0)
  then (
    let string_list = list_to_string list in
    let line_length =
      String.length string_list + (indent * indent_unit) + String.length title
    in
    if line_length < 85
    then output_line oc ~indent [%string {|%{title}: %{string_list}|}]
    else (
      let chunks = List.chunks_of list ~length:25 in
      output_line oc ~indent [%string {|%{title}:|}];
      List.iter chunks ~f:(fun chunk ->
        output_line oc ~indent:(indent + 1) (list_to_string chunk))))
;;

let output_list_lines oc list ~to_string ~title ~indent =
  if not (List.length list = 0) then output_line oc ~indent [%string {|%{title}:|}];
  List.iter list ~f:(fun a -> output_line oc ~indent:(indent + 1) (to_string a))
;;

let output_source_loc ?title oc config loc ~indent =
  match title with
  | None ->
    if not (Config.hide_unstable config)
    then output_line oc [%string {|%{loc#Source_code_position}|}] ~indent
  | Some title ->
    if not (Config.hide_unstable config)
    then output_line oc [%string {|%{title}: %{loc#Source_code_position}|}] ~indent
    else output_line oc [%string {|%{title}: <elided>|}] ~indent
;;

let output_names ?(indent = 0) oc (names : Name_and_loc.t list) ~title =
  (* For now don't output the locations as well, it doesn't seem that helpful in addition
     to the name. *)
  match names with
  | [] -> ()
  | [ { name; loc = _ } ] -> output_line oc [%string {|%{title}: %{name}|}] ~indent
  | names ->
    output_line oc [%string {|%{title}:|}] ~indent;
    List.iter names ~f:(fun { name; loc = _ } ->
      output_line oc [%string {|%{name}|}] ~indent:(indent + 1))
;;

let report_call_stack oc config call_stack ~indent =
  if List.length call_stack > 0 && not (Config.hide_unstable config)
  then (
    output_line oc ~indent "call_stack:";
    List.iter call_stack ~f:(fun slot ->
      output_line oc (Call_stack.Slot.format slot) ~indent:(indent + 1)))
;;

let report_always oc config creation_pos (target : Always_metadata.Target.t) ~kind ~indent
  =
  output_line oc [%string {|always %{kind}|}] ~indent;
  output_source_loc oc config creation_pos ~title:"created at" ~indent:(indent + 1);
  let title, pos =
    match target.variable with
    | User_created pos -> "driving (always variable)", pos
    | State_machine_state { creation_pos; _ } -> "driving (state variable)", creation_pos
  in
  match target.names with
  | [] -> output_source_loc oc config pos ~title ~indent:(indent + 1)
  | names -> output_names oc names ~title ~indent:(indent + 1)
;;

let report_single_mux oc config mux ~indent =
  let%tydi { select_names; kind } = Signal_coverage.Mux.debug_info mux in
  (match kind with
   | Basic -> ()
   | If { creation_pos; target; kind } ->
     report_always
       oc
       config
       creation_pos
       target
       ~kind:(Always_metadata.If.Kind.to_string kind)
       ~indent
   | Switch { creation_pos; target; case } ->
     report_always
       oc
       config
       creation_pos
       target
       ~kind:[%string {|switch %{case#Case}|}]
       ~indent);
  output_names oc select_names ~title:"selector names" ~indent;
  let%tydi { observed; unexpectedly_observed; not_covered } =
    Signal_coverage.Mux.coverage mux
  in
  let to_string = Int.to_string in
  output_list oc ~indent ~title:"saw selector values of" observed ~to_string;
  output_list oc ~indent ~title:"never saw" not_covered ~to_string;
  output_list oc ~indent ~title:"!! unexpectedly saw" unexpectedly_observed ~to_string;
  Out_channel.newline oc
;;

let report_single_cases oc config cases ~indent =
  let%tydi { select_names; kind } = Signal_coverage.Cases.debug_info cases in
  (match kind with
   | Basic -> ()
   | Switch { creation_pos; target; _ } ->
     report_always oc config creation_pos target ~kind:"switch" ~indent);
  output_names oc select_names ~title:"selector names" ~indent;
  let%tydi { observed; unexpectedly_observed; not_covered } =
    Signal_coverage.Cases.coverage cases
  in
  let to_string = Case.Positional_with_state.to_string in
  output_list oc ~indent ~title:"selector matched cases" observed ~to_string;
  output_list oc ~indent ~title:"never matched" not_covered ~to_string;
  output_list oc ~indent ~title:"!! unexpectedly saw" unexpectedly_observed ~to_string;
  Out_channel.newline oc
;;

let report_single_reg oc _config reg ~indent =
  let bits = Signal_coverage.Reg.bits reg in
  output_line oc ~indent [%string {|width: %{bits#Int}|}];
  let report_toggle ~on =
    let%tydi { observed; unexpectedly_observed; not_covered } =
      Signal_coverage.Reg.coverage reg
      |> Signal_coverage.filter_coverage ~f:(fun { on = is_on; _ } -> Bool.equal is_on on)
    in
    let to_string (toggle : Toggle.t) = Int.to_string toggle.bit in
    let state = if on then "ON" else "OFF" in
    output_list oc ~indent observed ~title:[%string {|bits toggled %{state}|}] ~to_string;
    output_list
      oc
      ~indent
      not_covered
      ~title:[%string {|bits never toggled %{state}|}]
      ~to_string;
    output_list
      oc
      ~indent
      ~title:[%string {|!! unexpectedly toggled %{state}|}]
      unexpectedly_observed
      ~to_string
  in
  report_toggle ~on:true;
  report_toggle ~on:false;
  Out_channel.newline oc
;;

let report_single_always_state oc _config state ~indent =
  let%tydi { observed; unexpectedly_observed; not_covered } =
    Signal_coverage.Always_state.coverage state
  in
  let to_string = Transition.State.to_string in
  output_list_lines oc ~indent ~title:"saw transitions" observed ~to_string;
  output_list_lines oc ~indent ~title:"never saw" not_covered ~to_string;
  output_list oc ~indent ~title:"!! unexpectedly saw" unexpectedly_observed ~to_string;
  Out_channel.newline oc
;;

let report_group_coverage
  (type a)
  oc
  config
  (module M : Signal_coverage.S with type t = a)
  (coverage : a list)
  report_single
  ~name
  ~unit
  =
  if List.length coverage > 0
  then (
    if not (Config.compact config)
    then (
      output_heading oc [%string {|%{name} coverage|}] H3;
      let stats = Coverage_stats.compute (module M) coverage in
      Coverage_stats.print_full oc stats ~name:"total" ~unit;
      Out_channel.newline oc);
    let to_report =
      List.filter_map coverage ~f:(fun coverage ->
        if Config.verbose config
           || (not (M.fully_covered coverage))
           || M.unexpectedly_observed_cases coverage <> 0
        then Some coverage
        else None)
    in
    let to_report =
      if Config.hide_unstable config
      then
        List.sort to_report ~compare:(fun a b ->
          Signal_graph.Normalized_signal_uid.compare (M.id a) (M.id b))
      else to_report
    in
    if List.length to_report > 0
    then
      List.iter to_report ~f:(fun c ->
        output_line
          oc
          [%string {|%{name} with id: %{(M.id c)#Signal_graph.Normalized_signal_uid}|}];
        (match M.signal_names c with
         | [] -> report_call_stack oc config (M.call_stack c) ~indent:1
         | names -> output_names oc names ~title:"names" ~indent:1);
        report_single oc config c ~indent:1))
;;

let report_coverage_by_group oc config (coverage : Signal_coverage.Grouped.t) =
  let%tydi { muxes; cases; regs; always_states } = coverage in
  report_group_coverage
    oc
    config
    (module Signal_coverage.Mux)
    muxes
    ~unit:"muxes"
    ~name:"Mux"
    report_single_mux;
  report_group_coverage
    oc
    config
    (module Signal_coverage.Cases)
    cases
    ~unit:"cases"
    ~name:"Cases"
    report_single_cases;
  report_group_coverage
    oc
    config
    (module Signal_coverage.Reg)
    regs
    ~unit:"regs"
    ~name:"Reg"
    report_single_reg;
  report_group_coverage
    oc
    config
    (module Signal_coverage.Always_state)
    always_states
    ~unit:"always states"
    ~name:"Always state"
    report_single_always_state
;;

let report_group_totals oc (coverage : Signal_coverage.Grouped.t) =
  let report_total
    (type a)
    (module M : Coverage.S with type t = a)
    (coverage : a list)
    ~name
    =
    let stats = Coverage_stats.compute (module M) coverage in
    Coverage_stats.print_precent oc stats ~name
  in
  let%tydi { muxes; cases; regs; always_states } = coverage in
  report_total (module Signal_coverage.Mux) muxes ~name:"muxes";
  report_total (module Signal_coverage.Cases) cases ~name:"cases";
  report_total (module Signal_coverage.Reg) regs ~name:"regs";
  report_total (module Signal_coverage.Always_state) always_states ~name:"always_states"
;;

let report_circuit_instance
  oc
  config
  (instance : Circuit_coverage.Instance.t)
  ~count
  ~indent
  =
  report_call_stack oc config instance.call_stack ~indent;
  if count > 1 then output_line oc ~indent [%string {|creation count: %{count#Int}|}];
  Out_channel.newline oc
;;

let report_circuit_coverage oc config coverage =
  if Config.verbose config
     || (not (Circuit_coverage.fully_covered coverage))
     || Circuit_coverage.unexpectedly_observed_cases coverage <> 0
  then (
    let signal_coverage = Circuit_coverage.signal_coverage coverage in
    let grouped = Signal_coverage.Grouped.of_flat signal_coverage in
    if not (Config.compact config)
    then (
      let name = Circuit_coverage.name coverage in
      output_heading oc [%string {|Circuit coverage for %{name}|}] H2;
      let signal_coverage_stats =
        Coverage_stats.compute (module Signal_coverage) signal_coverage
      in
      Coverage_stats.print_precent oc signal_coverage_stats ~name:"total coverage";
      report_group_totals oc grouped;
      let instances = Circuit_coverage.instance_counts coverage in
      match instances with
      | [ (instance, count) ] ->
        report_circuit_instance oc config instance ~count ~indent:0
      | _ ->
        output_line oc "Instances:";
        List.iter instances ~f:(fun (instance, count) ->
          report_circuit_instance oc config instance ~count ~indent:1));
    report_coverage_by_group oc config grouped)
;;

let write oc coverage ~hide_unstable ~verbose ~compact =
  let config = { Config.hide_unstable; verbose; compact } in
  if not (Config.compact config)
  then (
    output_heading oc [%string {|Total coverage|}] H1;
    let stats = Coverage_stats.compute (module Circuit_coverage) coverage in
    Coverage_stats.print_full oc stats ~name:"total" ~unit:"circuits";
    Out_channel.newline oc);
  List.iter coverage ~f:(fun coverage -> report_circuit_coverage oc config coverage)
;;
