open Core

let indent_unit = 4

let output_line_with_indent oc string ~indent =
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
  Out_channel.output_line oc [%string {|%{border} %{heading} %{border}|}];
  Out_channel.newline oc
;;

module Coverage_stats = struct
  type t =
    { tested : int
    ; full : int
    }

  let compute (type a) (module M : Coverage.S with type t = a) coverage ~f =
    { tested = List.length coverage
    ; full = List.count coverage ~f:(fun c -> M.fully_covered (f c))
    }
  ;;

  let percent { tested; full } = Percent.of_mult (Int.to_float full /. Int.to_float tested)

  let print_precent oc t ~name =
    if t.tested <> 0
    then Out_channel.output_line oc [%string {|%{name}: %{(percent t)#Percent}|}]
  ;;

  let print_full oc t ~name ~unit =
    print_precent oc t ~name;
    Out_channel.output_line oc [%string {|%{unit} tested: %{t.tested#Int}|}];
    Out_channel.output_line oc [%string {|%{unit} with full coverage: %{t.full#Int}|}]
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
    then output_line_with_indent oc ~indent [%string {|%{title}: %{string_list}|}]
    else (
      let chunks = List.chunks_of list ~length:25 in
      output_line_with_indent oc ~indent [%string {|%{title}:|}];
      List.iter chunks ~f:(fun chunk ->
        output_line_with_indent oc ~indent:(indent + 1) (list_to_string chunk))))
;;

let output_names oc (names : Name_and_loc.t list) ~title ~indent =
  match names with
  | [] -> ()
  | names ->
    output_line_with_indent oc [%string {|%{title}:|}] ~indent;
    List.iter names ~f:(fun { name; loc } ->
      output_line_with_indent oc [%string {|%{name}|}] ~indent:(indent + 1);
      output_line_with_indent
        oc
        [%string {|%{loc#Source_code_position}|}]
        ~indent:(indent + 2))
;;

let report_call_stack oc slots ~indent =
  List.iter slots ~f:(fun slot ->
    output_line_with_indent oc (Stack_slot.format slot) ~indent)
;;

let report_single_mux
  oc
  (mux : Signal_coverage.Mux.t Signal_coverage.with_metadata)
  ~indent
  =
  let mux = mux.data in
  let covered, not_covered =
    List.range 0 (Signal_coverage.Mux.case_count mux)
    |> List.partition_tf ~f:(fun case -> Signal_coverage.Mux.case_covered mux case)
  in
  output_names oc (Signal_coverage.Mux.select_names mux) ~title:"selector names" ~indent;
  output_list oc ~indent ~title:"saw selector values of" covered ~to_string:Int.to_string;
  output_list oc ~indent ~title:"never saw" not_covered ~to_string:Int.to_string;
  Out_channel.newline oc
;;

let report_single_cases
  oc
  (cases : Signal_coverage.Cases.t Signal_coverage.with_metadata)
  ~indent
  =
  let cases = cases.data in
  let covered, not_covered =
    Signal_coverage.Cases.cases cases
    |> List.partition_tf ~f:(fun case -> Signal_coverage.Cases.case_covered cases case)
  in
  output_names
    oc
    (Signal_coverage.Cases.select_names cases)
    ~title:"selector names"
    ~indent;
  let to_string = Signal_coverage.Cases.Case.to_string in
  output_list oc ~indent ~title:"saw selector values of" covered ~to_string;
  output_list oc ~indent ~title:"never saw" not_covered ~to_string;
  Out_channel.newline oc
;;

let report_single_reg
  oc
  (reg : Signal_coverage.Reg.t Signal_coverage.with_metadata)
  ~indent
  =
  let reg = reg.data in
  let bits = Signal_coverage.Reg.bits reg in
  output_line_with_indent oc ~indent [%string {|width: %{bits#Int}|}];
  let report_toggle ~on =
    let toggled, not_toggled =
      List.range 0 bits
      |> List.partition_tf ~f:(fun bit -> Signal_coverage.Reg.bit_toggled reg ~bit ~on)
    in
    let state = if on then "ON" else "OFF" in
    output_list
      oc
      ~indent
      toggled
      ~title:[%string {|bits toggled %{state}|}]
      ~to_string:Int.to_string;
    output_list
      oc
      ~indent
      not_toggled
      ~title:[%string {|bits never toggled %{state}|}]
      ~to_string:Int.to_string
  in
  report_toggle ~on:true;
  report_toggle ~on:false;
  Out_channel.newline oc
;;

let report_shared_coverage
  oc
  (debug_info : Signal_coverage.Debug_info.t)
  (coverage : _ Signal_coverage.with_metadata list)
  ~name
  report_single
  =
  Out_channel.output_line oc [%string {|%{name}:|}];
  let%tydi { names; call_stack; comment } = debug_info in
  output_names oc names ~title:"names" ~indent:1;
  (match call_stack with
   | [] -> ()
   | call_stack ->
     output_line_with_indent oc [%string {|call stack:|}] ~indent:1;
     report_call_stack oc call_stack ~indent:2);
  Option.iter comment ~f:(fun comment -> output_line_with_indent oc comment ~indent:1);
  Out_channel.newline oc;
  List.iter coverage ~f:(fun c ->
    output_line_with_indent
      oc
      [%string {|coverage for id: %{c.id#Signal_graph.Normalized_signal_uid}|}]
      ~indent:1;
    report_single oc c ~indent:2)
;;

let report_group_coverage
  (type a)
  oc
  (module M : Coverage.S with type t = a)
  (coverage : a Signal_coverage.with_metadata list)
  report_signal
  ~name
  ~unit
  ~verbose
  =
  if List.length coverage > 0
  then (
    output_heading oc [%string {|%{name} coverage|}] H3;
    let stats = Coverage_stats.compute (module M) coverage ~f:(fun { data; _ } -> data) in
    Coverage_stats.print_full oc stats ~name:"total" ~unit;
    Out_channel.newline oc;
    let by_debug_info = Hashtbl.create (module Signal_coverage.Debug_info) in
    List.iter coverage ~f:(fun coverage ->
      Hashtbl.update by_debug_info coverage.debug_info ~f:(function
        | Some coverage_list -> coverage :: coverage_list
        | None -> [ coverage ]));
    Hashtbl.iteri by_debug_info ~f:(fun ~key:debug_info ~data:coverage_list ->
      let to_report =
        List.filter_map coverage_list ~f:(fun coverage ->
          if verbose || not (M.fully_covered coverage.data) then Some coverage else None)
      in
      if List.length to_report > 0
      then report_shared_coverage oc debug_info to_report ~name report_signal);
    Out_channel.newline oc)
;;

let report_coverage_by_group oc (coverage : Signal_coverage.Grouped.t) ~verbose =
  let%tydi { muxes; cases; regs } = coverage in
  report_group_coverage
    oc
    (module Signal_coverage.Mux)
    muxes
    ~unit:"muxes"
    ~name:"Mux"
    report_single_mux
    ~verbose;
  report_group_coverage
    oc
    (module Signal_coverage.Cases)
    cases
    ~unit:"cases"
    ~name:"Cases"
    report_single_cases
    ~verbose;
  report_group_coverage
    oc
    (module Signal_coverage.Reg)
    regs
    ~unit:"regs"
    ~name:"Reg"
    report_single_reg
    ~verbose
;;

let report_group_totals oc (coverage : Signal_coverage.Grouped.t) =
  let report_total
    (type a)
    (module M : Coverage.S with type t = a)
    (coverage : a Signal_coverage.with_metadata list)
    ~name
    =
    let stats = Coverage_stats.compute (module M) coverage ~f:(fun { data; _ } -> data) in
    Coverage_stats.print_precent oc stats ~name
  in
  let%tydi { muxes; cases; regs } = coverage in
  report_total (module Signal_coverage.Mux) muxes ~name:"muxes";
  report_total (module Signal_coverage.Cases) cases ~name:"cases";
  report_total (module Signal_coverage.Reg) regs ~name:"regs"
;;

let report_circuit_instance oc (instance : Circuit_coverage.Instance.t) ~count ~indent =
  output_line_with_indent oc ~indent "call_stack:";
  report_call_stack oc instance.call_stack ~indent:(indent + 1);
  if count > 1
  then output_line_with_indent oc ~indent [%string {|creation count: %{count#Int}|}];
  Out_channel.newline oc
;;

let report_circuit_coverage oc coverage ~verbose =
  if verbose || not (Circuit_coverage.fully_covered coverage)
  then (
    let name = Circuit_coverage.name coverage in
    output_heading oc [%string {|Circuit coverage for %{name}|}] H2;
    let signal_coverage = Circuit_coverage.signal_coverage coverage in
    let signal_coverage_stats =
      Coverage_stats.compute (module Signal_coverage) signal_coverage ~f:Fn.id
    in
    Coverage_stats.print_precent oc signal_coverage_stats ~name:"total coverage";
    let grouped = Signal_coverage.Grouped.of_flat signal_coverage in
    report_group_totals oc grouped;
    Out_channel.newline oc;
    let instances = Circuit_coverage.instance_counts coverage in
    (match instances with
     | [ (instance, count) ] -> report_circuit_instance oc instance ~count ~indent:0
     | _ ->
       Out_channel.output_line oc "Instances:";
       List.iter instances ~f:(fun (instance, count) ->
         report_circuit_instance oc instance ~count ~indent:1));
    report_coverage_by_group oc grouped ~verbose)
;;

let write oc coverage ~verbose =
  output_heading oc [%string {|Total coverage|}] H1;
  let stats = Coverage_stats.compute (module Circuit_coverage) coverage ~f:Fn.id in
  Coverage_stats.print_full oc stats ~name:"total" ~unit:"circuits";
  Out_channel.newline oc;
  List.iter coverage ~f:(fun coverage -> report_circuit_coverage oc coverage ~verbose)
;;
