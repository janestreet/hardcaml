open! Import
open Printf
open Signal

(* write a DOT file with rank information - looks absolutely terrible *)
let write_dot_rank chan circuit =
  (* {[
       let get_name signal =
         (* {[
              match names signal with
              | [] -> "_" ^ Int64.to_string (uid signal)
              | h :: t -> h
            ]} *)
         Int64.to_string (uid signal)
       in
     ]} *)
  let outputs =
    List.fold
      (Circuit.outputs circuit)
      ~init:(Set.empty (module Uid))
      ~f:(fun set signal -> Set.add set (uid signal))
  in
  (* create a suitable fan-out mapping *)
  let fdeps s =
    match s with
    | Mem _ -> [ List.hd_exn (deps s) ]
    | Reg _ -> [ List.hd_exn (deps s) ]
    | _ -> deps s
  in
  let fan_out = Signal_graph.fan_out_map ~deps:fdeps (Circuit.signal_graph circuit) in
  let find_fan_out signal =
    match Map.find fan_out signal with
    | None -> []
    | Some s -> Set.to_list s
  in
  let uids l = List.map l ~f:uid in
  (* We start at the inputs, and traverse forward to the outputs, effectively using depth
     first search. *)
  let rec dfs visited ranks signals =
    match signals with
    | [] -> ranks
    | _ ->
      (* add these signals to the visited set *)
      let visited =
        List.fold signals ~init:visited ~f:(fun set signal -> Set.add set signal)
      in
      (* find fan_out from this level *)
      let signals =
        List.concat (List.map signals ~f:(fun signal -> find_fan_out signal))
      in
      (* filter out already visited signals, and outputs *)
      let signals =
        List.filter signals ~f:(fun signal ->
          (not (Set.mem visited signal)) && not (Set.mem outputs signal))
      in
      (* create set of uids of nodes at this rank *)
      let rank =
        List.fold
          signals
          ~init:(Set.empty (module Uid))
          ~f:(fun set signal -> Set.add set signal)
      in
      if Set.is_empty rank then ranks else dfs visited (rank :: ranks) signals
  in
  let ranks =
    dfs (Set.empty (module Uid)) [] (Circuit.inputs circuit |> uids)
    |> List.map ~f:Set.to_list
  in
  (* create the output level and add it to the ranks *)
  let ranks =
    List.map (Circuit.outputs circuit) ~f:(fun s -> uid s) :: ranks |> List.rev
  in
  let nranks = List.length ranks in
  (* write the bit to the left *)
  fprintf chan "digraph %s {\n" (Circuit.name circuit);
  for i = 0 to nranks - 1 do
    fprintf chan "%i" i;
    if i <> nranks - 1 then fprintf chan " -> "
  done;
  fprintf chan "\n";
  List.iteri ranks ~f:(fun i s ->
    fprintf chan " { rank=same; %i [shape=plaintext];\n" i;
    List.iter s ~f:(fun s -> fprintf chan "  _%Li;\n" s);
    fprintf chan "}\n");
  (* write edges *)
  Signal_graph.iter (Circuit.signal_graph circuit) ~f:(fun s ->
    List.iter (fdeps s) ~f:(fun d -> fprintf chan "_%Li -> _%Li;\n" (uid d) (uid s)));
  fprintf chan "}\n"
;;

(* GDL file with manhatten layout - looks much, much nicer *)
let write_gdl
      ?(names = false)
      ?(widths = false)
      ?(consts = true)
      ?(clocks = false)
      chan
      circuit
  =
  let quote s = "\"" ^ s ^ "\"" in
  fprintf chan "graph: {\n";
  let props =
    [ "title", quote (Circuit.name circuit)
    ; "manhattenedges", "yes"
    ; "inportsharing", "no"
    ; "outportsharing", "yes"
    ; "node.bordercolor", "lightblue"
    ]
  in
  let props = if widths then ("display_edge_labels", "yes") :: props else props in
  (* write list of default attributes *)
  List.iter props ~f:(fun (a, b) -> fprintf chan "%s: %s\n" a b);
  let folds c s =
    List.fold s ~init:"" ~f:(fun s n -> if String.is_empty s then n else n ^ c ^ s)
  in
  let name s =
    let names = Signal.names s in
    match names with
    | [] -> ""
    | [ h ] -> h
    | h :: t -> h ^ " (" ^ folds "," t ^ ")"
  in
  let write_node
        ?(border = "invisible")
        ?(shape = "box")
        ?(label = "")
        ?(bordercolour = "")
        ?(colour = "")
        ?(textcolour = "")
        signal
    =
    fprintf chan "node: { title: \"%Li\" " (uid signal);
    let name = if String.is_empty label || names then name signal else "" in
    (match label, name with
     | "", "" -> fprintf chan "label: \"none\" "
     | _, "" -> fprintf chan "label: \"%s\" " label
     | "", _ -> fprintf chan "label: \"\\fI%s\" " name
     | _ -> fprintf chan "label: \"%s\\n\\fI%s\" " label name);
    fprintf chan "shape: %s " shape;
    fprintf chan "borderstyle: %s " border;
    if not (String.is_empty textcolour) then fprintf chan "textcolor: %s " textcolour;
    if not (String.is_empty bordercolour)
    then fprintf chan "bordercolor: %s " bordercolour;
    if not (String.is_empty colour) then fprintf chan "color: %s " colour;
    fprintf chan " }\n"
  in
  let is_rom s =
    match s with
    | Mux _ ->
      List.fold (List.tl_exn (deps s)) ~init:true ~f:(fun b s -> b && is_const s)
    | _ -> false
  in
  let reg_deps s =
    match s with
    | Reg { register = r; d; _ } ->
      (if clocks then [ r.reg_clock ] else []) @ [ d; r.reg_enable ]
    | _ -> []
  in
  let mem_deps s =
    match s with
    | Mem { register = r; memory = m; _ } ->
      [ m.mem_write_data; r.reg_enable; m.mem_read_address; m.mem_write_address ]
    | Multiport_mem { write_ports; _ } ->
      Array.map write_ports ~f:(fun wr ->
        [ wr.write_clock; wr.write_enable; wr.write_address; wr.write_data ])
      |> Array.to_list
      |> List.concat
    | _ -> []
  in
  let is_input s = Circuit.is_input circuit s in
  let is_output s = Circuit.is_output circuit s in
  (* write nodes *)
  let write_node s =
    match s with
    | Empty -> write_node ~label:"empty" s
    | Const { constant; _ } ->
      write_node
        ~label:(Bits.to_constant constant |> Constant.to_hex_string ~signedness:Unsigned)
        s
    | Wire _ ->
      if List.is_empty (Signal.names s)
      then write_node ~textcolour:"lightgrey" ~label:"wire" s
      else if is_input s
      then write_node ~textcolour:"red" s
      else if is_output s
      then write_node ~textcolour:"red" s
      else write_node ~textcolour:"lightgrey" s
    | Select { high; low; _ } ->
      write_node ~textcolour:"lightgrey" ~label:(sprintf "[%i:%i]" high low) s
    | Op2 { op; _ } ->
      (match op with
       | Signal_add -> write_node ~border:"solid" ~shape:"circle" ~label:"+" s
       | Signal_sub -> write_node ~border:"solid" ~shape:"circle" ~label:"-" s
       | Signal_mulu -> write_node ~border:"solid" ~shape:"circle" ~label:"*" s
       | Signal_muls -> write_node ~border:"solid" ~shape:"circle" ~label:"*+" s
       | Signal_and -> write_node ~border:"solid" ~shape:"circle" ~label:"&" s
       | Signal_or -> write_node ~border:"solid" ~shape:"circle" ~label:"|" s
       | Signal_xor -> write_node ~border:"solid" ~shape:"circle" ~label:"^" s
       | Signal_eq -> write_node ~border:"solid" ~shape:"circle" ~label:"=" s
       | Signal_lt -> write_node ~border:"solid" ~shape:"circle" ~label:"<" s)
    | Not _ -> write_node ~border:"solid" ~shape:"circle" ~label:"~" s
    | Mux _ ->
      if is_rom s
      then (
        let els = List.length (deps s) - 1 in
        write_node ~border:"solid" ~shape:"box" ~label:(sprintf "rom%i" els) s)
      else write_node ~border:"solid" ~shape:"uptrapeze" ~label:"mux" s
    | Cat _ -> write_node ~border:"solid" ~shape:"trapeze" ~label:"cat" s
    | Reg _ ->
      write_node
        ~bordercolour:"lightblue"
        ~textcolour:"white"
        ~colour:"black"
        ~border:"solid"
        ~label:"reg"
        s
    | Mem { memory = m; _ } ->
      write_node
        ~bordercolour:"lightblue"
        ~textcolour:"white"
        ~colour:"black"
        ~border:"solid"
        ~label:(sprintf "mem%i" m.mem_size)
        s
    | Multiport_mem { size; _ } ->
      write_node
        ~bordercolour:"lightblue"
        ~textcolour:"white"
        ~colour:"black"
        ~border:"solid"
        ~label:(sprintf "mem%i" size)
        s
    | Mem_read_port _ ->
      write_node
        ~bordercolour:"lightblue"
        ~textcolour:"white"
        ~colour:"black"
        ~border:"solid"
        ~label:"mem_rdp"
        s
    | Inst { instantiation; _ } ->
      write_node ~border:"solid" ~label:(sprintf "inst\n%s" instantiation.inst_name) s
  in
  (* specialised dependancies *)
  let deps s =
    if is_rom s
    then [ List.hd_exn (deps s) ]
    else if is_reg s
    then reg_deps s
    else if is_mem s
    then mem_deps s
    else deps s
  in
  (* write edges *)
  let write_edges () =
    Signal_graph.depth_first_search
      (Circuit.signal_graph circuit)
      ~init:(Set.empty (module Uid))
      ~f_before:(fun a s ->
        let deps = deps s |> List.filter ~f:(fun t -> not (is_empty t)) in
        let deps =
          if consts then deps else deps |> List.filter ~f:(fun s -> not (is_const s))
        in
        if (not (List.is_empty deps)) && not (is_empty s)
        then (
          List.iter deps ~f:(fun d ->
            (* Note; labels always specified, even if they are disabled *)
            fprintf chan "edge: { source: \"%Li\" target: \"%Li\" " (uid d) (uid s);
            if (is_wire s && not (is_output s)) || is_select s
            then fprintf chan "arrowstyle: none ";
            fprintf chan "color:lightgrey thickness: 1 label: \"%i\" }\n" (width d));
          List.fold (s :: deps) ~init:a ~f:(fun a s -> Set.add a (uid s)))
        else a)
  in
  let nodes = write_edges () in
  Set.iter nodes ~f:(fun u -> write_node (Circuit.find_signal_exn circuit u));
  fprintf chan "}\n"
;;

let aisee3
      ?(args = "")
      ?(names = false)
      ?(widths = false)
      ?(consts = true)
      ?(clocks = false)
      circuit
  =
  let name, file = Filename.open_temp_file "aisee3" ".gdl" in
  write_gdl ~names ~widths ~consts ~clocks file circuit;
  Out_channel.close file;
  ignore (Unix.open_process_in ("aisee3 " ^ name ^ " " ^ args) : Stdio.In_channel.t)
;;
