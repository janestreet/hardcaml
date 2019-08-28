open! Import

module Entry = struct
  type t =
    { name : string (* actual circuit name *)
    ; mangled_name : string
    ; circuit : Circuit.t
    }
  [@@deriving fields]
end

type t =
  { mangler : Mangler.t
  ; entry_by_mangled_name : (string, Entry.t) Hashtbl.t
  }

(* Show a summary of the name mapping *)
let sexp_of_t t =
  let mapping =
    List.map (Hashtbl.data t.entry_by_mangled_name) ~f:(fun e -> e.name, e.mangled_name)
    |> List.sort ~compare:[%compare: string * string]
  in
  [%sexp (mapping : (string * string) list)]
;;

let create () =
  (* Choose caseless mangling and add all reserved words so module names are compatible
     with VHDL and Verilog. *)
  let mangler = Mangler.create ~case_sensitive:false in
  List.iter
    Reserved_words.(verilog @ vhdl)
    ~f:(fun r -> ignore (Mangler.add_identifier mangler r : [ `Ok | `Duplicate ]));
  { mangler; entry_by_mangled_name = Hashtbl.create (module String) }
;;

let add t circuit =
  let name = Circuit.name circuit in
  let mangled_name = Mangler.mangle t.mangler name in
  let entry =
    { Entry.name; mangled_name; circuit = Circuit.with_name circuit ~name:mangled_name }
  in
  Hashtbl.add_exn t.entry_by_mangled_name ~key:mangled_name ~data:entry;
  mangled_name
;;

let add_if_unique t circuit =
  (* find the existing circuits which started off with this (un-mangled) name *)
  match
    List.find (Hashtbl.data t.entry_by_mangled_name) ~f:(fun entry ->
      String.equal entry.name (Circuit.name circuit)
      && Circuit.structural_compare entry.circuit circuit)
  with
  | Some e -> e.mangled_name
  | None -> add t circuit
;;

let insert ?(share = true) database circuit =
  if share then add_if_unique database circuit else add database circuit
;;

let find t ~mangled_name =
  Hashtbl.find t.entry_by_mangled_name mangled_name |> Option.map ~f:Entry.circuit
;;

let get_circuits (t : t) =
  List.map (Hashtbl.data t.entry_by_mangled_name) ~f:(fun (e : Entry.t) -> e.circuit)
;;
