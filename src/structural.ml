(* We have the following object types;

   - MI module input {R}
   - MO module output {W}
   - MT module tristate {RW}

   - SI submodule input [<-MI, MT, W, C]
   - SO submodule output [->MO, MT, W]
   - ST submodule tristate [<-MI, MT, W, C ->MO, MT, W]

   - W wires {RW} [<-MI, MT, W, C -> MO, MT, W]
   - C constants {R} [->C, MO, MT]

   Basic rules;

   - module IO's may connect to submodules
   - submodulues connect via wires or module IOs
   - constants are currently included but are not needed - might be better to
     detect them at generation time instead - similar for other simple RTL
     operators.

   Multiple {RW}

   - many things may read an object
   - in general only 1 thing may drive (write) and object
   - special case - multiple tristates may drive an object

   NOTE: It would be nice if many of the rules below could be encoded into the
   type system, but I dont know how or if it's possible. *)

open Base
include Structural_intf

type name = string
type id = int
type width = int

type signal =
  | Empty
  (* module interface *)
  | Module_input of id * name * width * Rtl_attribute.t list ref
  | Module_output of id * name * width * signal ref * Rtl_attribute.t list ref
  | Module_tristate of id * name * width * signal list ref * Rtl_attribute.t list ref
  (* internal wires *)
  | Internal_wire of id * width * signal ref
  | Internal_triwire of id * width * signal list ref
  (* instnatiations *)
  | Instantiation_output of id * name (* reference to instantiation *)
  | Instantiation_tristate of id * name
  | Instantiation of
      id
      * name
      * (string * generic) list
      * (string * signal) list
      (* inputs (read) *)
      * (string * signal) list
      (* outputs (write; drive wires/module outputs *)
      * (string * signal) list (* tristate (write; drive triwires/module tristates *)
      * string option
      * Rtl_attribute.t list
  (* basic RTL operators *)
  | Rtl_op of id * width * rtl_op

and rtl_op =
  | Constant of string
  | Select of int * int * signal
  | Concat of signal list
  | Mux of signal * signal list

and generic =
  | GInt of int
  | GFloat of float
  | GString of string
  | GUnquoted of string

type circuit =
  { name : string
  ; id : id
  ; mutable signals : signal list
  }

exception Invalid_submodule_input_connection of string * string * signal
exception Invalid_submodule_output_connection of string * string * signal
exception Invalid_submodule_tristate_connection of string * string * signal
exception Wire_already_assigned of signal
exception Invalid_assignment_target of signal
exception Cant_assign_wire_with of signal
exception Cant_assign_triwire_with of signal
exception Invalid_name of signal
exception Invalid_width of signal
exception Invalid_id of signal
exception Invalid_constant of string
exception Rtl_op_arg_not_readable of signal
exception Too_few_mux_data_elements
exception Too_many_mux_data_elements of int
exception All_mux_data_elements_must_be_same_width of int list
exception No_elements_to_concat
exception Select_index_error of int * int
exception Binop_arg_widths_different of string
exception No_circuit
exception Circuit_already_started
exception Circuit_already_exists of string
exception IO_name_already_exists of string

let signal_is_empty = function
  | Empty -> true
  | _ -> false
;;

let initial_map () = Map.empty (module String)

(* state *)
let id' = ref 0
let const_map = ref (initial_map ())
let circuit' = ref None
let circuits' = ref []
let instance_names = Hash_set.create (module String)

let reset_circuit_database () =
  id' := 0;
  const_map := initial_map ();
  circuit' := None;
  circuits' := [];
  Hash_set.clear instance_names
;;

let id () =
  let id = !id' in
  Int.incr id';
  id
;;

let get_id = function
  | Empty -> -1 (* less than any possible created id *)
  | Module_input (id, _, _, _)
  | Module_output (id, _, _, _, _)
  | Module_tristate (id, _, _, _, _)
  | Instantiation (id, _, _, _, _, _, _, _)
  | Internal_triwire (id, _, _)
  | Internal_wire (id, _, _)
  | Rtl_op (id, _, _)
  | Instantiation_output (id, _)
  | Instantiation_tristate (id, _) -> id
;;

let signal_equal (s1 : signal) s2 = Int.equal (get_id s1) (get_id s2)

let name t =
  match t with
  | Module_input (_, name, _, _)
  | Module_output (_, name, _, _, _)
  | Module_tristate (_, name, _, _, _)
  | Instantiation (_, name, _, _, _, _, _, _) -> name
  | Internal_triwire (id, _, _) | Internal_wire (id, _, _) | Rtl_op (id, _, _) ->
    "_" ^ Int.to_string id
  | Empty | Instantiation_output _ | Instantiation_tristate _ -> raise (Invalid_name t)
;;

let width t =
  match t with
  | Module_input (_, _, width, _)
  | Module_output (_, _, width, _, _)
  | Module_tristate (_, _, width, _, _)
  | Internal_triwire (_, width, _)
  | Internal_wire (_, width, _)
  | Rtl_op (_, width, _) -> width
  | Empty | Instantiation _ | Instantiation_output _ | Instantiation_tristate _ ->
    raise (Invalid_width t)
;;

let empty name = { name; id = id (); signals = [] }

let circuit_exists name =
  match List.Assoc.find !circuits' name ~equal:String.equal with
  | Some _ -> true
  | _ -> false
;;

let start_circuit name =
  match !circuit' with
  | Some _ -> raise Circuit_already_started
  | None ->
    if circuit_exists name
    then raise (Circuit_already_exists name)
    else (
      Hash_set.clear instance_names;
      id' := 0;
      const_map := initial_map ();
      circuit' := Some (empty name))
;;

let check_unique_io_names c =
  let ios, _ =
    List.partition_tf c.signals ~f:(function
      | Module_input _ | Module_output _ | Module_tristate _ -> true
      | _ -> false)
  in
  ignore
    (List.fold
       ios
       ~init:(Set.empty (module String))
       ~f:(fun set io ->
         let name = name io in
         if Set.mem set name
         then raise (IO_name_already_exists name)
         else Set.add set name)
      : Set.M(String).t);
  ()
;;

let end_circuit () =
  match !circuit' with
  | None -> raise No_circuit
  | Some x ->
    check_unique_io_names x;
    circuits' := (x.name, x) :: !circuits';
    circuit' := None
;;

let get_circuit () =
  match !circuit' with
  | None -> raise No_circuit
  | Some x -> x
;;

let find_circuit name = List.Assoc.find_exn !circuits' name ~equal:String.equal

let create_circuit name f =
  start_circuit name;
  f ();
  end_circuit ();
  find_circuit name
;;

let add_sig s =
  let c = get_circuit () in
  c.signals <- s :: c.signals;
  s
;;

let ( >> ) a b = b a
let mk_input name width = Module_input (id (), name, width, ref []) >> add_sig

let mk_output name width =
  Module_output (id (), name, width, ref Empty, ref []) >> add_sig
;;

let add_attribute signal attr =
  match signal with
  | Module_input (_, _, _, l)
  | Module_output (_, _, _, _, l)
  | Module_tristate (_, _, _, _, l) -> l := attr :: !l
  | _ ->
    raise_s
      [%message
        "Structural can only add_attribute on Module_input or Module_output for now"]
;;

let mk_tristate name width =
  Module_tristate (id (), name, width, ref [], ref []) >> add_sig
;;

let mk_wire width = Internal_wire (id (), width, ref Empty) >> add_sig
let mk_triwire width = Internal_triwire (id (), width, ref []) >> add_sig

let ( <== ) a b =
  match a with
  | Module_output (_, _, _, con, _) | Internal_wire (_, _, con) ->
    (match !con with
     | Empty ->
       (match b with
        | Internal_wire _ | Module_input _ | Instantiation_output _ | Rtl_op _ -> con := b
        | _ -> raise (Cant_assign_wire_with b))
     | _ -> raise (Wire_already_assigned a))
  | Module_tristate (_, _, _, con, _) | Internal_triwire (_, _, con) ->
    (match b with
     | Module_tristate _ | Internal_triwire _ | Instantiation_tristate _
     | Rtl_op (_, _, _)
     | Internal_wire _ -> con := b :: !con
     | _ -> raise (Cant_assign_triwire_with b))
  | _ -> raise (Invalid_assignment_target a)
;;

let is_readable = function
  | Module_input _ | Internal_wire _ | Rtl_op _ | Module_tristate _ | Internal_triwire _
    -> true
  | _ -> false
;;

let is_writeable = function
  | Module_output _ | Internal_wire _ -> true
  | _ -> false
;;

let is_readwrite = function
  | Module_tristate _ | Internal_triwire _ -> true
  | _ -> false
;;

let is_connected = function
  | Module_output (_, _, _, con, _) | Internal_wire (_, _, con) ->
    not (signal_is_empty !con)
  | Module_tristate (_, _, _, cons, _) | Internal_triwire (_, _, cons) ->
    not (List.is_empty !cons)
  | _ -> true
;;

let inst ?instance_name ?(attributes = []) ?(g = []) ?(i = []) ?(o = []) ?(t = []) name =
  let mod_id = id () in
  (* inputs: module inputs, wires *)
  List.iter i ~f:(fun (n, s) ->
    if not (is_readable s) then raise (Invalid_submodule_input_connection (name, n, s)));
  List.iter o ~f:(fun (n, s) ->
    if not (is_writeable s)
    then raise (Invalid_submodule_output_connection (name, n, s))
    else s <== Instantiation_output (mod_id, n));
  List.iter t ~f:(fun (n, s) ->
    if not (is_readwrite s)
    then raise (Invalid_submodule_tristate_connection (name, n, s))
    else s <== Instantiation_tristate (mod_id, n));
  Option.iter instance_name ~f:(fun instance_name ->
    if String.is_prefix instance_name ~prefix:"_"
    then raise_s [%message "explicitly specified instance_names cannot start with a '_'"];
    Hash_set.add instance_names instance_name);
  ignore
    (Instantiation (mod_id, name, g, i, o, t, instance_name, attributes) >> add_sig
      : signal)
;;

let ( ==> ) a b = a, b

let const' b =
  String.iter b ~f:(function
    | '0' | '1' | 'z' -> ()
    | _ -> raise (Invalid_constant b));
  if String.is_empty b then raise (Invalid_constant b);
  Rtl_op (id (), String.length b, Constant b) >> add_sig
;;

let of_bit_string b =
  match Map.find !const_map b with
  | Some x -> x
  | None ->
    let c = const' b in
    const_map := Map.set !const_map ~key:b ~data:c;
    c
;;

let z w = of_bit_string (String.init w ~f:(fun _ -> 'z'))

let check_readable l =
  let ok s = if not (is_readable s) then raise (Rtl_op_arg_not_readable s) in
  List.iter l ~f:ok
;;

let mux sel d =
  check_readable (sel :: d);
  let len = List.length d in
  if len < 1 then raise Too_few_mux_data_elements;
  if len > 1 lsl width sel then raise (Too_many_mux_data_elements len);
  let w = width (List.hd_exn d) in
  if not (List.fold d ~init:true ~f:(fun a s -> a && width s = w))
  then raise (All_mux_data_elements_must_be_same_width (List.map d ~f:width));
  Rtl_op (id (), w, Mux (sel, d)) >> add_sig
;;

let concat_msb d =
  check_readable d;
  if List.length d = 0 then raise No_elements_to_concat;
  Rtl_op (id (), List.fold d ~init:0 ~f:(fun a s -> a + width s), Concat d) >> add_sig
;;

let select d hi lo =
  check_readable [ d ];
  if hi < lo then raise (Select_index_error (hi, lo));
  if lo < 0 then raise (Select_index_error (hi, lo));
  if hi >= width d then raise (Select_index_error (hi, lo));
  Rtl_op (id (), hi - lo + 1, Select (hi, lo, d)) >> add_sig
;;

let prefix = "hardcaml_lib_"

(* Comb API *)
module Base (C : Config) = struct
  open C

  type t = signal

  let equal = signal_equal
  let empty = Empty

  let is_empty = function
    | Empty -> true
    | _ -> false
  ;;

  let width = width
  let wire = mk_wire
  let ( -- ) a _ = a

  let binop0 name a b =
    if width a <> width b then raise (Binop_arg_widths_different name);
    let o = wire 1 in
    inst
      (prefix ^ name)
      ~g:[ "b" ==> GInt (width a) ]
      ~i:[ "i0" ==> a; "i1" ==> b ]
      ~o:[ "o" ==> o ];
    o
  ;;

  let binop1 name a b =
    if width a <> width b then raise (Binop_arg_widths_different name);
    let o = wire (width a) in
    inst
      (prefix ^ name)
      ~g:[ "b" ==> GInt (width a) ]
      ~i:[ "i0" ==> a; "i1" ==> b ]
      ~o:[ "o" ==> o ];
    o
  ;;

  let binop2 name a b =
    let o = wire (width a + width b) in
    inst
      (prefix ^ name)
      ~g:[ "w0" ==> GInt (width a); "w1" ==> GInt (width b) ]
      ~i:[ "i0" ==> a; "i1" ==> b ]
      ~o:[ "o" ==> o ];
    o
  ;;

  let concat2 = binop2 "concat2"

  let s_concat_msb d =
    let rec cat2 = function
      | [] -> raise No_elements_to_concat
      | [ a ] -> a
      | h :: t -> concat2 h (cat2 t)
    in
    cat2 d
  ;;

  let concat_msb = if structural_concat then s_concat_msb else concat_msb

  let s_select d h l =
    let o = wire (h - l + 1) in
    inst
      (prefix ^ "select")
      ~g:[ "b" ==> GInt (width d); "h" ==> GInt h; "l" ==> GInt l ]
      ~i:[ "i" ==> d ]
      ~o:[ "o" ==> o ];
    o
  ;;

  let select = if structural_select then s_select else select

  let mux2 s a b =
    let o = wire (width a) in
    inst
      (prefix ^ "mux2")
      ~g:[ "b" ==> GInt (width a) ]
      ~i:[ "s" ==> s; "d0" ==> b; "d1" ==> a ]
      ~o:[ "o" ==> o ];
    o
  ;;

  let s_mux sel d =
    let w = width sel in
    let rec f n l def =
      if n = w
      then (
        match l with
        | [ a ] -> a
        | _ -> raise (Too_many_mux_data_elements (List.length d)))
      else (
        let s = select sel n n in
        let rec pairs = function
          | [] -> []
          | [ a ] -> [ a ]
          | a :: b :: t -> mux2 s b a :: pairs t
        in
        match l with
        | [ a ] -> mux2 s def a
        | _ -> f (n + 1) (pairs l) def)
    in
    let def =
      try List.hd_exn (List.rev d) with
      | _ -> raise Too_few_mux_data_elements
    in
    f 0 d def
  ;;

  let mux = if structural_mux then s_mux else mux
  let of_constant c = Constant.to_binary_string c |> of_bit_string
  let ( +: ) = binop1 "add"
  let ( -: ) = binop1 "sub"
  let ( *: ) = binop2 "mulu"
  let ( *+ ) = binop2 "muls"
  let ( &: ) = binop1 "and"
  let ( |: ) = binop1 "or"
  let ( ^: ) = binop1 "xor"

  let ( ~: ) i =
    let o = wire (width i) in
    inst (prefix ^ "not") ~g:[ "b" ==> GInt (width i) ] ~i:[ "i" ==> i ] ~o:[ "o" ==> o ];
    o
  ;;

  let ( ==: ) = binop0 "eq"
  let ( <: ) = binop0 "lt"
  let to_string s = name s
  let to_constant _ = failwith "Structural.Base.to_constant not supported"

  let sexp_of_t t =
    let create ?id ?name ?width ?value ?range constructor =
      [%message
        constructor
          (id : (int option[@sexp.option]))
          (name : (string option[@sexp.option]))
          (width : (int option[@sexp.option]))
          (value : (string option[@sexp.option]))
          (range : ((int * int) option[@sexp.option]))]
    in
    match t with
    | Module_input (id, name, width, _) -> create "Module_input" ~id ~name ~width
    | Module_output (id, name, width, _, _) -> create "Module_output" ~id ~name ~width
    | Module_tristate (id, name, width, _, _) -> create "Module_tristate" ~id ~name ~width
    | Instantiation (id, name, _, _, _, _, _, _) -> create "Instantiation" ~id ~name
    | Internal_triwire (id, width, _) -> create "Internal_triwire" ~id ~width
    | Internal_wire (id, width, _) -> create "Internal_wire" ~id ~width
    | Rtl_op (id, width, Constant value) -> create "Constant" ~id ~width ~value
    | Rtl_op (id, width, Select (h, l, _)) -> create "Select" ~id ~width ~range:(h, l)
    | Rtl_op (id, width, Concat _) -> create "Concat" ~id ~width
    | Rtl_op (id, width, Mux _) -> create "Mux" ~id ~width
    | Empty -> create "Empty"
    | Instantiation_output (id, name) -> create "Instantiation_output" ~id ~name
    | Instantiation_tristate (id, name) -> create "Instantiation_tristate" ~id ~name
  ;;
end

module Base0 = Base (struct
  let structural_const = false
  let structural_mux = false
  let structural_concat = false
  let structural_select = false
end)

module Base1 = Base (struct
  let structural_const = false
  let structural_mux = true
  let structural_concat = true
  let structural_select = true
end)

module Base2 = Base (struct
  let structural_const = true
  let structural_mux = true
  let structural_concat = true
  let structural_select = true
end)

(* {[
     let remove_unconnected circuit =
       let module IdSet = Set.Make
                            (struct
                              type t = int
                              let compare = compare
                            end)
       in
       let add set signal = IdSet.add (get_id signal) set in
       let add_list set signals = List.fold add set signals in
       let rec find (i, o, io, m_o, m_io, inst, rest) = function
         | [] -> (i, o, io, m_o, m_io, inst, rest)
         | s :: t ->
           match s with
           | Module_input _ -> add i s
           | Module_output _ -> add o s
           | Module_tristate _ -> add io s
           | Instantiation_output _ -> add m_o s
           | Instantiation_tristate _ -> add m_io s
           | Instantiation _ -> add inst s
           | _ -> s :: rest
       in
       ()
   ]} *)

let get_attributes signal =
  match signal with
  | Module_input (_, _, _, attrs)
  | Module_output (_, _, _, _, attrs)
  | Module_tristate (_, _, _, _, attrs) -> !attrs
  | _ -> []
;;

let attribute_value_to_string (v : Rtl_attribute.Value.t) =
  match v with
  | String s -> "\"" ^ s ^ "\""
  | Int i -> Int.to_string i
  | Bool b -> if b then "true" else "false"
;;

let attributes_to_string attrs =
  let body =
    List.map attrs ~f:(fun attr ->
      let name = Rtl_attribute.name attr in
      match Rtl_attribute.value attr with
      | None -> name
      | Some value -> Printf.sprintf "%s=%s" name (attribute_value_to_string value))
    |> String.concat ~sep:","
  in
  Printf.sprintf "(* %s *)" body
;;

let write_verilog os circuit =
  let open Printf in
  let declare typ signal =
    (match get_attributes signal with
     | [] -> ""
     | attrs -> "  " ^ attributes_to_string attrs ^ "\n")
    ^ "  "
    ^ typ
    ^ " "
    ^ (if width signal = 1 then "" else sprintf "[%i:0] " (width signal - 1))
    ^ name signal
  in
  let seperator sep l =
    List.fold l ~init:"" ~f:(fun a s -> if String.is_empty a then s else a ^ sep ^ s)
  in
  let assign s0 s1 = os ("  assign " ^ name s0 ^ " = " ^ name s1 ^ ";\n") in
  let part f = List.partition_tf ~f in
  let is_input = function
    | Module_input _ -> true
    | _ -> false
  in
  let is_output = function
    | Module_output _ -> true
    | _ -> false
  in
  let is_inout = function
    | Module_tristate _ -> true
    | _ -> false
  in
  let is_inst = function
    | Instantiation _ -> true
    | _ -> false
  in
  let signals = List.rev circuit.signals in
  let inputs, signals = part is_input signals in
  let outputs, signals = part is_output signals in
  let inouts, signals = part is_inout signals in
  let inst, signals = part is_inst signals in
  (* module interface *)
  os ("module " ^ circuit.name ^ "\n");
  os "(\n";
  os
    (seperator
       ",\n"
       (List.concat
          [ List.map inputs ~f:(declare "input")
          ; List.map outputs ~f:(declare "output")
          ; List.map inouts ~f:(declare "inout")
          ]));
  os "\n);\n\n";
  (* write wire declarations *)
  let declare_wire s = os (declare "wire" s ^ ";\n") in
  List.iter signals ~f:declare_wire;
  (* write assignments *)
  let connects = function
    | Empty | Instantiation_output _ | Instantiation_tristate _ -> false
    | _ -> true
  in
  let write_assignment s =
    match s with
    | Internal_wire (_, _, con) -> if connects !con then assign s !con
    | Internal_triwire (_, _, cons) ->
      List.iter !cons ~f:(fun con -> if connects con then assign s con)
    | Rtl_op (_, width, op) ->
      os ("  assign " ^ name s ^ " = ");
      (match op with
       | Constant b -> os (Int.to_string width ^ "'b" ^ b)
       | Select (hi, lo, s) ->
         os (name s ^ "[" ^ Int.to_string hi ^ ":" ^ Int.to_string lo ^ "]")
       | Concat d ->
         os "{ ";
         os (seperator ", " (List.map d ~f:name));
         os " }"
       | Mux (sel, d) ->
         let rec write n l =
           match l with
           | [] -> ()
           | [ x ] -> os ("    " ^ name x)
           | x :: t ->
             os ("    " ^ name sel ^ " == " ^ Int.to_string n ^ " ? " ^ name x ^ " :\n");
             write (n + 1) t
         in
         os "\n";
         write 0 d);
      os ";\n"
    | _ -> failwith "write_assignment"
  in
  List.iter signals ~f:write_assignment;
  (* write module outputs and inouts *)
  let assign_output s =
    match s with
    | Module_output (_, _, _, con, _) -> if connects !con then assign s !con
    | Module_tristate (_, _, _, cons, _) ->
      List.iter !cons ~f:(fun con -> if connects con then assign s con)
    | _ -> failwith "assign_output"
  in
  List.iter outputs ~f:assign_output;
  List.iter inouts ~f:assign_output;
  (* write instantiations *)
  let write_inst = function
    | Instantiation (id, iname, g, i, o, t, instance_name, attributes) ->
      if not (List.is_empty attributes)
      then (
        os "  ";
        os (attributes_to_string attributes);
        os "\n");
      os ("  " ^ iname ^ "");
      if not (List.is_empty g)
      then (
        os "\n  #(\n";
        os
          (seperator
             ",\n"
             (List.map g ~f:(fun (n, g) ->
                "    ."
                ^ n
                ^ "("
                ^ (match g with
                   | GInt i -> Int.to_string i
                   | GFloat f -> Float.to_string f
                   | GString s -> "\"" ^ s ^ "\""
                   | GUnquoted s -> s)
                ^ ")")));
        os "\n  )");
      let instance_name =
        match instance_name with
        | None -> "_" ^ Int.to_string id
        | Some instance_name -> instance_name
      in
      os (" " ^ instance_name ^ "\n");
      os "  (\n";
      os
        (seperator
           ", \n"
           (List.concat
              [ List.map i ~f:(fun (n, s) -> "    ." ^ n ^ "(" ^ name s ^ ")")
              ; List.map o ~f:(fun (n, s) -> "    ." ^ n ^ "(" ^ name s ^ ")")
              ; List.map t ~f:(fun (n, s) -> "    ." ^ n ^ "(" ^ name s ^ ")")
              ]));
      os "\n  );\n"
    | _ ->
      (* This should never be raised *)
      raise_s [%message "Expecting an instantiation"]
  in
  List.iter inst ~f:write_inst;
  os "endmodule\n"
;;

module Lib = struct
  let reg ~clock ~en d =
    let q = mk_wire (width d) in
    inst
      (prefix ^ "reg")
      ~g:[ "b" ==> GInt (width d) ]
      ~i:[ "clock" ==> clock; "enable" ==> en; "d" ==> d ]
      ~o:[ "q" ==> q ];
    q
  ;;

  let reg_r ~clock ~reset ?(def = 0) ~en d =
    let q = mk_wire (width d) in
    inst
      (prefix ^ "reg_r")
      ~g:[ "b" ==> GInt (width d); "default" ==> GInt def ]
      ~i:[ "clock" ==> clock; "reset" ==> reset; "enable" ==> en; "d" ==> d ]
      ~o:[ "q" ==> q ];
    q
  ;;

  let reg_c ~clock ~clear ?(def = 0) ~en d =
    let q = mk_wire (width d) in
    inst
      (prefix ^ "reg_c")
      ~g:[ "b" ==> GInt (width d); "default" ==> GInt def ]
      ~i:[ "clock" ==> clock; "clear" ==> clear; "enable" ==> en; "d" ==> d ]
      ~o:[ "q" ==> q ];
    q
  ;;

  let reg_rc ~clock ~reset ~clear ?(def = 0) ~en d =
    let q = mk_wire (width d) in
    inst
      (prefix ^ "reg_rc")
      ~g:[ "b" ==> GInt (width d); "default" ==> GInt def ]
      ~i:
        [ "clock" ==> clock
        ; "reset" ==> reset
        ; "clear" ==> clear
        ; "enable" ==> en
        ; "d" ==> d
        ]
      ~o:[ "q" ==> q ];
    q
  ;;

  let tristate_buffer ~en ~i ~t =
    let o = mk_wire (width i) in
    inst
      (prefix ^ "tristate_buffer")
      ~g:[ "b" ==> GInt (width i) ]
      ~i:[ "en" ==> en; "i" ==> i ]
      ~o:[ "o" ==> o ]
      ~t:[ "t" ==> t ];
    o
  ;;
end

module With_interface (I : Interface.S) (O : Interface.S) (T : Interface.S) = struct
  let create_circuit name f =
    create_circuit name (fun () ->
      let i = I.map2 I.port_names I.port_widths ~f:mk_input in
      let o = O.map2 O.port_names O.port_widths ~f:mk_output in
      let t = T.map2 T.port_names T.port_widths ~f:mk_tristate in
      f i o t)
  ;;

  let inst ?instance_name ?attributes ?g name i o t =
    let check_port_width name expected_width s =
      if width s <> expected_width
      then
        raise_s
          [%message
            "Instantiation argument does not match provided interface port width!"
              (name : string)
              (expected_width : int)
              (width s : int)]
    in
    I.iter3 I.port_names I.port_widths i ~f:check_port_width;
    O.iter3 O.port_names O.port_widths o ~f:check_port_width;
    T.iter3 T.port_names T.port_widths t ~f:check_port_width;
    inst
      ?instance_name
      ?attributes
      ?g
      ~i:(I.to_list (I.zip I.port_names i))
      ~o:(O.to_list (O.zip O.port_names o))
      ~t:(T.to_list (T.zip T.port_names t))
      name
  ;;
end
