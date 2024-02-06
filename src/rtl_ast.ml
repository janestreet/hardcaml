open Base

type range =
  | Vector of { width : int }
  | Bit
[@@deriving sexp_of, equal]

type reg_or_wire =
  | Reg
  | Wire
[@@deriving equal, sexp_of]

type var =
  { name : string
  ; range : range
  ; reg_or_wire : reg_or_wire
  ; attributes : Rtl_attribute.t list
  ; comment : string option
  }
[@@deriving sexp_of, equal]

type output =
  { output : var
  ; driven_by : var option
  }
[@@deriving sexp_of]

type logic_declaration =
  { read : var
  ; write : var
  ; all_names : var list
  }
[@@deriving sexp_of]

type memory_declaration =
  { memory : var
  ; memory_type : string
  ; depth : int
  ; q_out : logic_declaration
  }
[@@deriving sexp_of]

type multiport_memory_declaration =
  { memory : var
  ; memory_type : string
  ; depth : int
  ; range : range
  }
[@@deriving sexp_of]

type declaration =
  | Logic of logic_declaration
  | Multiport_memory of multiport_memory_declaration
  | Inst of logic_declaration
[@@deriving sexp_of]

type binop =
  | Add
  | Sub
  | Mulu
  | Muls
  | And
  | Or
  | Xor
  | Eq
  | Lt
[@@deriving sexp_of]

type assignment =
  | Binop of
      { lhs : var
      ; arg_a : var
      ; op : binop
      ; arg_b : var
      ; signed : bool
      }
  | Not of
      { lhs : var
      ; arg : var
      }
  | Mux of
      { lhs : var
      ; select : var
      ; cases : var list
      }
  | Select of
      { lhs : var
      ; arg : var
      ; high : int
      ; low : int
      }
  | Concat of
      { lhs : var
      ; args : var list
      }
  | Const of
      { lhs : var
      ; constant : Bits.t
      }
  | Wire of
      { lhs : var
      ; driver : var
      }
[@@deriving sexp_of]

type condition =
  | Level of
      { level : Level.t
      ; var : var
      }
  | Edge of
      { edge : Edge.t
      ; var : var
      }
  | Clock of
      { edge : Edge.t
      ; clock : var
      }
[@@deriving sexp_of]

type always =
  | If of
      { condition : condition
      ; on_true : always list
      ; on_false : always list
      }
  | Assignment of
      { lhs : var
      ; rhs : var
      }
  | Memory_assignment of
      { lhs : var
      ; index : var
      ; rhs : var
      }
  | Case of
      { select : var
      ; cases : always list list
      }
[@@deriving sexp_of]

type sensitivity =
  { edge : Edge.t
  ; var : var
  }
[@@deriving sexp_of]

type sensitivity_list =
  | Star
  | Edges of sensitivity list
[@@deriving sexp_of]

type instantiation_input_port =
  { port_name : string
  ; connection : var
  }
[@@deriving sexp_of]

type instantiation_output_port =
  { port_name : string
  ; connection : var
  ; high : int
  ; low : int
  }
[@@deriving sexp_of]

type instantiation =
  { name : string
  ; instance : string
  ; parameters : Parameter.t list
  ; input_ports : instantiation_input_port list
  ; output_ports : instantiation_output_port list
  ; attributes : Rtl_attribute.t list
  }
[@@deriving sexp_of]

type statement =
  | Assignment of assignment
  | Instantiation of instantiation
  | Always of
      { sensitivity_list : sensitivity_list
      ; always : always
      }
  | Mux of
      { to_assignment : unit -> statement
      ; to_always : unit -> statement
      ; is_mux2 : bool
      }
  | Multiport_mem of { always : statement array }
  | Mem_read_port of
      { lhs : var
      ; memory : var
      ; address : var
      }
[@@deriving sexp_of]

type t =
  { name : string
  ; inputs : var list
  ; outputs : output list
  ; declarations : declaration list
  ; statements : statement list
  ; var_map : declaration Map.M(Signal.Uid).t
  }
[@@deriving sexp_of]

let bit_or_vec = function
  | 1 -> Bit
  | width -> Vector { width }
;;

let bit_or_vec_of_signal s = bit_or_vec (Signal.width s)

let declaration_of_logic ~reg_or_wire lang signal =
  let names = Rtl_name.mangle_signal_names lang signal in
  let range = bit_or_vec_of_signal signal in
  let var name =
    { name
    ; range
    ; reg_or_wire
    ; attributes = Signal.attributes signal
    ; comment = Signal.comment signal
    }
  in
  match names with
  | [] -> (* the mangler will return at least one name *) assert false
  | name :: _ -> { read = var name; write = var name; all_names = List.map names ~f:var }
;;

let declaration_of_multiport_memory lang signal =
  let array, type_ = Rtl_name.mangle_multiport_mem_name lang signal in
  let depth =
    match signal with
    | Multiport_mem { size; _ } -> size
    | _ -> assert false
  in
  { memory =
      { name = array
      ; range = bit_or_vec_of_signal signal
      ; reg_or_wire = Reg
      ; attributes = Signal.attributes signal
      ; comment = Signal.comment signal
      }
  ; memory_type = type_
  ; depth
  ; range = bit_or_vec (Signal.width signal)
  }
;;

let var_of_io_port (lang : Rtl_name.t) signal =
  match Signal.names signal with
  | [] ->
    raise_s
      [%message
        "[Rtl_ast] circuit ports must have a name"
          ~note:"This error should have been caught during circuit generation."
          ~port:(signal : Signal.t)]
  | [ name ] ->
    Rtl_name.add_port_name lang signal name;
    ( Signal.uid signal
    , { name
      ; range = bit_or_vec_of_signal signal
      ; reg_or_wire = Wire
      ; attributes = Signal.attributes signal
      ; comment = Signal.comment signal
      } )
  | _ ->
    raise_s
      [%message
        "[Rtl_ast] circuit ports may not have multiple names"
          ~note:"This error should have been caught during circuit generation."
          ~port:(signal : Signal.t)]
;;

let is_internal_signal_of_circuit circuit signal =
  (not (Circuit.is_input circuit signal))
  && (not (Circuit.is_output circuit signal))
  && not (Signal.is_empty signal)
;;

let find_logic var_map signal =
  match Map.find_exn var_map (Signal.uid signal) with
  | Logic logic -> logic
  | Inst inst -> inst
  | Multiport_memory _ -> (* only Mem_read_ports should be seen here. *) assert false
;;

let find_multiport_memory var_map signal =
  match Map.find_exn var_map (Signal.uid signal) with
  | Multiport_memory memory -> memory
  | _ -> raise_s [%message "[Rtl_ast] expecting multiport memory declaration"]
;;

let always_of_reg var_map (register : Signal.Type.register) ~q ~d =
  let find = find_logic var_map in
  let q_of d : always = Assignment { lhs = (find q).write; rhs = (find d).read } in
  let enabled =
    if Signal.is_empty register.reg_enable || Signal.is_vdd register.reg_enable
    then q_of d
    else
      If
        { condition = Level { level = High; var = (find register.reg_enable).read }
        ; on_true = [ q_of d ]
        ; on_false = []
        }
  in
  let cleared =
    if Signal.is_empty register.reg_clear
    then enabled
    else (
      let clear_to = q_of register.reg_clear_value in
      If
        { condition =
            Level
              { level = register.reg_clear_level; var = (find register.reg_clear).read }
        ; on_true = [ clear_to ]
        ; on_false = [ enabled ]
        })
  in
  let clock_and_reset : always =
    let clocked : always =
      If
        { condition =
            Clock
              { edge = register.reg_clock_edge; clock = (find register.reg_clock).read }
        ; on_true = [ cleared ]
        ; on_false = []
        }
    in
    if Signal.is_empty register.reg_reset
    then clocked
    else (
      let reset_to = q_of register.reg_reset_value in
      If
        { condition =
            Edge { edge = register.reg_reset_edge; var = (find register.reg_reset).read }
        ; on_true = [ reset_to ]
        ; on_false = [ clocked ]
        })
  in
  let sensitivity_list =
    let at_edge signal = function
      | Edge.Rising -> { edge = Rising; var = (find signal).read }
      | Falling -> { edge = Falling; var = (find signal).read }
    in
    Edges
      (List.filter_opt
         [ Some (at_edge register.reg_clock register.reg_clock_edge)
         ; (if Signal.is_empty register.reg_reset
            then None
            else Some (at_edge register.reg_reset register.reg_reset_edge))
         ])
  in
  Always { sensitivity_list; always = clock_and_reset }
;;

let always_of_multiport_mem ~var_map ~multiport_memory_declaration ~write_ports =
  let find = find_logic var_map in
  let always_of_write_port (write_port : _ Write_port.t) =
    let q_of () : always =
      Memory_assignment
        { lhs = multiport_memory_declaration.memory
        ; index = (find write_port.write_address).read
        ; rhs = (find write_port.write_data).read
        }
    in
    let enabled =
      let write_enable = write_port.write_enable in
      if Signal.is_empty write_enable || Signal.is_vdd write_enable
      then q_of ()
      else
        If
          { condition = Level { level = High; var = (find write_enable).read }
          ; on_true = [ q_of () ]
          ; on_false = []
          }
    in
    let write_clock = write_port.write_clock in
    let clocked : always =
      If
        { condition = Clock { edge = Rising; clock = (find write_clock).read }
        ; on_true = [ enabled ]
        ; on_false = []
        }
    in
    let sensitivity_list = Edges [ { edge = Rising; var = (find write_clock).read } ] in
    Always { sensitivity_list; always = clocked }
  in
  Array.map write_ports ~f:always_of_write_port
;;

let create_phantom_inputs lang circuit =
  Circuit.phantom_inputs circuit
  |> List.map ~f:(fun (name, width) ->
       Rtl_name.add_phantom_port_name lang name;
       { name
       ; range = bit_or_vec width
       ; reg_or_wire = Wire
       ; attributes = []
       ; comment = None
       })
;;

let is_mux2 = function
  | Signal.Type.Mux { signal_id = _; select; cases = [ _; _ ] }
    when Signal.width select = 1 -> true
  | _ -> false
;;

let create_vars lang internal =
  let rec f var_map shared_constants_map decls internal signals =
    match signals with
    | [] -> var_map, decls, internal
    | signal :: signals ->
      let add_to_decl_map signal decl =
        f
          (Map.add_exn var_map ~key:(Signal.uid signal) ~data:decl)
          shared_constants_map
          (decl :: decls)
          (signal :: internal)
          signals
      in
      if Signal.Type.is_mem signal
      then (
        let decl = Multiport_memory (declaration_of_multiport_memory lang signal) in
        add_to_decl_map signal decl)
      else if Signal.Type.is_inst signal
      then (
        let decl = Inst (declaration_of_logic ~reg_or_wire:Wire lang signal) in
        add_to_decl_map signal decl)
      else if Signal.Type.is_const signal && List.is_empty (Signal.names signal)
      then (
        let const = Signal.Type.const_value signal in
        match Map.find shared_constants_map const with
        | Some decl ->
          f
            (Map.add_exn var_map ~key:(Signal.uid signal) ~data:decl)
            shared_constants_map
            decls
            internal
            signals
        | None ->
          let decl = Logic (declaration_of_logic ~reg_or_wire:Wire lang signal) in
          f
            (Map.add_exn var_map ~key:(Signal.uid signal) ~data:decl)
            (Map.add_exn shared_constants_map ~key:const ~data:decl)
            (decl :: decls)
            (signal :: internal)
            signals)
      else (
        let reg_or_wire =
          if Signal.Type.is_reg signal
             || (Signal.Type.is_mux signal && not (is_mux2 signal))
          then Reg
          else Wire
        in
        let decl = Logic (declaration_of_logic ~reg_or_wire lang signal) in
        add_to_decl_map signal decl)
  in
  let map, decls, internal =
    f (Map.empty (module Signal.Uid)) (Map.empty (module Bits)) [] [] internal
  in
  map, List.rev decls, List.rev internal
;;

let create_statement lang var_map (signal : Signal.t) =
  let find context signal =
    try find_logic var_map signal with
    | _ ->
      raise_s [%message "[Rtl_ast] Failed to find signal in logic map" (context : string)]
  in
  match signal with
  | Empty -> assert false
  | Multiport_mem { write_ports; _ } ->
    let multiport_memory_declaration = find_multiport_memory var_map signal in
    Multiport_mem
      { always =
          always_of_multiport_mem ~var_map ~multiport_memory_declaration ~write_ports
      }
  | Mem_read_port { memory; read_address; _ } ->
    Mem_read_port
      { lhs = (find "Mem_read_port.lhs" signal).write
      ; memory = (find_multiport_memory var_map memory).memory
      ; address = (find "Mem_read_port.address" read_address).read
      }
  | Not { arg; _ } ->
    Assignment
      (Not { lhs = (find "Not.lhs" signal).write; arg = (find "Not.arg" arg).read })
  | Cat { args; _ } ->
    Assignment
      (Concat
         { lhs = (find "Cat.lhs" signal).write
         ; args = List.map args ~f:(fun arg -> (find "Cat.arg" arg).read)
         })
  | Op2 { op = Signal_muls; arg_a; arg_b; _ } ->
    Assignment
      (Binop
         { lhs = (find "Op2.lhs" signal).write
         ; arg_a = (find "Op2.arg_a" arg_a).read
         ; op = Muls
         ; arg_b = (find "Op2.arg_b" arg_b).read
         ; signed = true
         })
  | Op2 { op; arg_a; arg_b; _ } ->
    let op =
      match op with
      | Signal_add -> Add
      | Signal_sub -> Sub
      | Signal_mulu -> Mulu
      | Signal_muls -> assert false
      | Signal_and -> And
      | Signal_or -> Or
      | Signal_xor -> Xor
      | Signal_eq -> Eq
      | Signal_lt -> Lt
    in
    Assignment
      (Binop
         { lhs = (find "Op2.lhs" signal).write
         ; arg_a = (find "Op2.arg_a" arg_a).read
         ; op
         ; arg_b = (find "Op2.arg_b" arg_b).read
         ; signed = false
         })
  | Wire { driver; _ } ->
    Assignment
      (Wire
         { lhs = (find "Wire.lhs" signal).write
         ; driver = (find "Wire.driver" !driver).read
         })
  | Select { arg; high; low; _ } ->
    Assignment
      (Select
         { lhs = (find "Select.lhs" signal).write
         ; arg = (find "Select.arg" arg).read
         ; high
         ; low
         })
  | Mux { select; cases; _ } ->
    let to_always () =
      Always
        { sensitivity_list = Star
        ; always =
            Case
              { select = (find "Case.select" select).read
              ; cases =
                  List.map cases ~f:(fun case : always list ->
                    [ Assignment
                        { lhs = (find "Case.lhs" signal).write
                        ; rhs = (find "Case.rhs" case).read
                        }
                    ])
              }
        }
    in
    let to_assignment () =
      Assignment
        (Mux
           { lhs = (find "Assignment.lhs" signal).write
           ; select = (find "Assignment.select" select).read
           ; cases = List.map cases ~f:(fun case -> (find "Assignment.case" case).read)
           })
    in
    Mux { to_assignment; to_always; is_mux2 = is_mux2 signal }
  | Reg { register; d; _ } -> always_of_reg var_map register ~q:signal ~d
  | Const { constant; _ } ->
    Assignment (Const { lhs = (find "Const.lhs" signal).write; constant })
  | Inst { signal_id = _; extra_uid = _; instantiation } ->
    let input_ports =
      List.map instantiation.inst_inputs ~f:(fun (port_name, signal) ->
        { port_name; connection = (find ("Inst.input_port: " ^ port_name) signal).read })
    in
    let output_ports =
      List.map instantiation.inst_outputs ~f:(fun (port_name, (width, low)) ->
        { port_name
        ; connection = (find ("Inst.output_port: " ^ port_name) signal).read
        ; high = low + width - 1
        ; low
        })
    in
    Instantiation
      { name = instantiation.inst_name
      ; instance = Rtl_name.mangle_instantiation_name lang signal
      ; parameters = instantiation.inst_generics
      ; input_ports
      ; output_ports
      ; attributes = Signal.attributes signal
      }
;;

let create_statements lang var_map internal =
  List.map internal ~f:(fun signal ->
    try create_statement lang var_map signal with
    | exn ->
      raise_s
        [%message
          "[Rtl_ast] failed to create statement for signal"
            (signal : Signal.t)
            (exn : exn)])
;;

let add_io_vars ~var_map vars =
  List.fold vars ~init:var_map ~f:(fun map (uid, var) ->
    Map.add_exn
      map
      ~key:uid
      ~data:(Logic { read = var; write = var; all_names = [ var ] }))
;;

let create_var_map inputs internal_vars =
  let var_map = Map.of_alist_exn (module Signal.Uid) internal_vars in
  add_io_vars ~var_map inputs
;;

let driven_by var_map output =
  let find signal =
    try find_logic var_map signal with
    | _ -> raise_s [%message "Failed to find output driver" (output : Signal.t)]
  in
  match output with
  | Signal.Type.Wire { driver; _ } -> (find !driver).read
  | _ -> (* this cannot happen by constrution *) assert false
;;

let create_outputs ~blackbox var_map outputs output_vars =
  let outputs =
    List.map2_exn output_vars outputs ~f:(fun (_, var) signal ->
      { output = var
      ; driven_by = (if blackbox then None else Some (driven_by var_map signal))
      })
  in
  let var_map = add_io_vars ~var_map output_vars in
  outputs, var_map
;;

let of_circuit ~blackbox lang circuit =
  let module_name = Circuit.name circuit in
  let inputs = Circuit.inputs circuit |> List.map ~f:(var_of_io_port lang) in
  let phantom_inputs = create_phantom_inputs lang circuit in
  let outputs = Circuit.outputs circuit in
  let output_vars = List.map outputs ~f:(var_of_io_port lang) in
  if blackbox
  then (
    let var_map = create_var_map inputs [] in
    let outputs, var_map = create_outputs ~blackbox var_map outputs output_vars in
    { name = module_name
    ; inputs = List.map inputs ~f:snd @ phantom_inputs
    ; outputs
    ; declarations = []
    ; statements = []
    ; var_map
    })
  else (
    let signal_graph = Signal_graph.create (Circuit.outputs circuit) in
    let internal =
      Signal_graph.filter signal_graph ~f:(is_internal_signal_of_circuit circuit)
    in
    let var_map, declarations, internal = create_vars lang internal in
    let var_map = add_io_vars ~var_map inputs in
    let statements = create_statements lang var_map internal in
    let outputs, var_map = create_outputs ~blackbox var_map outputs output_vars in
    { name = module_name
    ; inputs = List.map inputs ~f:snd @ phantom_inputs
    ; outputs
    ; declarations
    ; statements
    ; var_map
    })
;;

module Signals_name_map = struct
  module Uid_with_index = struct
    module T = struct
      type t = Signal.Uid.t * int [@@deriving compare, sexp_of]
    end

    include T
    include (val Comparator.make ~compare ~sexp_of_t)
  end

  type t_rtl_ast = t
  type t = string Map.M(Uid_with_index).t [@@deriving equal, sexp_of]

  let create (t : t_rtl_ast) =
    let empty = Map.empty (module Uid_with_index) in
    Map.fold t.var_map ~init:empty ~f:(fun ~key:uid ~data:declaration map ->
      let all_names =
        match declaration with
        | Logic { all_names; _ } -> all_names
        | Multiport_memory { memory; _ } -> [ memory ]
        | Inst { all_names; _ } -> all_names
      in
      List.foldi all_names ~init:map ~f:(fun idx map var ->
        Map.add_exn map ~key:(uid, idx) ~data:var.name))
  ;;
end
