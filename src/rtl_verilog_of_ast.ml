open Base

let tab_len = 4
let tab = String.init tab_len ~f:(Fn.const ' ')
let tab2 = tab ^ tab
let spaces n = String.init n ~f:(Fn.const ' ')

let range (r : Rtl_ast.range) =
  match r with
  | Bit -> ""
  | Vector { width } -> [%string " [%{(width-1)#Int}:0]"]
;;

let range_compat (r : Rtl_ast.range) =
  match r with
  | Bit -> " [0:0]"
  | Vector _ -> range r
;;

let range_and_name name r = [%string "%{range r} %{name}"]
let input name range = [%string "input%{range_and_name name range}"]
let output name range = [%string "output%{range_and_name name range}"]
let reg name range = [%string "reg%{range_and_name name range}"]
let wire name range = [%string "wire%{range_and_name name range}"]

let attributes_to_string (attributes : Rtl_attribute.t list) =
  let attrs =
    List.map attributes ~f:(fun attr ->
      match Rtl_attribute.value attr with
      | None -> [%string "%{Rtl_attribute.name attr}"]
      | Some attr_value ->
        let attr_value =
          match attr_value with
          | String s -> [%string {|"%{s}"|}]
          | Int i -> [%string "%{i#Int}"]
          | Bool b -> if b then "1" else "0"
        in
        [%string "%{Rtl_attribute.name attr}=%{attr_value}"])
  in
  [%string {|(* %{String.concat ~sep:"," attrs} *)|}]
;;

let declare_io_ports buffer (ast : Rtl_ast.t) =
  let add_string = Buffer.add_string buffer in
  let io_ports =
    List.concat
      [ List.map ast.inputs ~f:(fun { name; range; attributes; _ } ->
          ( name
          , match attributes with
            | [] -> input name range
            | attrs ->
              [%string "%{attributes_to_string attrs}\n%{tab}%{input name range}"] ))
      ; List.map
          ast.outputs
          ~f:(fun { output = { name; range; attributes; _ }; driven_by = _ } ->
          ( name
          , match attributes with
            | [] -> output name range
            | attrs ->
              [%string "%{attributes_to_string attrs}\n%{tab}%{output name range}"] ))
      ]
  in
  let io_ports_decl =
    List.map io_ports ~f:fst |> String.concat ~sep:[%string ",\n%{tab}"]
  in
  add_string [%string {|module %{ast.name} (
%{tab}%{io_ports_decl}
);

|}];
  List.iter io_ports ~f:(fun p -> add_string [%string "%{tab}%{snd p};\n"]);
  add_string "\n"
;;

let write_attributes buffer attrs =
  match attrs with
  | [] -> ()
  | attrs -> Buffer.add_string buffer [%string "%{tab}%{attributes_to_string attrs}\n"]
;;

let get_comment comment =
  Option.map comment ~f:(fun c -> [%string "/* %{c} */"]) |> Option.value ~default:""
;;

let declaration buffer (decl : Rtl_ast.declaration) =
  let add_string = Buffer.add_string buffer in
  let write_attr (var : Rtl_ast.var) = write_attributes buffer var.attributes in
  let is_write_var (decl : Rtl_ast.logic_declaration) var =
    Rtl_ast.equal_var var decl.write
  in
  let write_var ~write_attrs decl (var : Rtl_ast.var) =
    let comment = get_comment var.comment in
    (* we write attributes on the aliases as well - which is one choice. *)
    if write_attrs then write_attr var;
    if Rtl_ast.equal_reg_or_wire var.reg_or_wire Reg && is_write_var decl var
    then add_string [%string "%{tab}%{reg var.name var.range}%{comment};\n"]
    else add_string [%string "%{tab}%{wire var.name var.range}%{comment};\n"]
  in
  match decl with
  | Logic decl -> List.iter decl.all_names ~f:(write_var ~write_attrs:true decl)
  | Inst inst -> List.iter inst.all_names ~f:(write_var ~write_attrs:false inst)
  | Multiport_memory { memory; depth; range = r; _ } ->
    let range = range_compat r in
    let comment = get_comment memory.comment in
    write_attr memory;
    add_string
      [%string "%{tab}reg%{range} %{memory.name}[0:%{(depth-1)#Int}]%{comment};\n"]
;;

let declarations buffer (ast : Rtl_ast.t) =
  List.iter ast.declarations ~f:(declaration buffer)
;;

let rec write_always indent buffer (t : Rtl_ast.always) =
  let add_string = Buffer.add_string buffer in
  let block indent ts ~f =
    match ts with
    | [ _ ] -> f (indent ^ tab)
    | _ ->
      add_string [%string "%{indent}begin\n"];
      f (indent ^ tab);
      add_string [%string "%{indent}end\n"]
  in
  let cond (c : Rtl_ast.condition) =
    match c with
    | Level { level = High; var } | Edge { edge = Rising; var } -> var.name
    | Level { level = Low; var } | Edge { edge = Falling; var } ->
      [%string "%{var.name} == 0"]
    | Clock _ -> (* clocks are not written in verilog  *) assert false
  in
  match t with
  | If { condition = Clock _; on_true; on_false = [] } ->
    List.iter on_true ~f:(write_always indent buffer)
  | If { condition; on_true; on_false = [] } ->
    add_string [%string "%{indent}if (%{cond condition})\n"];
    block indent on_true ~f:(fun indent ->
      List.iter on_true ~f:(write_always indent buffer))
  | If { condition; on_true; on_false } ->
    add_string [%string "%{indent}if (%{cond condition})\n"];
    block indent on_true ~f:(fun indent ->
      List.iter on_true ~f:(write_always indent buffer));
    add_string [%string "%{indent}else\n"];
    block indent on_false ~f:(fun indent ->
      List.iter on_false ~f:(write_always indent buffer))
  | Assignment { lhs; rhs } ->
    add_string [%string "%{indent}%{lhs.name} <= %{rhs.name};\n"]
  | Memory_assignment { lhs; index; rhs } ->
    add_string [%string "%{indent}%{lhs.name}[%{index.name}] <= %{rhs.name};\n"]
  | Case { select; cases } ->
    add_string [%string "%{indent}case (%{select.name})\n"];
    let num_cases = List.length cases in
    List.iteri cases ~f:(fun index case ->
      if index = num_cases - 1
      then add_string [%string "%{indent}default:\n"]
      else add_string [%string "%{indent}%{index#Int}:\n"];
      block indent case ~f:(fun indent -> List.iter case ~f:(write_always indent buffer)));
    add_string [%string "%{indent}endcase\n"]
;;

let write_always buffer (sensitivity_list : Rtl_ast.sensitivity_list) always =
  let add_string = Buffer.add_string buffer in
  let sensitivity_list =
    match sensitivity_list with
    | Star -> "*"
    | Edges edges ->
      let edges =
        List.map edges ~f:(fun { edge; var } ->
          match edge with
          | Rising -> [%string "posedge %{var.name}"]
          | Falling -> [%string "negedge %{var.name}"])
        |> String.concat ~sep:" or "
      in
      [%string "(%{edges})"]
  in
  add_string [%string "%{tab}always @%{sensitivity_list} begin\n"];
  write_always tab2 buffer always;
  add_string [%string "%{tab}end\n"]
;;

let parameter_value (p : Parameter.t) =
  match p.value with
  | String v -> [%string {|"%{v}"|}]
  | Int v -> [%string "%{v#Int}"]
  (* For consistency with VHDL but not proven necessary for verilog. In terms of
     formatting floats, printfs [%f] seems to do with right thing. *)
  | Real v -> Printf.sprintf "%f" v
  | Bool b | Bit b -> if b then "1'b1" else "1'b0"
  | Std_logic_vector v | Std_ulogic_vector v ->
    [%string
      "%{(Logic.Std_logic_vector.width v)#Int}'b%{(Logic.Std_logic_vector.to_string v)}"]
  | Bit_vector v ->
    [%string "%{(Logic.Bit_vector.width v)#Int}'b%{(Logic.Bit_vector.to_string v)}"]
  | Std_logic b | Std_ulogic b ->
    (* According to the modelsim manual, you must encode the enumeration index. I suspect
       this is not especially portable. *)
    [%string "4'd%{(Logic.Std_logic.to_int b)#Int}"]
;;

let write_instantiation buffer (instantiation : Rtl_ast.instantiation) =
  let add_string = Buffer.add_string buffer in
  write_attributes buffer instantiation.attributes;
  add_string [%string "%{tab}%{instantiation.name}\n"];
  let sep = [%string ",\n%{spaces (tab_len*2 + 3)}"] in
  (match instantiation.parameters with
   | [] -> ()
   | parameters ->
     let parameters =
       String.concat
         ~sep
         (List.map parameters ~f:(fun p ->
            let name, value = Parameter_name.to_string p.name, parameter_value p in
            [%string ".%{name}(%{value})"]))
     in
     add_string [%string "%{tab2}#( %{parameters} )\n"]);
  add_string [%string "%{tab2}%{instantiation.instance}\n"];
  let sep = [%string ",\n%{spaces (tab_len*2 + 2)}"] in
  let input_ports =
    List.map instantiation.input_ports ~f:(fun { port_name; connection } ->
      [%string ".%{port_name}(%{connection.name})"])
  in
  let output_ports =
    match instantiation.output_ports with
    | [ { port_name; connection; high = 0; low = 0 } ] ->
      [ [%string ".%{port_name}(%{connection.name})"] ]
    | output_ports ->
      List.map output_ports ~f:(fun { port_name; connection; high; low } ->
        [%string ".%{port_name}(%{connection.name}[%{high#Int}:%{low#Int}])"])
  in
  let ports = String.concat ~sep (input_ports @ output_ports) in
  add_string [%string "%{tab2}( %{ports} );\n"]
;;

let operator : Rtl_ast.binop -> string = function
  | Add -> "+"
  | Sub -> "-"
  | And -> "&"
  | Or -> "|"
  | Xor -> "^"
  | Mulu -> "*"
  | Muls -> "*"
  | Eq -> "=="
  | Lt -> "<"
;;

let rec statement buffer (stat : Rtl_ast.statement) =
  let add_string = Buffer.add_string buffer in
  match stat with
  | Assignment (Not { lhs; arg }) ->
    add_string [%string "%{tab}assign %{lhs.name} = ~ %{arg.name};\n"]
  | Assignment (Concat { lhs; args }) ->
    let sep = ",\n" ^ spaces (String.length lhs.name + ((tab_len * 3) + 4)) in
    let args = List.map args ~f:(fun arg -> arg.name) |> String.concat ~sep in
    add_string [%string "%{tab}assign %{lhs.name} = { %{args} };\n"]
  | Assignment (Binop { lhs; arg_a; op; arg_b; signed = false }) ->
    add_string
      [%string "%{tab}assign %{lhs.name} = %{arg_a.name} %{operator op} %{arg_b.name};\n"]
  | Assignment (Binop { lhs; arg_a; op; arg_b; signed = true }) ->
    add_string
      [%string
        "%{tab}assign %{lhs.name} = $signed(%{arg_a.name}) %{operator op} \
         $signed(%{arg_b.name});\n"]
  | Assignment (Wire { lhs; driver }) ->
    add_string [%string "%{tab}assign %{lhs.name} = %{driver.name};\n"]
  | Assignment (Select { lhs; arg; high; low }) ->
    add_string
      [%string "%{tab}assign %{lhs.name} = %{arg.name}[%{high#Int}:%{low#Int}];\n"]
  | Assignment (Const { lhs; constant }) ->
    add_string
      [%string
        "%{tab}assign %{lhs.name} = %{Bits.width constant#Int}'b%{Bits.to_bstr constant};\n"]
  | Assignment (Mux { lhs; select; cases = [ on_false; on_true ] }) ->
    add_string
      [%string
        "%{tab}assign %{lhs.name} = %{select.name} ? %{on_true.name} : %{on_false.name};\n"]
  | Assignment (Mux _) -> (* In verilog, these cases are written by always *) assert false
  | Mux { to_assignment; to_always; is_mux2 } ->
    if is_mux2
    then statement buffer (to_assignment ())
    else statement buffer (to_always ())
  | Always { sensitivity_list; always } -> write_always buffer sensitivity_list always
  | Instantiation instantiation -> write_instantiation buffer instantiation
  | Multiport_mem { always } -> Array.iter always ~f:(statement buffer)
  | Mem_read_port { lhs; memory; address } ->
    add_string [%string "%{tab}assign %{lhs.name} = %{memory.name}[%{address.name}];\n"]
;;

let statements buffer (ast : Rtl_ast.t) = List.iter ast.statements ~f:(statement buffer)

let output_assignments buffer (ast : Rtl_ast.t) =
  let add_string = Buffer.add_string buffer in
  List.iter ast.outputs ~f:(fun { output; driven_by } ->
    Option.iter driven_by ~f:(fun driven_by ->
      add_string [%string "%{tab}assign %{output.name} = %{driven_by.name};\n"]))
;;

let write_alias buffer (decl : Rtl_ast.declaration) =
  let add_string = Buffer.add_string buffer in
  match decl with
  | Logic decl ->
    List.iter decl.all_names ~f:(fun var ->
      if not (Rtl_ast.equal_var var decl.write)
      then add_string [%string "%{tab}assign %{var.name} = %{decl.write.name};\n"])
  (* We dont allow naming in the following cases, so no aliases *)
  | Multiport_memory _ -> ()
  | Inst _ -> ()
;;

let write_aliases buffer (ast : Rtl_ast.t) =
  List.iter ast.declarations ~f:(write_alias buffer)
;;

let to_buffer buffer (ast : Rtl_ast.t) =
  let add_string = Buffer.add_string buffer in
  declare_io_ports buffer ast;
  declarations buffer ast;
  statements buffer ast;
  write_aliases buffer ast;
  output_assignments buffer ast;
  add_string "\nendmodule\n"
;;
