open! Core0

let tab_len = 4
let spaces n = String.init n ~f:(Fn.const ' ') |> Rope.of_string
let tab = spaces 4
let tab2 = spaces 8
let concat_map ?sep t ~f = Rope.concat ?sep (List.map t ~f)

let range (r : Rtl_ast.range) =
  match r with
  | Bit -> Rope.empty
  | Vector { width } -> [%rope " [%{(width-1)#Int}:0]"]
;;

let range_compat (r : Rtl_ast.range) =
  match r with
  | Bit -> [%rope " [0:0]"]
  | Vector _ -> range r
;;

let range_and_name name r = [%rope "%{range r} %{name}"]
let input name range = [%rope "input%{range_and_name name range}"]
let output name range = [%rope "output%{range_and_name name range}"]
let reg name range = [%rope "reg%{range_and_name name range}"]
let wire name range = [%rope "wire%{range_and_name name range}"]
let bit name range = [%rope "bit%{range_and_name name range}"]

let attributes_to_string (attributes : Rtl_attribute.t list) =
  let attrs =
    List.map attributes ~f:(fun attr ->
      match Rtl_attribute.value attr with
      | None -> Rope.of_string (Rtl_attribute.name attr)
      | Some attr_value ->
        let attr_value =
          match attr_value with
          | String s -> [%rope {|"%{s#String}"|}]
          | Int i -> [%rope "%{i#Int}"]
          | Bool b -> Rope.of_string (if b then "1" else "0")
        in
        [%rope "%{Rtl_attribute.name attr#String}=%{attr_value}"])
  in
  [%rope {|(* %{Rope.concat ~sep:[%rope ","] attrs} *)|}]
;;

let declare_io_ports (ast : Rtl_ast.t) =
  let io_ports =
    List.concat
      [ List.map ast.inputs ~f:(fun { name; range; attributes; _ } ->
          ( name
          , match attributes with
            | [] -> input name range
            | attrs -> [%rope "%{attributes_to_string attrs}\n%{tab}%{input name range}"]
          ))
      ; List.map
          ast.outputs
          ~f:(fun { output = { name; range; attributes; _ }; driven_by = _ } ->
            ( name
            , match attributes with
              | [] -> output name range
              | attrs ->
                [%rope "%{attributes_to_string attrs}\n%{tab}%{output name range}"] ))
      ]
  in
  let io_ports_decl = concat_map ~sep:[%rope ",\n%{tab}"] io_ports ~f:fst in
  let io_ports = concat_map io_ports ~f:(fun p -> [%rope "%{tab}%{snd p};\n"]) in
  [%rope
    {|module %{ast.name#String} (
%{tab}%{io_ports_decl}
);

%{io_ports}
|}]
;;

let write_attributes attrs =
  match attrs with
  | [] -> Rope.empty
  | attrs -> [%rope "%{tab}%{attributes_to_string attrs}\n"]
;;

let get_comment comment =
  Option.map comment ~f:(fun c -> [%rope "/* %{c#String} */"])
  |> Option.value ~default:Rope.empty
;;

let verilog_constant bits =
  let width = Bits.width bits in
  let value = Bits.to_bstr bits in
  [%rope "%{width#Int}'b%{value#String}"]
;;

let declaration (decl : Rtl_ast.declaration) ~(two_state : bool) =
  let write_attr (var : Rtl_ast.var) = write_attributes var.attributes in
  let is_write_var (decl : Rtl_ast.logic_declaration) var =
    Rtl_ast.equal_var var decl.write
  in
  let write_var ~write_attrs (decl : Rtl_ast.logic_declaration) (var : Rtl_ast.var) =
    let comment = get_comment var.comment in
    let initialize_to =
      Option.value_map ~default:Rope.empty decl.initialize_to ~f:(fun initialize_to ->
        [%rope " = %{verilog_constant initialize_to}"])
    in
    (* we write attributes on the aliases as well - which is one choice. *)
    let attr = if write_attrs then write_attr var else Rope.empty in
    if Rtl_ast.equal_reg_or_wire var.reg_or_wire Reg && is_write_var decl var
    then
      [%rope
        "%{attr}%{tab}%{(if two_state then bit else reg) var.name \
         var.range}%{comment}%{initialize_to};\n"]
    else
      [%rope
        "%{attr}%{tab}%{(if two_state then bit else wire) var.name var.range}%{comment};\n"]
  in
  match decl with
  | Logic decl -> concat_map decl.all_names ~f:(write_var ~write_attrs:true decl)
  | Inst inst ->
    concat_map inst.logic.all_names ~f:(write_var ~write_attrs:false inst.logic)
  | Multiport_memory { memory; depth; range = r; _ } ->
    let range = range_compat r in
    let comment = get_comment memory.comment in
    let attr = write_attr memory in
    [%rope "%{attr}%{tab}reg%{range} %{memory.name}[0:%{(depth-1)#Int}]%{comment};\n"]
;;

let declarations Rtl_ast.{ declarations; config = { two_state; _ }; _ } =
  List.map declarations ~f:(declaration ~two_state) |> Rope.concat
;;

let rec write_always_statements indent (t : Rtl_ast.always) =
  let block indent ts ~f =
    match ts with
    | [ _ ] -> f (Rope.concat [ indent; tab ])
    | _ ->
      Rope.concat
        [ [%rope "%{indent}begin\n"]
        ; f (Rope.concat [ indent; tab ])
        ; [%rope "%{indent}end\n"]
        ]
  in
  let cond (c : Rtl_ast.condition) =
    match c with
    | Level { level = High; var } | Edge { edge = Rising; var } -> [%rope "%{var.name}"]
    | Level { level = Low; var } | Edge { edge = Falling; var } ->
      [%rope "%{var.name} == 0"]
    | Clock _ -> (* clocks are not written in verilog *) assert false
  in
  match t with
  | If { condition = Clock _; on_true; on_false = [] } ->
    concat_map on_true ~f:(write_always_statements indent)
  | If { condition; on_true; on_false = [] } ->
    Rope.concat
      [ [%rope "%{indent}if (%{cond condition})\n"]
      ; block indent on_true ~f:(fun indent ->
          concat_map on_true ~f:(write_always_statements indent))
      ]
  | If { condition; on_true; on_false } ->
    Rope.concat
      [ [%rope "%{indent}if (%{cond condition})\n"]
      ; block indent on_true ~f:(fun indent ->
          concat_map on_true ~f:(write_always_statements indent))
      ; [%rope "%{indent}else\n"]
      ; block indent on_false ~f:(fun indent ->
          concat_map on_false ~f:(write_always_statements indent))
      ]
  | Assignment { lhs; rhs } -> [%rope "%{indent}%{lhs.name} <= %{rhs.name};\n"]
  | Memory_assignment { lhs; index; rhs } ->
    [%rope "%{indent}%{lhs.name}[%{index.name}] <= %{rhs.name};\n"]
  | Constant_memory_assignment { lhs; index; value } ->
    [%rope "%{indent}%{lhs.name}[%{index#Int}] <= %{verilog_constant value};\n"]
  | Case { select; cases } ->
    Rope.concat
      [ [%rope "%{indent}case (%{select.name})\n"]
      ; concat_map cases ~f:(fun case ->
          let case, statements =
            match case with
            | { match_with = Int index; statements } ->
              [%rope "%{indent}%{index#Int}:\n"], statements
            | { match_with = Const bits; statements } ->
              [%rope "%{indent}%{verilog_constant bits}:\n"], statements
            | { match_with = Default; statements } ->
              [%rope "%{indent}default:\n"], statements
          in
          Rope.concat
            [ case
            ; block indent statements ~f:(fun indent ->
                concat_map statements ~f:(write_always_statements indent))
            ])
      ; [%rope "%{indent}endcase\n"]
      ]
;;

let write_always (sensitivity_list : Rtl_ast.sensitivity_list) always =
  let sensitivity_list =
    match sensitivity_list with
    | Star -> [%rope "*"]
    | Edges edges ->
      let edges =
        concat_map ~sep:[%rope " or "] edges ~f:(fun { edge; var } ->
          match edge with
          | Rising -> [%rope "posedge %{var.name}"]
          | Falling -> [%rope "negedge %{var.name}"])
      in
      [%rope "(%{edges})"]
  in
  Rope.concat
    [ [%rope "%{tab}always @%{sensitivity_list} begin\n"]
    ; write_always_statements tab2 always
    ; [%rope "%{tab}end\n"]
    ]
;;

let write_initial always =
  Rope.concat
    [ [%rope "%{tab}initial begin\n"]
    ; concat_map always ~f:(fun always -> write_always_statements tab2 always)
    ; [%rope "%{tab}end\n"]
    ]
;;

let parameter_value (p : Parameter.t) =
  let rec value_to_string (value : Parameter.Value.t) =
    match value with
    | String v -> [%string {|"%{v}"|}]
    | Int v -> [%string.global "%{v#Int}"]
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
      (* According to the modelsim manual, you must encode the enumeration index. I
         suspect this is not especially portable. *)
      [%string "4'd%{(Logic.Std_logic.to_int b)#Int}"]
    | Array l ->
      let array = List.map l ~f:value_to_string |> String.concat ~sep:", " in
      [%string "{%{array}}"]
  in
  value_to_string p.value |> Rope.of_string
;;

let write_instantiation (instantiation : Rtl_ast.instantiation) =
  Rope.concat
    [ write_attributes instantiation.attributes
    ; [%rope "%{tab}%{instantiation.name#String}\n"]
    ; (let sep = [%rope ",\n%{spaces (tab_len*2 + 3)}"] in
       match instantiation.parameters with
       | [] -> Rope.empty
       | parameters ->
         let parameters =
           concat_map ~sep parameters ~f:(fun p ->
             let name, value = Parameter_name.to_string p.name, parameter_value p in
             [%rope ".%{name#String}(%{value})"])
         in
         [%rope "%{tab2}#( %{parameters} )\n"])
    ; [%rope "%{tab2}%{instantiation.instance#String}\n"]
    ; (let sep = [%rope ",\n%{spaces (tab_len*2 + 2)}"] in
       let input_ports =
         List.map instantiation.input_ports ~f:(fun { port_name; connection } ->
           [%rope ".%{port_name#String}(%{connection.name})"])
       in
       let output_ports =
         match instantiation.output_ports with
         | [ { port_name; connection; high = 0; low = 0 } ] ->
           [ [%rope ".%{port_name#String}(%{connection.name})"] ]
         | output_ports ->
           List.map output_ports ~f:(fun { port_name; connection; high; low } ->
             [%rope ".%{port_name#String}(%{connection.name}[%{high#Int}:%{low#Int}])"])
       in
       let ports = Rope.concat ~sep (input_ports @ output_ports) in
       [%rope "%{tab2}( %{ports} );\n"])
    ]
;;

let operator (op : Rtl_ast.binop) =
  Rope.of_string
    (match op with
     | Add -> "+"
     | Sub -> "-"
     | And -> "&"
     | Or -> "|"
     | Xor -> "^"
     | Mulu -> "*"
     | Muls -> "*"
     | Eq -> "=="
     | Lt -> "<")
;;

let rec statement (stat : Rtl_ast.statement) =
  match stat with
  | Assignment (Not { lhs; arg }) -> [%rope "%{tab}assign %{lhs.name} = ~ %{arg.name};\n"]
  | Assignment (Concat { lhs; args }) ->
    let sep = [%rope ",\n%{spaces (Rope.length lhs.name + ((tab_len * 3) + 4))}"] in
    let args = concat_map ~sep args ~f:(fun arg -> [%rope "%{arg.name}"]) in
    [%rope "%{tab}assign %{lhs.name} = { %{args} };\n"]
  | Assignment (Binop { lhs; arg_a; op = Muls; arg_b }) ->
    [%rope
      "%{tab}assign %{lhs.name} = $signed(%{arg_a.name}) %{operator Muls} \
       $signed(%{arg_b.name});\n"]
  | Assignment (Binop { lhs; arg_a; op; arg_b }) ->
    [%rope "%{tab}assign %{lhs.name} = %{arg_a.name} %{operator op} %{arg_b.name};\n"]
  | Assignment (Wire { lhs; driver }) ->
    [%rope "%{tab}assign %{lhs.name} = %{driver.name};\n"]
  | Assignment (Select { lhs; arg; high; low }) ->
    [%rope "%{tab}assign %{lhs.name} = %{arg.name}[%{high#Int}:%{low#Int}];\n"]
  | Assignment (Const { lhs; constant }) ->
    [%rope "%{tab}assign %{lhs.name} = %{verilog_constant constant};\n"]
  | Assignment (Mux { lhs; select; cases = [ on_false; on_true ] }) ->
    [%rope
      "%{tab}assign %{lhs.name} = %{select.name} ? %{on_true.name} : %{on_false.name};\n"]
  | Assignment (Mux _) -> (* In verilog, these cases are written by always *) assert false
  | Mux { to_assignment; to_always; is_mux2 } ->
    if is_mux2 then statement (to_assignment ()) else statement (to_always ())
  | Always { sensitivity_list; always } -> write_always sensitivity_list always
  | Initial { always } -> write_initial always
  | Instantiation instantiation -> write_instantiation instantiation
  | Multiport_mem { always; initial } ->
    Rope.concat
      [ concat_map always ~f:statement
      ; Option.value_map ~default:Rope.empty initial ~f:statement
      ]
  | Mem_read_port { lhs; memory; address } ->
    [%rope "%{tab}assign %{lhs.name} = %{memory.name}[%{address.name}];\n"]
;;

let statements (ast : Rtl_ast.t) = concat_map ast.statements ~f:statement

let output_assignments (ast : Rtl_ast.t) =
  concat_map ast.outputs ~f:(fun { output; driven_by } ->
    Option.value_map ~default:Rope.empty driven_by ~f:(fun driven_by ->
      [%rope "%{tab}assign %{output.name} = %{driven_by.name};\n"]))
;;

let write_alias (decl : Rtl_ast.declaration) =
  match decl with
  | Logic decl ->
    concat_map decl.all_names ~f:(fun var ->
      if not (Rtl_ast.equal_var var decl.write)
      then [%rope "%{tab}assign %{var.name} = %{decl.write.name};\n"]
      else Rope.empty)
  (* We dont allow naming in the following cases, so no aliases *)
  | Multiport_memory _ -> Rope.empty
  | Inst _ -> Rope.empty
;;

let write_aliases (ast : Rtl_ast.t) = concat_map ast.declarations ~f:write_alias

let to_rope (ast : Rtl_ast.t) =
  Rope.concat
    [ declare_io_ports ast
    ; declarations ast
    ; statements ast
    ; write_aliases ast
    ; output_assignments ast
    ; [%rope "\nendmodule\n"]
    ]
;;
