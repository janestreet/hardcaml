open! Core0

(* Type of a VHDL attribute. *)
module Attribute_type = struct
  module Type = struct
    type t =
      | Int
      | String
      | Bool
    [@@deriving sexp_of, equal ~localize]

    let to_string = function
      | Int -> "integer"
      | String -> "string"
      | Bool -> "boolean"
    ;;
  end

  type t =
    { name : string
    ; type_ : Type.t
    }
  [@@deriving sexp_of]

  (* Map of all attributes in the design. An attribute may be present multiple times in
     the design, but must only have one type. The following ensures this. *)
  let add_if_unique (attrs : t Map.M(String.Caseless).t) (attr : Rtl_attribute.t) =
    Option.value_map ~default:attrs (Rtl_attribute.value attr) ~f:(function value ->
      let name = Rtl_attribute.name attr in
      let attr =
        match value with
        | Int _ -> { name; type_ = Int }
        | String _ -> { name; type_ = String }
        | Bool _ -> { name; type_ = Bool }
      in
      (match Map.find attrs name with
       | Some { name; type_ } ->
         if Type.equal type_ attr.type_
         then attrs
         else
           raise_s
             [%message
               "Attribute used with two different types"
                 (name : string)
                 (type_ : Type.t)
                 (attr.type_ : Type.t)]
       | None -> Map.add_exn attrs ~key:name ~data:attr))
  ;;

  let add_from_var attrs (var : Rtl_ast.var) =
    List.fold var.attributes ~init:attrs ~f:add_if_unique
  ;;
end

let tab_len = 4
let spaces n = String.init n ~f:(Fn.const ' ') |> Rope.of_string
let tab = spaces 4
let tab2 = spaces 8
let concat_map ?sep t ~f = Rope.concat ?sep (List.map t ~f)

let slv (range : Rtl_ast.range) two_state =
  match range with
  | Bit -> if two_state then [%rope "bit"] else [%rope "std_logic"]
  | Vector { width } ->
    if two_state
    then [%rope "bit_vector(%{(width-1)#Int} downto 0)"]
    else [%rope "std_logic_vector(%{(width-1)#Int} downto 0)"]
;;

let to_integer (var : Rtl_ast.var) (two_state : bool) =
  if Rtl_ast.equal_range var.range Bit
  then
    if two_state
    then [%rope {|to_integer(unsigned'("" & %{var.name}))|}]
    else [%rope {|to_integer(unsigned(std_logic_vector'("" & %{var.name})))|}]
  else [%rope "to_integer(unsigned(%{var.name}))"]
;;

(* Always convert to std_logic[{,_vector}] from the current variable (which may or may not
   be 2-state) *)
let to_std_logic_vector (var : Rtl_ast.var) (two_state : bool) =
  if two_state
  then
    if Rtl_ast.equal_range var.range Bit
    then [%rope {|to_stdlogic(%{var.name})|}]
    else [%rope {|to_stdlogicvector(%{var.name})|}]
  else [%rope {|%{var.name}|}]
;;

(* Always convert from std_logic{,_vector} to the current mode *)
let of_std_logic_vector ~range inner (two_state : bool) =
  if two_state
  then
    if Rtl_ast.equal_range range Bit
    then [%rope {|to_bit(%{inner})|}]
    else [%rope {|to_bitvector(%{inner})|}]
  else [%rope {|%{inner}|}]
;;

let input name range two_state = [%rope "%{name} : in %{slv range two_state}"]
let output name range two_state = [%rope "%{name} : out %{slv range two_state}"]

let attribute ~name ~class_ (attr : Rtl_attribute.t) =
  match Rtl_attribute.value attr with
  | None ->
    (* In verilog we can have attributes with no value, but I am not sure how to do that
       in vhdl - if you can at all. *)
    Rope.empty
  | Some attr_value ->
    let attr_value =
      match attr_value with
      | String s -> [%rope {|"%{s#String}"|}]
      | Int i -> [%rope "%{i#Int}"]
      | Bool b -> if b then [%rope "true"] else [%rope "false"]
    in
    [%rope
      "%{tab}attribute %{Rtl_attribute.name attr#String} of %{name} : %{class_#String} \
       is %{attr_value};\n"]
;;

let attributes ~name ~class_ attrs = concat_map attrs ~f:(attribute ~name ~class_)

let attributes_of_var ~class_ (var : Rtl_ast.var) =
  attributes ~name:var.name ~class_ var.attributes
;;

let numeric_lib two_state =
  Rope.of_string (if two_state then "numeric_bit" else "numeric_std")
;;

let preamble two_state =
  if two_state
  then
    [%rope
      {|%{tab}-- Conversions
%{tab}function to_stdlogic(i : in bit) return std_logic is
%{tab}begin
%{tab}%{tab}if i = '0' then
%{tab}%{tab}%{tab}return '0';
%{tab}%{tab}else
%{tab}%{tab}%{tab}return '1';
%{tab}%{tab}end if;
%{tab}end function;
|}]
  else Rope.empty
;;

let declare_io_ports (ast : Rtl_ast.t) (attrs : Attribute_type.t list) : Rope.t =
  let two_state = ast.config.two_state in
  let io_ports =
    Rope.concat
      ~sep:[%rope ";\n%{tab2}"]
      (List.concat
         [ List.map ast.inputs ~f:(fun { name; range; _ } -> input name range two_state)
         ; List.map ast.outputs ~f:(fun { output = { name; range; _ }; driven_by = _ } ->
             output name range two_state)
         ])
  in
  let attribute_types =
    match attrs with
    | [] -> Rope.empty
    | _ ->
      concat_map attrs ~f:(fun { name; type_ } ->
        [%rope "%{tab}attribute %{name#String} : %{type_#Attribute_type.Type};\n"])
  in
  let attributes =
    Rope.concat
      [ concat_map ast.inputs ~f:(attributes_of_var ~class_:"signal")
      ; concat_map ast.outputs ~f:(fun { output; _ } ->
          attributes_of_var ~class_:"signal" output)
      ]
  in
  let two_state = ast.config.two_state in
  [%rope
    {|library ieee;
use ieee.std_logic_1164.all;
use ieee.%{numeric_lib two_state}.all;

entity %{ast.name#String} is
%{tab}port (
%{tab2}%{io_ports}
%{tab});
%{attribute_types}%{attributes}end entity;

architecture rtl of %{ast.name#String} is
%{preamble two_state}
|}]
;;

let vhdl_constant constant =
  if Bits.width constant = 1
  then [%rope "'%{Bits.to_bstr constant#String}'"]
  else [%rope "\"%{Bits.to_bstr constant#String}\""]
;;

let declaration (decl : Rtl_ast.declaration) ~two_state =
  let write_var ~write_attrs (decl : Rtl_ast.logic_declaration) (var : Rtl_ast.var) =
    let initialize_to =
      Option.value_map ~default:Rope.empty decl.initialize_to ~f:(fun i ->
        [%rope " := %{vhdl_constant i}"])
    in
    let attrs =
      if write_attrs then attributes_of_var ~class_:"signal" var else Rope.empty
    in
    [%rope
      "%{tab}signal %{var.name} : %{slv var.range two_state}%{initialize_to};\n%{attrs}"]
  in
  match decl with
  | Logic decl -> concat_map decl.all_names ~f:(write_var ~write_attrs:true decl)
  | Inst { logic; instance_name } ->
    Rope.concat
      [ concat_map logic.all_names ~f:(write_var ~write_attrs:false logic)
      ; attributes
          ~name:[%rope "%{instance_name#String}"]
          ~class_:"label"
          logic.write.attributes
      ]
  | Multiport_memory { memory; memory_type; depth; range = _ } ->
    let attrs = attributes_of_var ~class_:"signal" memory in
    Rope.concat
      [ [%rope
          "%{tab}type %{memory_type#String} is array (0 to %{(depth-1)#Int}) of %{slv \
           memory.range false};\n"]
      ; [%rope "%{tab}signal %{memory.name} : %{memory_type#String};\n"]
      ; attrs
      ]
;;

let declarations (ast : Rtl_ast.t) =
  concat_map ast.declarations ~f:(declaration ~two_state:ast.config.two_state)
;;

let rec case_types_are_integer (cases : Rtl_ast.case list) =
  match cases with
  | [] -> raise_s [%message "[Rtl_vhdl_of_ast] Unable to infer case type"]
  | { match_with = Default; statements = _ } :: cases -> case_types_are_integer cases
  | { match_with = Int _; statements = _ } :: _ -> true
  | { match_with = Const _; statements = _ } :: _ -> false
;;

let rec write_always_statements indent (two_state : bool) (t : Rtl_ast.always) =
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
    | Level { level = High; var } -> [%rope "%{var.name} = '1'"]
    | Level { level = Low; var } -> [%rope "%{var.name} = '0'"]
    | Edge { edge = Rising; var } | Clock { edge = Rising; clock = var } ->
      [%rope "rising_edge(%{var.name})"]
    | Edge { edge = Falling; var } | Clock { edge = Falling; clock = var } ->
      [%rope "falling_edge(%{var.name})"]
  in
  match t with
  | If { condition; on_true; on_false = [] } ->
    Rope.concat
      [ [%rope "%{indent}if %{cond condition} then\n"]
      ; block indent on_true ~f:(fun indent ->
          concat_map on_true ~f:(write_always_statements indent two_state))
      ; [%rope "%{indent}end if;\n"]
      ]
  | If { condition; on_true; on_false } ->
    Rope.concat
      [ [%rope "%{indent}if %{cond condition} then\n"]
      ; block indent on_true ~f:(fun indent ->
          concat_map on_true ~f:(write_always_statements indent two_state))
      ; [%rope "%{indent}else\n"]
      ; block indent on_false ~f:(fun indent ->
          concat_map on_false ~f:(write_always_statements indent two_state))
      ; [%rope "%{indent}end if;\n"]
      ]
  | Assignment { lhs; rhs } -> [%rope "%{indent}%{lhs.name} <= %{rhs.name};\n"]
  | Memory_assignment { lhs; index; rhs } ->
    [%rope
      "%{indent}%{lhs.name}(%{to_integer index two_state}) <= %{to_std_logic_vector rhs \
       two_state};\n"]
  | Constant_memory_assignment { lhs; index; value } ->
    [%rope "%{indent}%{lhs.name}(%{index#Int}) <= %{vhdl_constant value};\n"]
  | Case { select; cases } ->
    Rope.concat
      [ (if case_types_are_integer cases
         then [%rope "%{indent}case %{to_integer select two_state} is\n"]
         else [%rope "%{indent}case %{select.name} is\n"])
      ; concat_map cases ~f:(fun case ->
          let case, statements =
            match case with
            | { match_with = Int index; statements } ->
              [%rope "%{indent}when %{index#Int} =>\n"], statements
            | { match_with = Const bits; statements } ->
              [%rope "%{indent}when %{vhdl_constant bits} =>\n"], statements
            | { match_with = Default; statements } ->
              [%rope "%{indent}when others =>\n"], statements
          in
          Rope.concat
            [ case
            ; block indent statements ~f:(fun indent ->
                concat_map statements ~f:(write_always_statements indent two_state))
            ])
      ; [%rope "%{indent}end case;\n"]
      ]
;;

let write_always (two_state : bool) (sensitivity_list : Rtl_ast.sensitivity_list) always =
  let sensitivity_list =
    match sensitivity_list with
    | Star -> [%rope "(all)"]
    | Edges edges ->
      let edges =
        concat_map
          edges
          ~f:(fun { edge = _; var } -> [%rope "%{var.name}"])
          ~sep:[%rope ", "]
      in
      [%rope "(%{edges})"]
  in
  Rope.concat
    [ [%rope "%{tab}process %{sensitivity_list} begin\n"]
    ; always |> List.map ~f:(write_always_statements tab2 two_state) |> Rope.concat
    ; [%rope "%{tab}end process;\n"]
    ]
;;

let write_initial two_state always =
  Rope.concat
    [ [%rope "%{tab}process begin\n"]
    ; concat_map always ~f:(write_always_statements tab2 two_state)
    ; [%rope "%{tab2}wait;\n"]
    ; [%rope "%{tab}end process;\n"]
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
    | Bool b -> if b then "true" else "false"
    | Bit b -> if b then "'1'" else "'0'"
    | Std_logic_vector v ->
      [%string {|std_logic_vector'("%{Logic.Std_logic_vector.to_string v}")|}]
    | Std_ulogic_vector v ->
      [%string {|std_ulogic_vector'("%{(Logic.Std_logic_vector.to_string v)}")|}]
    | Bit_vector v -> [%string {|"%{(Logic.Bit_vector.to_string v)}"|}]
    | Std_logic b | Std_ulogic b ->
      (* According to the modelsim manual, you must encode the enumeration index. I
         suspect this is not especially portable. *)
      [%string "'%{(Logic.Std_logic.to_char b)#Char}'"]
    | Array l ->
      let array = List.map l ~f:value_to_string |> String.concat ~sep:", " in
      [%string "(%{array})"]
  in
  value_to_string p.value |> Rope.of_string
;;

let write_instantiation (instantiation : Rtl_ast.instantiation) =
  Rope.concat
    [ [%rope
        "%{tab}%{instantiation.instance#String}: entity \
         work.%{instantiation.name#String} (rtl)\n"]
    ; (match instantiation.parameters with
       | [] -> Rope.empty
       | parameters ->
         let parameters =
           concat_map ~sep:[%rope ",\n%{spaces (tab_len*3 + 10)}"] parameters ~f:(fun p ->
             let name, value = Parameter_name.to_string p.name, parameter_value p in
             [%rope "%{name#String} => %{value}"])
         in
         [%rope "%{tab2}generic map ( %{parameters} )\n"])
    ; (let input_ports =
         List.map instantiation.input_ports ~f:(fun { port_name; connection } ->
           [%rope "%{port_name#String} => %{connection.name}"])
       in
       let output_ports =
         match instantiation.output_ports with
         | [ { port_name; connection; high = 0; low = 0 } ] ->
           [ [%rope "%{port_name#String} => %{connection.name}"] ]
         | output_ports ->
           List.map output_ports ~f:(fun { port_name; connection; high; low } ->
             if high = low
             then [%rope "%{port_name#String} => %{connection.name}(%{low#Int})"]
             else
               [%rope
                 "%{port_name#String} => %{connection.name}(%{high#Int} downto \
                  %{low#Int})"])
       in
       let ports =
         Rope.concat
           ~sep:[%rope ",\n%{spaces (tab_len*3 + 7)}"]
           (input_ports @ output_ports)
       in
       [%rope "%{tab2}port map ( %{ports} );\n"])
    ]
;;

let operator (op : Rtl_ast.binop) =
  Rope.of_string
    (match op with
     | Add -> "+"
     | Sub -> "-"
     | And -> "and"
     | Or -> "or"
     | Xor -> "xor"
     | Mulu -> "*"
     | Muls -> "*"
     | Eq -> "?="
     | Lt -> "?<")
;;

let rec statement two_state (stat : Rtl_ast.statement) =
  let assign (lhs : Rtl_ast.var) expr = [%rope "%{tab}%{lhs.name} <= %{expr};\n"] in
  let std_logic_vector x = [%rope "std_logic_vector(%{x})"] in
  let bit_vector x = [%rope "bit_vector(%{x})"] in
  let assign_vec (lhs : Rtl_ast.var) expr =
    if Rtl_ast.equal_range Bit lhs.range
    then assign lhs [%rope {|(%{expr}) ?= "1"|}]
    else if two_state
    then assign lhs (bit_vector expr)
    else assign lhs (std_logic_vector expr)
  in
  let to_vector (signedness : Signedness.t) (a : Rtl_ast.var) =
    (* Parenthesization is handled below because bit conversions don't need extra parens *)
    let signedness e =
      match signedness with
      | Unsigned -> [%rope "unsigned%{e}"]
      | Signed -> [%rope "signed%{e}"]
    in
    if Rtl_ast.equal_range Bit a.range
    then
      signedness
        (if two_state
         then [%rope {|'("" & %{a.name})|}]
         else [%rope {|(std_logic_vector'("" & %{a.name}))|}])
    else signedness [%rope "(%{a.name})"]
  in
  match stat with
  | Assignment (Not { lhs; arg }) -> assign lhs [%rope "not %{arg.name}"]
  | Assignment (Concat { lhs; args }) ->
    let args = concat_map args ~f:(fun arg -> [%rope "%{arg.name}"]) ~sep:[%rope " & "] in
    assign lhs args
  | Assignment (Binop { lhs; arg_a; op = (Or | Xor | And) as op; arg_b }) ->
    assign lhs [%rope "%{arg_a.name} %{operator op} %{arg_b.name}"]
  | Assignment (Binop { lhs; arg_a; op = Muls; arg_b }) ->
    assign_vec
      lhs
      [%rope "%{to_vector Signed arg_a} %{operator Muls} %{to_vector Signed arg_b}"]
  | Assignment (Binop { lhs; arg_a; op = Mulu; arg_b }) ->
    assign_vec
      lhs
      [%rope "%{to_vector Unsigned arg_a} %{operator Mulu} %{to_vector Unsigned arg_b}"]
  | Assignment (Binop { lhs; arg_a; op = (Add | Sub) as op; arg_b }) ->
    assign_vec
      lhs
      [%rope "%{to_vector Unsigned arg_a} %{operator op} %{to_vector Unsigned arg_b}"]
  | Assignment (Binop { lhs; arg_a; op = (Lt | Eq) as op; arg_b }) ->
    assign
      lhs
      [%rope "%{to_vector Unsigned arg_a} %{operator op} %{to_vector Unsigned arg_b}"]
  | Assignment (Wire { lhs; driver }) -> assign lhs driver.name
  | Assignment (Select { lhs; arg; high; low }) ->
    if Rtl_ast.equal_range Bit lhs.range
    then assign lhs [%rope "%{arg.name}(%{low#Int})"]
    else assign lhs [%rope "%{arg.name}(%{high#Int} downto %{low#Int})"]
  | Assignment (Const { lhs; constant }) ->
    [%rope "%{tab}%{lhs.name} <= %{vhdl_constant constant};\n"]
  | Assignment (Mux { lhs; select; cases }) ->
    let num_cases = List.length cases in
    let cases =
      Rope.concat
        ~sep:[%rope ",\n%{tab2}"]
        (List.mapi cases ~f:(fun i case ->
           if i = num_cases - 1
           then [%rope "%{case.name} when others"]
           else [%rope "%{case.name} when %{i#Int}"]))
    in
    [%rope
      "%{tab}with %{to_integer select two_state} select %{lhs.name} <=\n\
       %{tab2}%{cases};\n"]
  | Mux { to_assignment; to_always = _; is_mux2 = _ } ->
    statement two_state (to_assignment ())
  | Always { sensitivity_list; always } ->
    write_always two_state sensitivity_list [ always ]
  | Initial { always } -> write_initial two_state always
  | Instantiation instantiation -> write_instantiation instantiation
  | Multiport_mem { always; initial } ->
    (* Convert multiport mems to have all of their write ports in one process, this is
       necessary to simulate multiple write ports in VHDL *)
    let all_write_clocks =
      List.concat_map always ~f:(function
        | Always { sensitivity_list; always = _ } ->
          (match sensitivity_list with
           | Star -> failwith "not allowed to have sensitivity (*) in a memory write port"
           | Edges edges -> edges)
        | _ -> failwith "not allowed to have non-always statements in a memory write port")
    in
    let all_write_statements =
      List.map always ~f:(function
        | Always { sensitivity_list = _; always } -> always
        | _ -> failwith "not allowed to have non-always statements in a memory write port")
    in
    Rope.concat
      [ (if List.is_empty all_write_statements
         then Rope.empty
         else write_always two_state (Edges all_write_clocks) all_write_statements)
      ; Option.value_map ~default:Rope.empty initial ~f:(statement two_state)
      ]
  | Mem_read_port { lhs; memory; address } ->
    let mem_read = [%rope "%{memory.name}(%{to_integer address two_state})"] in
    [%rope
      "%{tab}%{lhs.name} <= %{of_std_logic_vector ~range:lhs.range mem_read two_state};\n"]
;;

let statements (ast : Rtl_ast.t) =
  concat_map ast.statements ~f:(statement ast.config.two_state)
;;

let output_assignments (ast : Rtl_ast.t) =
  concat_map ast.outputs ~f:(fun { output; driven_by } ->
    Option.value_map ~default:Rope.empty driven_by ~f:(fun driven_by ->
      [%rope "%{tab}%{output.name} <= %{driven_by.name};\n"]))
;;

let write_alias (decl : Rtl_ast.declaration) =
  match decl with
  | Logic decl ->
    concat_map decl.all_names ~f:(fun var ->
      if not (Rtl_ast.equal_var var decl.write)
      then [%rope "%{tab}%{var.name} <= %{decl.write.name};\n"]
      else Rope.empty)
  (* We dont allow naming in the following cases, so no aliases *)
  | Multiport_memory _ -> Rope.empty
  | Inst _ -> Rope.empty
;;

let write_aliases (ast : Rtl_ast.t) = concat_map ast.declarations ~f:write_alias

(* Find attribute types in all ports and declarations. *)
let get_attribute_types (ast : Rtl_ast.t) =
  let add_decls ~attribute_types (decls : Rtl_ast.declaration list) =
    List.fold decls ~init:attribute_types ~f:(fun attrs decl ->
      match decl with
      | Logic { all_names; _ } | Inst { logic = { all_names; _ }; _ } ->
        List.fold all_names ~init:attrs ~f:Attribute_type.add_from_var
      | Multiport_memory { memory; _ } -> Attribute_type.add_from_var attrs memory)
  in
  let attribute_types =
    add_decls ~attribute_types:(Map.empty (module String.Caseless)) ast.declarations
  in
  let attribute_types =
    List.fold ~init:attribute_types ast.inputs ~f:Attribute_type.add_from_var
  in
  let attribute_types =
    List.fold ~init:attribute_types ast.outputs ~f:(fun attrs { output; _ } ->
      Attribute_type.add_from_var attrs output)
  in
  Map.data attribute_types
;;

let to_rope (ast : Rtl_ast.t) =
  let attribute_types = get_attribute_types ast in
  Rope.concat
    [ declare_io_ports ast attribute_types
    ; declarations ast
    ; [%rope "\nbegin\n\n"]
    ; statements ast
    ; write_aliases ast
    ; output_assignments ast
    ; [%rope "\nend architecture;\n"]
    ]
;;
