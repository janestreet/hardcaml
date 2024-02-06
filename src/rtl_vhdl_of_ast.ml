open Base

let tab_len = 4
let tab = String.init tab_len ~f:(Fn.const ' ')
let tab2 = tab ^ tab
let spaces n = String.init n ~f:(Fn.const ' ')

let slv (range : Rtl_ast.range) =
  match range with
  | Bit -> "std_logic"
  | Vector { width } -> [%string "std_logic_vector(%{(width-1)#Int} downto 0)"]
;;

let input name range = [%string "%{name} : in %{slv range}"]
let output name range = [%string "%{name} : out %{slv range}"]

let declare_io_ports buffer (ast : Rtl_ast.t) =
  let add_string = Buffer.add_string buffer in
  let hc = "hc_" in
  let io_ports =
    String.concat
      ~sep:[%string ";\n%{tab2}"]
      (List.concat
         [ List.map ast.inputs ~f:(fun { name; range; _ } -> input name range)
         ; List.map ast.outputs ~f:(fun { output = { name; range; _ }; driven_by = _ } ->
             output name range)
         ])
  in
  add_string
    [%string
      {|library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity %{ast.name} is
%{tab}port (
%{tab2}%{io_ports}
%{tab});
end entity;

architecture rtl of %{ast.name} is

%{tab}-- conversion functions
%{tab}function %{hc}uns(a : std_logic)        return unsigned         is variable b : unsigned(0 downto 0); begin b(0) := a; return b; end;
%{tab}function %{hc}uns(a : std_logic_vector) return unsigned         is begin return unsigned(a); end;
%{tab}function %{hc}sgn(a : std_logic)        return signed           is variable b : signed(0 downto 0); begin b(0) := a; return b; end;
%{tab}function %{hc}sgn(a : std_logic_vector) return signed           is begin return signed(a); end;
%{tab}function %{hc}sl (a : std_logic_vector) return std_logic        is begin return a(a'right); end;
%{tab}function %{hc}sl (a : unsigned)         return std_logic        is begin return a(a'right); end;
%{tab}function %{hc}sl (a : signed)           return std_logic        is begin return a(a'right); end;
%{tab}function %{hc}sl (a : boolean)          return std_logic        is begin if a then return '1'; else return '0'; end if; end;
%{tab}function %{hc}slv(a : std_logic_vector) return std_logic_vector is begin return a; end;
%{tab}function %{hc}slv(a : unsigned)         return std_logic_vector is begin return std_logic_vector(a); end;
%{tab}function %{hc}slv(a : signed)           return std_logic_vector is begin return std_logic_vector(a); end;
|}]
;;

let raise_vhdl_doesnt_support_attributes_yet () =
  raise_s [%message "[Rtl_vhdl_of_ast] Signal attributes are not supported in VHDL yet"]
;;

let vhdl_doesnt_support_attributes_yet attributes =
  let raise_on_attributes = false in
  if raise_on_attributes && not (List.is_empty attributes)
  then raise_vhdl_doesnt_support_attributes_yet ()
;;

let declaration buffer (decl : Rtl_ast.declaration) =
  let add_string = Buffer.add_string buffer in
  let write_var (var : Rtl_ast.var) =
    vhdl_doesnt_support_attributes_yet var.attributes;
    add_string [%string "%{tab}signal %{var.name} : %{slv var.range};\n"]
  in
  match decl with
  | Logic decl -> List.iter decl.all_names ~f:write_var
  | Inst inst -> List.iter inst.all_names ~f:write_var
  | Multiport_memory { memory; memory_type; depth; range = _ } ->
    vhdl_doesnt_support_attributes_yet memory.attributes;
    add_string
      [%string
        "%{tab}type %{memory_type} is array (0 to %{(depth-1)#Int}) of %{slv \
         memory.range};\n"];
    add_string [%string "%{tab}signal %{memory.name} : %{memory_type};\n"]
;;

let declarations buffer (ast : Rtl_ast.t) =
  List.iter ast.declarations ~f:(declaration buffer)
;;

let rec write_always indent buffer (t : Rtl_ast.always) =
  let add_string = Buffer.add_string buffer in
  let cond (c : Rtl_ast.condition) =
    match c with
    | Level { level = High; var } -> [%string "%{var.name} = '1'"]
    | Level { level = Low; var } -> [%string "%{var.name} = '0'"]
    | Edge { edge = Rising; var } | Clock { edge = Rising; clock = var } ->
      [%string "rising_edge(%{var.name})"]
    | Edge { edge = Falling; var } | Clock { edge = Falling; clock = var } ->
      [%string "falling_edge(%{var.name})"]
  in
  match t with
  | If { condition; on_true; on_false = [] } ->
    add_string [%string "%{indent}if %{cond condition} then\n"];
    List.iter on_true ~f:(write_always (indent ^ tab) buffer);
    add_string [%string "%{indent}end if;\n"]
  | If { condition; on_true; on_false } ->
    add_string [%string "%{indent}if %{cond condition} then\n"];
    List.iter on_true ~f:(write_always (indent ^ tab) buffer);
    add_string [%string "%{indent}else\n"];
    List.iter on_false ~f:(write_always (indent ^ tab) buffer);
    add_string [%string "%{indent}end if;\n"]
  | Assignment { lhs; rhs } ->
    add_string [%string "%{indent}%{lhs.name} <= %{rhs.name};\n"]
  | Memory_assignment { lhs; index; rhs } ->
    add_string
      [%string
        "%{indent}%{lhs.name}(to_integer(hc_uns(%{index.name}))) <= %{rhs.name};\n"]
  | Case { select; cases } ->
    add_string [%string "%{indent}case to_integer(%{select.name}) is\n"];
    let num_cases = List.length cases in
    List.iteri cases ~f:(fun index case ->
      if index = num_cases - 1
      then add_string [%string "%{indent}when others =>\n"]
      else add_string [%string "%{indent}when %{index#Int} =>\n"];
      List.iter case ~f:(write_always indent buffer));
    add_string [%string "%{indent}end case;\n"]
;;

let write_always buffer (sensitivity_list : Rtl_ast.sensitivity_list) always =
  let add_string = Buffer.add_string buffer in
  let sensitivity_list =
    match sensitivity_list with
    | Star ->
      raise_s [%message "[Rtl_vhdl_of_ast] VHDL does not support [Star] sensitivity"]
    | Edges edges ->
      let edges =
        List.map edges ~f:(fun { edge = _; var } -> var.name) |> String.concat ~sep:", "
      in
      [%string "(%{edges})"]
  in
  add_string [%string "%{tab}process %{sensitivity_list} begin\n"];
  write_always tab2 buffer always;
  add_string [%string "%{tab}end process;\n"]
;;

let parameter_value (p : Parameter.t) =
  match p.value with
  | String v -> [%string {|"%{v}"|}]
  | Int v -> [%string "%{v#Int}"]
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
    (* According to the modelsim manual, you must encode the enumeration index. I suspect
       this is not especially portable. *)
    [%string "'%{(Logic.Std_logic.to_char b)#Char}'"]
;;

let write_instantiation buffer (instantiation : Rtl_ast.instantiation) =
  let add_string = Buffer.add_string buffer in
  add_string
    [%string "%{tab}%{instantiation.instance}: entity work.%{instantiation.name} (rtl)\n"];
  (match instantiation.parameters with
   | [] -> ()
   | parameters ->
     let parameters =
       String.concat
         ~sep:[%string ",\n%{spaces (tab_len*3 + 10)}"]
         (List.map parameters ~f:(fun p ->
            let name, value = Parameter_name.to_string p.name, parameter_value p in
            [%string "%{name} => %{value}"]))
     in
     add_string [%string "%{tab2}generic map ( %{parameters} )\n"]);
  let input_ports =
    List.map instantiation.input_ports ~f:(fun { port_name; connection } ->
      [%string "%{port_name} => %{connection.name}"])
  in
  let output_ports =
    match instantiation.output_ports with
    | [ { port_name; connection; high = 0; low = 0 } ] ->
      [ [%string "%{port_name} => %{connection.name}"] ]
    | output_ports ->
      List.map output_ports ~f:(fun { port_name; connection; high; low } ->
        if high = low
        then [%string "%{port_name} => %{connection.name}(%{low#Int})"]
        else [%string "%{port_name} => %{connection.name}(%{high#Int} downto %{low#Int})"])
  in
  let ports =
    String.concat
      ~sep:[%string ",\n%{spaces (tab_len*3 + 7)}"]
      (input_ports @ output_ports)
  in
  add_string [%string "%{tab2}port map ( %{ports} );\n"]
;;

let operator : Rtl_ast.binop -> string = function
  | Add -> "+"
  | Sub -> "-"
  | And -> "and"
  | Or -> "or"
  | Xor -> "xor"
  | Mulu -> "*"
  | Muls -> "*"
  | Eq -> "="
  | Lt -> "<"
;;

let rec statement buffer (stat : Rtl_ast.statement) =
  let add_string = Buffer.add_string buffer in
  let assign (lhs : Rtl_ast.var) expr =
    if Rtl_ast.equal_range Bit lhs.range
    then [%string "%{tab}%{lhs.name} <= hc_sl(%{expr});\n"]
    else [%string "%{tab}%{lhs.name} <= hc_slv(%{expr});\n"]
  in
  match stat with
  | Assignment (Not { lhs; arg }) ->
    add_string (assign lhs [%string "not hc_uns(%{arg.name})"])
  | Assignment (Concat { lhs; args }) ->
    let args = List.map args ~f:(fun arg -> arg.name) |> String.concat ~sep:" & " in
    add_string [%string "%{tab}%{lhs.name} <= %{args};\n"]
  | Assignment (Binop { lhs; arg_a; op; arg_b; signed = false }) ->
    add_string
      (assign lhs [%string "hc_uns(%{arg_a.name}) %{operator op} hc_uns(%{arg_b.name})"])
  | Assignment (Binop { lhs; arg_a; op; arg_b; signed = true }) ->
    add_string
      (assign lhs [%string "hc_sgn(%{arg_a.name}) %{operator op} hc_sgn(%{arg_b.name})"])
  | Assignment (Wire { lhs; driver }) ->
    add_string [%string "%{tab}%{lhs.name} <= %{driver.name};\n"]
  | Assignment (Select { lhs; arg; high; low }) ->
    if Rtl_ast.equal_range Bit lhs.range
    then
      add_string
        [%string
          "%{tab}%{lhs.name} <= hc_sl(%{arg.name}(%{high#Int} downto %{low#Int}));\n"]
    else
      add_string
        [%string "%{tab}%{lhs.name} <= %{arg.name}(%{high#Int} downto %{low#Int});\n"]
  | Assignment (Const { lhs; constant }) ->
    if Bits.width constant = 1
    then add_string [%string "%{tab}%{lhs.name} <= '%{Bits.to_bstr constant}';\n"]
    else add_string [%string "%{tab}%{lhs.name} <= \"%{Bits.to_bstr constant}\";\n"]
  | Assignment (Mux { lhs; select; cases }) ->
    let num_cases = List.length cases in
    let cases =
      String.concat
        ~sep:[%string ",\n%{tab2}"]
        (List.mapi cases ~f:(fun i case ->
           if i = num_cases - 1
           then [%string "%{case.name} when others"]
           else [%string "%{case.name} when %{i#Int}"]))
    in
    add_string
      [%string
        "%{tab}with to_integer(hc_uns(%{select.name})) select %{lhs.name} <=\n\
         %{tab2}%{cases};\n"]
  | Mux { to_assignment; to_always = _; is_mux2 = _ } ->
    statement buffer (to_assignment ())
  | Always { sensitivity_list; always } -> write_always buffer sensitivity_list always
  | Instantiation instantiation -> write_instantiation buffer instantiation
  | Multiport_mem { always } -> Array.iter always ~f:(statement buffer)
  | Mem_read_port { lhs; memory; address } ->
    add_string
      [%string
        "%{tab}%{lhs.name} <= %{memory.name}(to_integer(hc_uns(%{address.name})));\n"]
;;

let statements buffer (ast : Rtl_ast.t) = List.iter ast.statements ~f:(statement buffer)

let output_assignments buffer (ast : Rtl_ast.t) =
  let add_string = Buffer.add_string buffer in
  List.iter ast.outputs ~f:(fun { output; driven_by } ->
    Option.iter driven_by ~f:(fun driven_by ->
      add_string [%string "%{tab}%{output.name} <= %{driven_by.name};\n"]))
;;

let write_alias buffer (decl : Rtl_ast.declaration) =
  let add_string = Buffer.add_string buffer in
  match decl with
  | Logic decl ->
    List.iter decl.all_names ~f:(fun var ->
      if not (Rtl_ast.equal_var var decl.write)
      then add_string [%string "%{tab}%{var.name} <= %{decl.write.name};\n"])
  (* We dont allow naming in the following cases, so no aliases *)
  | Multiport_memory _ -> ()
  | Inst _ -> ()
;;

let write_aliases buffer (ast : Rtl_ast.t) =
  List.iter ast.declarations ~f:(write_alias buffer)
;;

let to_buffer buffer ast =
  let add_string = Buffer.add_string buffer in
  declare_io_ports buffer ast;
  declarations buffer ast;
  add_string "\nbegin\n\n";
  statements buffer ast;
  write_aliases buffer ast;
  output_assignments buffer ast;
  add_string "\nend architecture;\n"
;;
