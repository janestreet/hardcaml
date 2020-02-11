(* generic rtl writing for vhdl/verilog *)

open! Import
open Signal
include Rtl_intf

(* utils *)

type io = string -> unit
type name = Signal.t -> string

type type_decl =
  | Input
  | Output
  | Wire
  | Reg
  | Constant of string

let tab n = String.init n ~f:(fun _ -> ' ')
let t4 = tab 4
let t8 = tab 8

let write_strings io f s =
  let s = Array.of_list s in
  let len = Array.length s in
  for i = 0 to len - 1 do
    io (f (i = 0) (i = len - 1) s.(i))
  done
;;

let rec sep s l =
  match l with
  | [] -> ""
  | [ a ] -> a
  | a :: b -> a ^ s ^ sep s b
;;

let str_map f s =
  let l = String.length s in
  let x = Bytes.create l in
  for i = 0 to l - 1 do
    Bytes.set x i (f s.[i])
  done;
  Bytes.to_string x
;;

let is_alpha = function
  | 'a' .. 'z' | 'A' .. 'Z' -> true
  | _ -> false
;;

let is_num = function
  | '0' .. '9' -> true
  | _ -> false
;;

module VerilogNames = struct
  let case_sensitive = true
  let prefix = "_"
  let reserved = Reserved_words.verilog

  let rec legalize name =
    (* Alpha or '_' are legal staring identifiers.  If this is not the case add the prefix
       and try again. *)
    if is_alpha name.[0] || Char.equal name.[0] '_'
    then
      (* alpha, num, _, $ are ok, replace invalid chars with '_' *)
      str_map
        (fun c ->
           if is_alpha c || is_num c || Char.equal c '_' || Char.equal c '$'
           then c
           else '_')
        name
    else legalize (prefix ^ name)
  ;;
end

module VhdlNames = struct
  let case_sensitive = false
  let prefix = "hc_"

  let reserved =
    Reserved_words.vhdl
    @ [ prefix ^ "uns"; prefix ^ "sgn"; prefix ^ "sl"; prefix ^ "slv" ]
  ;;

  let rec legalize name =
    (* Alpha are legal staring identifiers.  If this is not the case add the prefix and
       try again. *)
    if is_alpha name.[0]
    then
      (* alpha, num, _ are ok, replace invalid chars with '_' *)
      str_map
        (fun c -> if is_alpha c || is_num c || Char.equal c '_' then c else '_')
        name
    else legalize (prefix ^ name)
  ;;
end

(* control mapping of signals to their (various) names *)
module type SignalNaming = sig
  (* identifier case sensitivity *)
  val case_sensitive : bool

  (* given a name, turn it into a legal identifier *)
  val prefix : string
  val reserved : string list
  val legalize : string -> string
end

module type SignalNameManager = sig
  module UI : Map.Key with type t = Signal.Uid.t * int
  module SMap : Map.S with module Key := UI

  type mem_names =
    { arr : string
    ; typ : string
    ; t1 : string
    ; t2 : string
    }

  type name_map =
    { mangler : Mangler.t
    ; signals : string SMap.t
    ; mem : mem_names Uid_map.t
    ; inst_labels : string Uid_map.t
    }

  val reserved : string list
  val init : string list -> name_map
  val add_port : Signal.t -> name_map -> name_map
  val add_signal : Signal.t -> name_map -> name_map
  val signal_name : name_map -> Signal.t -> int -> string
  val mem_names : name_map -> Signal.t -> mem_names
  val inst_label : name_map -> Signal.t -> string
end

module SignalNameManager (S : SignalNaming) () = struct
  module UI = struct
    type t = Uid.t * int [@@deriving compare, sexp_of]

    include (val Comparator.make ~compare ~sexp_of_t)
  end

  module SMap = Map.Make (UI)

  let prefix = S.prefix
  let reserved = S.reserved

  type mem_names =
    { arr : string
    ; typ : string
    ; t1 : string
    ; t2 : string
    }
  [@@deriving sexp_of]

  type name_map =
    { mangler : Mangler.t
    ; signals : string SMap.t
    ; mem : mem_names Uid_map.t
    ; inst_labels : string Uid_map.t
    }
  [@@deriving sexp_of]

  (* mangle a name *)
  let mangle name nm = Mangler.mangle nm.mangler name
  let generate_name signal = S.prefix ^ Int64.to_string (uid signal)

  (* get list of names of signal, or auto generate one *)
  let names_of_signal signal =
    let n = names signal in
    if List.is_empty n then [ generate_name signal ] else n
  ;;

  let add_signal_name uid idx name nm =
    (* mangle *)
    let name = mangle name nm in
    (* add to signal map *)
    { nm with signals = Map.set nm.signals ~key:(uid, idx) ~data:name }
  ;;

  let add_signal_names signal nm =
    (* get names and legalize *)
    let names = names_of_signal signal in
    let names = List.map names ~f:S.legalize in
    (* add names to map *)
    fst
      (List.fold names ~init:(nm, 0) ~f:(fun (nm, idx) name ->
         add_signal_name (uid signal) idx name nm, idx + 1))
  ;;

  (* add reserved names to the mangler *)
  let init resv =
    let mangler = Mangler.create ~case_sensitive:S.case_sensitive in
    Mangler.add_identifiers_exn mangler resv;
    { mangler; signals = SMap.empty; mem = Uid_map.empty; inst_labels = Uid_map.empty }
  ;;

  (* Add port names, but ensure they are unique and legal and throw and error if not. *)
  let add_port signal nm =
    match names signal with
    | [] ->
      raise_s
        [%message
          "circuit ports must have a name"
            ~note:"This error should have been caught during circuit generation."
            ~port:(signal : Signal.t)]
    | [ name ] ->
      if not (String.equal (S.legalize name) name)
      then
        raise_s
          [%message
            "illegal port name"
              (name : string)
              ~legal_name:(S.legalize name : string)
              ~note:"Hardcaml will not change ports names."
              ~port:(signal : Signal.t)]
      else (
        match Mangler.find_index nm.mangler name with
        | Some _ ->
          raise_s
            [%message
              "port name has already been defined or matches a reserved identifier"
                ~port:(signal : Signal.t)]
        | None -> add_signal_name (uid signal) 0 name nm)
    | _ ->
      raise_s
        [%message
          "circuit ports may not have multiple names"
            ~note:"This error should have been caught during circuit generation."
            ~port:(signal : Signal.t)]
  ;;

  let add_mem signal nm =
    let name =
      List.hd_exn (names_of_signal signal)
      (* XXX check: may need to legalize these names *)
    in
    let name_arr = mangle (name ^ "_mem") nm in
    let name_typ = mangle (name ^ "_type") nm in
    let name_t1 = mangle (name ^ "_blk") nm in
    let name_t2 = mangle (name ^ "_idx") nm in
    { nm with
      mem =
        Map.set
          nm.mem
          ~key:(uid signal)
          ~data:{ arr = name_arr; typ = name_typ; t1 = name_t1; t2 = name_t2 }
    }
  ;;

  let add_inst iname signal nm =
    let iname = S.legalize iname in
    let name = mangle iname nm in
    { nm with inst_labels = Map.set nm.inst_labels ~key:(uid signal) ~data:name }
  ;;

  (* add signals names to map; deal with special cases *)
  let add_signal signal nm =
    (* signal names *)
    let nm = add_signal_names signal nm in
    (* special cases *)
    let nm =
      match signal with
      | Mem _ | Multiport_mem _ -> add_mem signal nm
      | Inst { instantiation; _ } -> add_inst instantiation.inst_instance signal nm
      | _ -> nm
    in
    nm
  ;;

  let raise_internal_error ~while_ ?index ~for_signal () =
    raise_s
      [%message
        (String.concat [ "[Rtl.SignalNameManager] internal error while "; while_ ])
          (index : (int option[@sexp.option]))
          (for_signal : Signal.t)]
  ;;

  let signal_name nm signal idx =
    let uid = uid signal in
    try Map.find_exn nm.signals (uid, idx) with
    | _ ->
      raise_internal_error
        ~while_:"looking up signal name"
        ~index:idx
        ~for_signal:signal
        ()
  ;;

  let mem_names nm signal =
    let uid = uid signal in
    try Map.find_exn nm.mem uid with
    | _ -> raise_internal_error ~while_:"looking up memory names" ~for_signal:signal ()
  ;;

  let inst_label nm signal =
    let uid = uid signal in
    try Map.find_exn nm.inst_labels uid with
    | _ ->
      raise_internal_error ~while_:"looking up instantiation label" ~for_signal:signal ()
  ;;
end

let raise_unexpected ~generator ~while_ ~got_signal =
  raise_s
    [%message
      (String.concat [ "unexpected signal type while "; while_ ])
        (generator : string)
        (got_signal : Signal.t)]
;;

let raise_expected ~generator ~while_ ~expected ~got_signal =
  raise_s
    [%message
      (String.concat [ "expected signal type"; while_ ])
        (expected : string)
        (generator : string)
        (got_signal : Signal.t)]
;;

(* regiser/memory process/always helper *)

module Process = struct
  (* helper for writing registers neatly *)
  type level =
    | Edge of Edge.t
    | Level of Level.t
  [@@deriving sexp_of]

  type stat =
    | If of Signal.t * level * stat * stat
    | Assign of Signal.t * Signal.t
    | Empty
  [@@deriving sexp_of]

  let stat_is_empty = function
    | Empty -> true
    | _ -> false
  ;;

  let make_reg ?(clock = true) () ~register ~signal ~data_in =
    let q = signal in
    let d = data_in in
    (* main assignment *)
    let e = Assign (q, d) in
    (* enable *)
    let e =
      if is_empty register.reg_enable || is_vdd register.reg_enable
      then e
      else If (register.reg_enable, Level High, e, Empty)
    in
    (* clear *)
    let e =
      if is_empty register.reg_clear
      then e
      else
        If
          ( register.reg_clear
          , Level register.reg_clear_level
          , Assign (q, register.reg_clear_value)
          , e )
    in
    (* reset and clock *)
    let clock e =
      if clock
      then If (register.reg_clock, Edge register.reg_clock_edge, e, Empty)
      else e
    in
    let e =
      if is_empty register.reg_reset
      then clock e
      else
        If
          ( register.reg_reset
          , Edge register.reg_reset_edge
          , Assign (q, register.reg_reset_value)
          , clock e )
    in
    e
  ;;
end

(* vhdl/verilog config *)

module type Rtl_internal = sig
  module Names : SignalNameManager

  val comment : string -> string

  val header_and_ports
    :  io:io
    -> name:string
    -> i:(string * int * Rtl_attribute.t list) list
    -> o:(string * int * Rtl_attribute.t list) list
    -> unit

  val signal_decl : io -> string -> Signal.t -> unit
  val alias_decl : io -> string -> Signal.t -> unit
  val mem_decl : io -> name -> Names.mem_names -> Signal.t -> unit
  val start_logic : io -> unit
  val logic : io -> name -> Signal.t -> unit
  val logic_mem : io -> name -> Names.mem_names -> Signal.t -> register -> memory -> unit
  val logic_mem2 : io -> name -> Names.mem_names -> Signal.t -> unit
  val logic_inst : io -> name -> string -> Signal.t -> instantiation -> unit
  val assign : io -> string -> string -> unit
  val end_logic : io -> unit
  val check_signal : Signal.t -> unit
end

module VerilogCore : Rtl_internal = struct
  let raise_unexpected = raise_unexpected ~generator:"verilog"
  let raise_expected = raise_expected ~generator:"verilog"

  module Names = SignalNameManager (VerilogNames) ()

  let string_of_type = function
    | Input -> "input"
    | Output -> "output"
    | Wire -> "wire"
    | Reg -> "reg"
    | Constant _ -> "wire"
  ;;

  let comment s = "/* " ^ s ^ " */"

  let decl t n b =
    let decl s n b =
      if b = 1 then s ^ " " ^ n else s ^ " [" ^ Int.to_string (b - 1) ^ ":0] " ^ n
    in
    match t with
    | Constant v -> decl (string_of_type t) n b ^ " = " ^ Int.to_string b ^ "'b" ^ v
    | _ -> decl (string_of_type t) n b
  ;;

  let print_attribute' attrs =
    match attrs with
    | [] -> ""
    | attrs ->
      let attribute_value_to_string (v : Rtl_attribute.Value.t) =
        match v with
        | String s -> "\"" ^ s ^ "\""
        | Int i -> Int.to_string i
        | Bool b -> if b then "true" else "false"
      in
      let attribute_to_string attr =
        match Rtl_attribute.value attr with
        | None -> Rtl_attribute.name attr
        | Some value ->
          sprintf "%s=%s" (Rtl_attribute.name attr) (attribute_value_to_string value)
      in
      let tag = List.map attrs ~f:attribute_to_string |> String.concat ~sep:"," in
      t4 ^ sprintf "(* %s *)\n" tag
  ;;

  let print_attribute s = print_attribute' (attributes s)

  let header_and_ports ~io ~name ~i ~o =
    let module_port _ last (s, _, _tag) =
      if last then t4 ^ s ^ "\n" else t4 ^ s ^ "," ^ "\n"
    in
    let decl_port t _ _ (s, b, tag) = print_attribute' tag ^ t4 ^ decl t s b ^ ";\n" in
    io ("module " ^ name ^ " (\n");
    write_strings io module_port (i @ o);
    io ");\n\n";
    write_strings io (decl_port Input) i;
    write_strings io (decl_port Output) o;
    io "\n"
  ;;

  let signal_decl io name s =
    let tag = print_attribute s in
    match s with
    | Empty -> raise_unexpected ~while_:"declaring signals" ~got_signal:s
    | Mux { cases; _ } ->
      (match cases with
       | [ _; _ ] -> io (tag ^ t4 ^ decl Wire name (width s) ^ ";\n")
       | _ -> io (tag ^ t4 ^ decl Reg name (width s) ^ ";\n"))
    | Op2 _ | Not _ | Cat _ | Wire _ | Select _ | Inst _ ->
      io (tag ^ t4 ^ decl Wire name (width s) ^ ";\n")
    | Reg _ -> io (tag ^ t4 ^ decl Reg name (width s) ^ ";\n")
    | Const { constant; _ } ->
      io (tag ^ t4 ^ decl (Constant (Bits.to_bstr constant)) name (width s) ^ ";\n")
    | Mem _ -> io (tag ^ t4 ^ decl Wire name (width s) ^ ";\n")
    | Mem_read_port _ -> io (tag ^ t4 ^ decl Wire name (width s) ^ ";\n")
    | Multiport_mem _ -> ()
  ;;

  let alias_decl io name s =
    let tag = print_attribute s in
    io (tag ^ t4 ^ decl Wire name (width s) ^ ";\n")
  ;;

  let mem_decl io name mem s =
    let tag = print_attribute s in
    let open Names in
    match s with
    | Mem { memory = sp; _ } ->
      let b = Int.to_string (width s - 1) in
      let s = Int.to_string (sp.mem_size - 1) in
      io (tag ^ t4 ^ "reg [" ^ b ^ ":0] " ^ mem.arr ^ "[0:" ^ s ^ "];\n")
    | Multiport_mem { size; _ } ->
      let b = Int.to_string (width s - 1) in
      let size = Int.to_string (size - 1) in
      io (tag ^ t4 ^ "reg [" ^ b ^ ":0] " ^ name s ^ "[0:" ^ size ^ "];\n")
    | _ -> raise_expected ~while_:"declaring memories" ~expected:"memory" ~got_signal:s
  ;;

  let start_logic _ = ()

  let clocked ~io ~signal ~register ~data_in ~name ~assign =
    let open Process in
    let level n = function
      | Level High | Edge Rising -> "(" ^ n ^ ")"
      | Level Low | Edge Falling -> "(" ^ n ^ " == 0" ^ ")"
    in
    let rec write_reg tab r =
      match r with
      | Empty -> ()
      | If (c, v, t, f) ->
        io (tab ^ "if " ^ level (name c) v ^ "\n");
        write_reg (tab ^ t4) t;
        if not (stat_is_empty f)
        then (
          io (tab ^ "else\n");
          write_reg (tab ^ t4) f)
      | Assign (q, d) -> assign tab q d
    in
    let edge s (l : Edge.t) =
      (match l with
       | Rising -> "posedge "
       | Falling -> "negedge ")
      ^ name s
    in
    let edges =
      if is_empty register.reg_reset
      then [ edge register.reg_clock register.reg_clock_edge ]
      else
        [ edge register.reg_clock register.reg_clock_edge
        ; edge register.reg_reset register.reg_reset_edge
        ]
    in
    let edges = sep " or " edges in
    io (t4 ^ "always @(" ^ edges ^ ") begin\n");
    write_reg t8 (make_reg ~clock:false () ~register ~signal ~data_in);
    io (t4 ^ "end\n")
  ;;

  let logic io name s =
    let dep n = List.nth_exn (deps s) n in
    let sn = name s in
    let binop op arg_a arg_b =
      let a = name arg_a in
      let b = name arg_b in
      io (t4 ^ "assign " ^ sn ^ " = " ^ a ^ " " ^ op ^ " " ^ b ^ ";\n")
    in
    let sbinop op arg_a arg_b =
      let a = name arg_a in
      let b = name arg_b in
      let a, b = "$signed(" ^ a ^ ")", "$signed(" ^ b ^ ")" in
      io (t4 ^ "assign " ^ sn ^ " = " ^ a ^ " " ^ op ^ " " ^ b ^ ";\n")
    in
    match s with
    | Mem _ | Multiport_mem _ | Inst _ | Empty ->
      raise_unexpected ~while_:"writing logic assignments" ~got_signal:s
    | Not { arg; _ } -> io (t4 ^ "assign " ^ sn ^ " = ~ " ^ name arg ^ ";\n")
    | Cat { args; _ } ->
      let cat = sep ", " (List.map args ~f:(fun s -> name s)) in
      io (t4 ^ "assign " ^ sn ^ " = { " ^ cat ^ " };\n")
    | Mux { select; cases; _ } ->
      (match cases with
       | [ false_; true_ ] ->
         io
           (t4
            ^ "assign "
            ^ sn
            ^ " = "
            ^ name select
            ^ " ? "
            ^ name true_
            ^ " : "
            ^ name false_
            ^ ";\n")
       | _ ->
         let n = List.length cases in
         io (t4 ^ "always @* begin\n");
         io (t8 ^ "case (" ^ name select ^ ")\n");
         List.iteri cases ~f:(fun i s ->
           if i <> n - 1
           then io (t8 ^ Int.to_string i ^ ": ")
           else io (t8 ^ "default" ^ ": ");
           io (sn ^ " <= " ^ name s ^ ";\n"));
         io (t8 ^ "endcase\n");
         io (t4 ^ "end\n"))
    | Op2 { op; arg_a; arg_b; _ } ->
      (match op with
       | Signal_add -> binop "+"
       | Signal_sub -> binop "-"
       | Signal_mulu -> binop "*"
       | Signal_muls -> sbinop "*"
       | Signal_and -> binop "&"
       | Signal_or -> binop "|"
       | Signal_xor -> binop "^"
       | Signal_eq -> binop "=="
       | Signal_lt -> binop "<")
        arg_a
        arg_b
    | Wire { driver; _ } -> io (t4 ^ "assign " ^ sn ^ " = " ^ name !driver ^ ";\n")
    | Reg { register; d = data_in; _ } ->
      clocked ~io ~signal:s ~register ~data_in ~name ~assign:(fun tab q d ->
        io (tab ^ name q ^ " <= " ^ name d ^ ";\n"))
    | Select { high; low; _ } ->
      io
        (t4
         ^ "assign "
         ^ sn
         ^ " = "
         ^ name (dep 0)
         ^ "["
         ^ Int.to_string high
         ^ ":"
         ^ Int.to_string low
         ^ "];\n")
    | Mem_read_port { memory; read_address; _ } ->
      io (t4 ^ "assign " ^ sn ^ " = " ^ name memory ^ "[" ^ name read_address ^ "];\n")
    | Const _ -> ()
  ;;

  (* already done *)

  let logic_mem io name mem signal register sp =
    let open Names in
    let sn = name signal in
    let data_in = List.hd_exn (deps signal) in
    clocked ~io ~signal ~register ~data_in ~name ~assign:(fun tab _ d ->
      let d' = uid (List.hd_exn (deps signal)) in
      if Uid.equal d' (uid d)
      then (
        let wa = name sp.mem_write_address in
        io (tab ^ mem.arr ^ "[" ^ wa ^ "] <= " ^ name d ^ ";\n"))
      else (
        (* some reset/clear clause *)
        let i = mem.t2 in
        io (tab ^ "begin: " ^ mem.t1 ^ "\n");
        io (tab ^ t4 ^ "integer " ^ i ^ ";\n");
        io
          (tab
           ^ t4
           ^ "for ("
           ^ i
           ^ "=0; "
           ^ i
           ^ "<"
           ^ Int.to_string sp.mem_size
           ^ "; "
           ^ i
           ^ "="
           ^ i
           ^ "+1)\n");
        io (tab ^ t8 ^ mem.arr ^ "[" ^ i ^ "] <= " ^ name d ^ ";\n");
        io (tab ^ "end\n")));
    (* read *)
    let a = name sp.mem_read_address in
    io (t4 ^ "assign " ^ sn ^ " = " ^ mem.arr ^ "[" ^ a ^ "];\n")
  ;;

  let logic_mem2 io name _mem signal =
    let write_ports =
      match signal with
      | Multiport_mem { write_ports; _ } -> write_ports
      | _ -> raise_s [%message "Internal error - expecting Multiport_mem signal"]
    in
    Array.iter write_ports ~f:(fun write_port ->
      clocked
        ~io
        ~signal
        ~register:
          Reg_spec.(
            create () ~clock:write_port.write_clock
            |> override ~global_enable:write_port.write_enable)
        ~data_in:write_port.write_data
        ~name
        ~assign:(fun tab _ d ->
          let wa = name write_port.write_address in
          io (tab ^ name signal ^ "[" ^ wa ^ "] <= " ^ name d ^ ";\n")))
  ;;

  let logic_inst io name inst_name s i =
    io (t4 ^ i.inst_name ^ "\n");
    let assoc n v = "." ^ n ^ "(" ^ v ^ ")" in
    (* parameters *)
    let param_string (p : Parameter.t) =
      match p.value with
      | String v -> "\"" ^ v ^ "\""
      | Int v -> Int.to_string v
      (* For consistency with VHDL, but not proved necessary for verilog. *)
      | Real v -> sprintf "%f" v
      | Bool b | Bit b -> if b then "1'b1" else "1'b0"
      | Std_logic_vector v | Std_ulogic_vector v ->
        Printf.sprintf
          "%i'b%s"
          (Parameter.Std_logic_vector.width v)
          (Parameter.Std_logic_vector.to_string v)
      | Bit_vector v ->
        Printf.sprintf
          "%i'b%s"
          (Parameter.Bit_vector.width v)
          (Parameter.Bit_vector.to_string v)
      | Std_logic b | Std_ulogic b ->
        Printf.sprintf "4'd%i" (Parameter.Std_logic.to_int b)
    in
    if not (List.is_empty i.inst_generics)
    then (
      let generics =
        let generic (p : Parameter.t) =
          assoc (Parameter_name.to_string p.name) (param_string p)
        in
        sep ", " (List.map i.inst_generics ~f:generic)
      in
      io (t8 ^ "#( " ^ generics ^ " )\n"));
    io (t8 ^ inst_name ^ "\n");
    (* ports *)
    let write_port_reference ?indexes port_name signal =
      try
        match indexes with
        | None -> assoc port_name (name signal)
        | Some (hi, lo) ->
          assoc
            port_name
            (name signal ^ "[" ^ Int.to_string hi ^ ":" ^ Int.to_string lo ^ "]")
      with
      | (exn : exn) ->
        raise_s
          [%message
            "failed to connect instantiation port"
              (inst_name : string)
              (port_name : string)
              (signal : Signal.t)
              (indexes : (int * int) option)
              (exn : exn)]
    in
    let in_ports = List.map i.inst_inputs ~f:(fun (n, s) -> write_port_reference n s) in
    let out_ports =
      if width s = 1
      then
        (* special case - 1 output of 1 bit *)
        List.map i.inst_outputs ~f:(fun (n, _) -> write_port_reference n s)
      else
        List.map i.inst_outputs ~f:(fun (n, (w, l)) ->
          write_port_reference n s ~indexes:(w + l - 1, l))
    in
    io (t8 ^ "( " ^ sep ", " (in_ports @ out_ports) ^ " );\n")
  ;;

  let assign io t f = io (t4 ^ "assign " ^ t ^ " = " ^ f ^ ";\n")
  let end_logic io = io "endmodule\n"
  let check_signal (_ : Signal.t) = ()
end

module VhdlCore : Rtl_internal = struct
  let raise_unexpected = raise_unexpected ~generator:"vhdl"
  let raise_expected = raise_expected ~generator:"vhdl"

  module Names = SignalNameManager (VhdlNames) ()

  let conversions =
    let p = Names.prefix in
    [ "function "
      ^ p
      ^ "uns(a : std_logic)        return unsigned         is variable b : unsigned(0 \
         downto 0); begin b(0) := a; return b; end;"
    ; "function "
      ^ p
      ^ "uns(a : std_logic_vector) return unsigned         is begin return unsigned(a); \
         end;"
    ; "function "
      ^ p
      ^ "sgn(a : std_logic)        return signed           is variable b : signed(0 \
         downto 0); begin b(0) := a; return b; end;"
    ; "function "
      ^ p
      ^ "sgn(a : std_logic_vector) return signed           is begin return signed(a); \
         end;"
    ; "function "
      ^ p
      ^ "sl (a : std_logic_vector) return std_logic        is begin return a(a'right); \
         end;"
    ; "function "
      ^ p
      ^ "sl (a : unsigned)         return std_logic        is begin return a(a'right); \
         end;"
    ; "function "
      ^ p
      ^ "sl (a : signed)           return std_logic        is begin return a(a'right); \
         end;"
    ; "function "
      ^ p
      ^ "sl (a : boolean)          return std_logic        is begin if a then return \
         '1'; else return '0'; end if; end;"
    ; "function "
      ^ p
      ^ "slv(a : std_logic_vector) return std_logic_vector is begin return a; end;"
    ; "function "
      ^ p
      ^ "slv(a : unsigned)         return std_logic_vector is begin return \
         std_logic_vector(a); end;"
    ; "function "
      ^ p
      ^ "slv(a : signed)           return std_logic_vector is begin return \
         std_logic_vector(a); end;"
    ]
  ;;

  let comment s = "-- " ^ s

  let const_str b v =
    let q = if b = 1 then "'" else "\"" in
    q ^ v ^ q
  ;;

  let decl t n b =
    let decl_slv b =
      if b = 1
      then "std_logic"
      else "std_logic_vector (" ^ Int.to_string (b - 1) ^ " downto 0)"
    in
    match t with
    | Constant v -> "constant " ^ n ^ " : " ^ decl_slv b ^ " := " ^ const_str b v
    | Input -> n ^ " : in " ^ decl_slv b
    | Output -> n ^ " : out " ^ decl_slv b
    | Wire -> "signal " ^ n ^ " : " ^ decl_slv b
    | Reg -> "signal " ^ n ^ " : " ^ decl_slv b
  ;;

  let header_and_ports ~io ~name ~i ~o =
    let entity_in_port _ _ (s, n, _attrs) = t8 ^ decl Input s n ^ ";\n" in
    let entity_out_port _ last (s, n, _attrs) =
      t8 ^ decl Output s n ^ if last then "\n" else ";\n"
    in
    io "library ieee;\n";
    io "use ieee.std_logic_1164.all;\n";
    io "use ieee.numeric_std.all;\n\n";
    io ("entity " ^ name ^ " is\n");
    io "    port (\n";
    write_strings io entity_in_port i;
    write_strings io entity_out_port o;
    io "    );\n";
    io "end entity;\n\n";
    io ("architecture rtl of " ^ name ^ " is\n\n");
    io (t4 ^ comment "conversion functions" ^ "\n");
    List.iter conversions ~f:(fun s -> io (t4 ^ s ^ "\n"));
    io "\n"
  ;;

  let signal_decl io name s =
    match s with
    | Empty -> raise_unexpected ~while_:"declaring signals" ~got_signal:s
    | Op2 _ | Mux _ | Cat _ | Not _ | Wire _ | Select _ | Inst _ ->
      io (t4 ^ decl Wire name (width s) ^ ";\n")
    | Reg _ -> io (t4 ^ decl Reg name (width s) ^ ";\n")
    | Const { constant; _ } ->
      io (t4 ^ decl (Constant (Bits.to_bstr constant)) name (width s) ^ ";\n")
    | Mem _ | Mem_read_port _ -> io (t4 ^ decl Reg name (width s) ^ ";\n")
    | Multiport_mem _ -> ()
  ;;

  let alias_decl io name s = io (t4 ^ decl Wire name (width s) ^ ";\n")

  let mem_decl io name mem s =
    let open Names in
    match s with
    | Mem { memory = sp; _ } ->
      let b = Int.to_string (width s - 1) in
      let sz = Int.to_string (sp.mem_size - 1) in
      (* need a sepatate ID for the array type *)
      io (t4 ^ "type " ^ mem.typ ^ " is array (0 to " ^ sz ^ ") of ");
      if width s = 1
      then io "std_logic;\n"
      else io ("std_logic_vector(" ^ b ^ " downto 0);\n");
      io (t4 ^ "signal " ^ mem.arr ^ " : " ^ mem.typ ^ ";\n")
    | Multiport_mem { size; _ } ->
      let b = Int.to_string (width s - 1) in
      let sz = Int.to_string (size - 1) in
      io (t4 ^ "type " ^ mem.typ ^ " is array (0 to " ^ sz ^ ") of ");
      if width s = 1
      then io "std_logic;\n"
      else io ("std_logic_vector(" ^ b ^ " downto 0);\n");
      io (t4 ^ "signal " ^ name s ^ " : " ^ mem.typ ^ ";\n")
    | _ -> raise_expected ~while_:"declaring memories" ~expected:"memory" ~got_signal:s
  ;;

  let start_logic io = io "begin\n\n"

  let clocked ~io ~signal ~register ~data_in ~name ~assign =
    let open Process in
    let level n = function
      | Level High -> n ^ " = '1'"
      | Level Low -> n ^ " = '0'"
      | Edge Rising -> "rising_edge(" ^ n ^ ")"
      | Edge Falling -> "falling_edge(" ^ n ^ ")"
    in
    let rec write_reg tab r =
      match r with
      | Empty -> ()
      | If (c, v, t, f) ->
        io (tab ^ "if " ^ level (name c) v ^ " then\n");
        write_reg (tab ^ t4) t;
        if not (stat_is_empty f)
        then (
          io (tab ^ "else\n");
          write_reg (tab ^ t4) f);
        io (tab ^ "end if;\n")
      | Assign (q, d) -> assign tab q d
    in
    let edges =
      if is_empty register.reg_reset
      then [ register.reg_clock ]
      else [ register.reg_clock; register.reg_reset ]
    in
    let edges = sep ", " (List.map edges ~f:(fun s -> name s)) in
    io (t4 ^ "process (" ^ edges ^ ") begin\n");
    write_reg t8 (make_reg () ~register ~signal ~data_in);
    io (t4 ^ "end process;\n")
  ;;

  let logic io name s =
    let dep n = List.nth_exn (deps s) n in
    let sn = name s in
    let dname n = name (dep n) in
    let unsigned_name s = Names.prefix ^ "uns(" ^ name s ^ ")" in
    let signed_name s = Names.prefix ^ "sgn(" ^ name s ^ ")" in
    let slv str =
      Names.prefix ^ (if width s = 1 then "sl" else "slv") ^ "(" ^ str ^ ")"
    in
    let binop name op a b =
      io (t4 ^ sn ^ " <= " ^ slv (name a ^ " " ^ op ^ " " ^ name b) ^ ";\n")
    in
    let sbinop a b = binop signed_name a b in
    let binop a b = binop unsigned_name a b in
    match s with
    | Mem _ | Multiport_mem _ | Inst _ | Empty ->
      raise_unexpected ~while_:"writing logic assignments" ~got_signal:s
    | Not { arg; _ } -> io (t4 ^ sn ^ " <= " ^ slv ("not " ^ unsigned_name arg) ^ ";\n")
    | Cat { args; _ } ->
      let cat = sep " & " (List.map args ~f:(fun s -> name s)) in
      io (t4 ^ sn ^ " <= " ^ cat ^ ";\n")
    | Mux { select; cases; _ } ->
      let n = List.length cases in
      io (t4 ^ "with to_integer(" ^ unsigned_name select ^ ") select ");
      io (sn ^ " <= \n");
      List.iteri cases ~f:(fun i s ->
        io (t8 ^ name s ^ " when ");
        if i <> n - 1 then io (Int.to_string i ^ ",\n") else io "others;\n")
    | Op2 { op; arg_a; arg_b; _ } ->
      (match op with
       | Signal_add -> binop "+"
       | Signal_sub -> binop "-"
       | Signal_mulu -> binop "*"
       | Signal_muls -> sbinop "*"
       | Signal_and -> binop "and"
       | Signal_or -> binop "or"
       | Signal_xor -> binop "xor"
       | Signal_eq -> binop "="
       | Signal_lt -> binop "<")
        arg_a
        arg_b
    | Wire { driver; _ } -> io (t4 ^ sn ^ " <= " ^ name !driver ^ ";\n")
    | Reg { register; d; _ } ->
      clocked ~io ~signal:s ~register ~data_in:d ~name ~assign:(fun tab q d ->
        io (tab ^ name q ^ " <= " ^ name d ^ ";\n"))
    | Select { high; low; _ } ->
      let sel =
        dname 0 ^ "(" ^ Int.to_string high ^ " downto " ^ Int.to_string low ^ ")"
      in
      let sel = if width s = 1 then slv sel else sel in
      io (t4 ^ sn ^ " <= " ^ sel ^ ";\n")
    | Mem_read_port { memory; read_address; _ } ->
      io
        (t4
         ^ sn
         ^ " <= "
         ^ name memory
         ^ "(to_integer(hc_uns("
         ^ name read_address
         ^ ")));\n")
    | Const _ -> ()
  ;;

  (* already done *)

  let logic_mem io name mem signal register sp =
    let open Names in
    let sn = name signal in
    let data_in = List.hd_exn (deps signal) in
    let to_integer s = "to_integer(" ^ Names.prefix ^ "uns(" ^ s ^ "))" in
    clocked ~io ~signal ~register ~data_in ~name ~assign:(fun tab _ d ->
      let d' = uid (List.hd_exn (deps signal)) in
      if Uid.equal d' (uid d)
      then (
        let wa = name sp.mem_write_address in
        io (tab ^ mem.arr ^ "(" ^ to_integer wa ^ ") <= " ^ name d ^ ";\n"))
      else (
        (* some reset/clear clause *)
        let i = mem.t2 in
        io
          (tab ^ "for " ^ i ^ " in 0 to " ^ Int.to_string (sp.mem_size - 1) ^ " loop\n");
        io (tab ^ t4 ^ mem.arr ^ "(" ^ i ^ ") <= " ^ name d ^ ";\n");
        io (tab ^ "end loop;\n")));
    (* read *)
    let a = name sp.mem_read_address in
    io (t4 ^ sn ^ " <= " ^ mem.arr ^ "(" ^ to_integer a ^ ");\n")
  ;;

  let logic_mem2 io name _mem signal =
    let to_integer s = "to_integer(" ^ Names.prefix ^ "uns(" ^ s ^ "))" in
    let write_ports =
      match signal with
      | Multiport_mem { write_ports; _ } -> write_ports
      | _ -> raise_s [%message "Internal error - expecting Multiport_mem signal"]
    in
    Array.iter write_ports ~f:(fun write_port ->
      clocked
        ~io
        ~signal
        ~register:
          Reg_spec.(
            create () ~clock:write_port.write_clock
            |> override ~global_enable:write_port.write_enable)
        ~data_in:write_port.write_data
        ~name
        ~assign:(fun tab _ d ->
          let wa = name write_port.write_address in
          io (tab ^ name signal ^ "(" ^ to_integer wa ^ ") <= " ^ name d ^ ";\n")))
  ;;

  let logic_inst io name inst_name s i =
    io
      (t4
       ^ inst_name
       ^ ": entity "
       ^ i.inst_lib
       ^ "."
       ^ i.inst_name
       ^ " ("
       ^ i.inst_arch
       ^ ")"
       ^ "\n");
    let assoc n v = n ^ " => " ^ v in
    (* parameters *)
    let param_string (p : Parameter.t) =
      match p.value with
      | String v -> "\"" ^ v ^ "\""
      | Bit_vector v -> "\"" ^ Parameter.Bit_vector.to_string v ^ "\""
      | Std_logic_vector v ->
        String.concat
          [ "std_logic_vector'(\""; Parameter.Std_logic_vector.to_string v; "\")" ]
      | Std_ulogic_vector v ->
        String.concat
          [ "std_ulogic_vector'(\""; Parameter.Std_logic_vector.to_string v; "\")" ]
      | Int v -> Int.to_string v
      (* floats must be printed with a trailing number in vhdl (ie [11.0] not [11.]) and
         [%f] seems to do that. *)
      | Real v -> sprintf "%f" v
      | Bool v -> if v then "true" else "false"
      | Bit b -> if b then "'1'" else "'0'"
      | Std_ulogic b | Std_logic b -> sprintf "'%c'" (Parameter.Std_logic.to_char b)
    in
    if not (List.is_empty i.inst_generics)
    then (
      let generics =
        let generic (p : Parameter.t) =
          assoc (Parameter_name.to_string p.name) (param_string p)
        in
        sep ", " (List.map i.inst_generics ~f:generic)
      in
      io (t8 ^ "generic map ( " ^ generics ^ ")\n"));
    let in_ports = List.map i.inst_inputs ~f:(fun (n, s) -> assoc n (name s)) in
    let out_ports =
      if width s = 1
      then
        (* special case - 1 output of 1 bit *)
        List.map i.inst_outputs ~f:(fun (n, _) -> assoc n (name s))
      else
        List.map i.inst_outputs ~f:(fun (n, (w, l)) ->
          if w = 1
          then assoc n (name s ^ "(" ^ Int.to_string l ^ ")")
          else
            assoc
              n
              (name s
               ^ "("
               ^ Int.to_string (w + l - 1)
               ^ " downto "
               ^ Int.to_string l
               ^ ")"))
    in
    io (t8 ^ "port map ( " ^ sep ", " (in_ports @ out_ports) ^ " );\n")
  ;;

  let assign io t f = io (t4 ^ t ^ " <= " ^ f ^ ";\n")
  let end_logic io = io "end architecture;\n"

  let check_signal t =
    match Signal.attributes t with
    | [] -> ()
    | _ -> raise_s [%message "Signal attributes are not supported in VHDL yet"]
  ;;
end

(* RTL writer *)

module Make (R : Rtl_internal) = struct
  let write blackbox io circ =
    let inputs, outputs = Circuit.inputs circ, Circuit.outputs circ in
    let phantom_inputs =
      List.map ~f:(fun (a, b) -> a, b, []) (Circuit.phantom_inputs circ)
    in
    let signal_graph = Circuit.signal_graph circ in
    (* write signal declarations *)
    let is_internal s =
      (not (Circuit.is_input circ s))
      && (not (Circuit.is_output circ s))
      && not (is_empty s)
    in
    let internal_signals = Signal_graph.filter signal_graph ~f:is_internal in
    (* initialize the mangler *)
    let nm =
      (* reserved words and the names of phantom inputs. *)
      R.Names.init (R.Names.reserved @ List.map phantom_inputs ~f:(fun (n, _, _) -> n))
    in
    let add f nm x = List.fold x ~init:nm ~f:(fun nm x -> f x nm) in
    let nm = add R.Names.add_port nm inputs in
    let nm = add R.Names.add_port nm outputs in
    let nm = add R.Names.add_signal nm internal_signals in
    let primary_name s = R.Names.signal_name nm s 0 in
    let secondary_names s =
      let l = List.length (names s) in
      if l < 2
      then []
      else (
        let n = Array.init (l - 1) ~f:(fun i -> R.Names.signal_name nm s (i + 1)) in
        Array.to_list n)
    in
    let primary s = primary_name s, width s, attributes s in
    List.iter internal_signals ~f:(fun s -> R.check_signal s);
    R.header_and_ports
      ~io
      ~name:(Circuit.name circ)
      ~i:(List.map inputs ~f:primary @ phantom_inputs)
      ~o:(List.map outputs ~f:primary);
    (* write internal declarations *)
    if not blackbox
    then (
      io (t4 ^ R.comment "signal declarations" ^ "\n");
      List.iter internal_signals ~f:(fun s ->
        (* primary signals *)
        R.signal_decl io (primary_name s) s;
        (* aliases *)
        List.iter (secondary_names s) ~f:(fun name -> R.alias_decl io name s);
        (* special declarations *)
        match s with
        | Mem _ | Multiport_mem _ ->
          let mem_names = R.Names.mem_names nm s in
          R.mem_decl io primary_name mem_names s
        | _ -> ());
      io "\n";
      R.start_logic io;
      (* logic *)
      io (t4 ^ R.comment "logic" ^ "\n");
      List.iter internal_signals ~f:(fun signal ->
        match signal with
        | Mem { register = r; memory = m; _ } ->
          let mem = R.Names.mem_names nm signal in
          R.logic_mem io primary_name mem signal r m
        | Multiport_mem _ ->
          let mem = R.Names.mem_names nm signal in
          R.logic_mem2 io primary_name mem signal
        | Inst { instantiation; _ } ->
          let inst_name = R.Names.inst_label nm signal in
          R.logic_inst io primary_name inst_name signal instantiation
        | _ -> R.logic io primary_name signal);
      io "\n";
      (* connect aliases *)
      io (t4 ^ R.comment "aliases" ^ "\n");
      List.iter internal_signals ~f:(fun s ->
        List.iter (secondary_names s) ~f:(fun t -> R.assign io t (primary_name s)));
      io "\n";
      (* connect outputs *)
      io (t4 ^ R.comment "output assignments" ^ "\n");
      List.iter outputs ~f:(R.logic io primary_name);
      io "\n");
    R.end_logic io
  ;;
end

module Vhdl = Make (VhdlCore)
module Verilog = Make (VerilogCore)

module Language = struct
  type t =
    | Verilog
    | Vhdl
  [@@deriving sexp_of]

  let file_extension = function
    | Verilog -> ".v"
    | Vhdl -> ".vhd"
  ;;

  let output blackbox = function
    | Vhdl -> Vhdl.write blackbox
    | Verilog -> Verilog.write blackbox
  ;;

  let legalize_identifier = function
    | Verilog -> VerilogNames.legalize
    | Vhdl -> VhdlNames.legalize
  ;;
end

module Hierarchy_path : sig
  type t [@@deriving sexp_of]

  val empty : t
  val push : t -> string -> t
  val is_top_circuit : t -> Circuit.t -> bool
end = struct
  type t = string list

  let empty = []
  let push t s = s :: t
  let to_string_list t = List.rev t
  let sexp_of_t t = [%sexp (to_string_list t : string list)]

  let is_top_circuit t circuit =
    match t with
    | [ name ] -> String.equal (Circuit.name circuit) name
    | _ -> false
  ;;
end

module Output_mode = struct
  type t =
    | In_directory of string
    | To_buffer of Buffer.t
    | To_channel of Out_channel.t
    | To_file of string
  [@@deriving sexp_of]
end

module Output = struct
  module Mode = struct
    type t =
      | In_directory of string
      | To_buffer of Buffer.t
      | To_channel of Out_channel.t
      | To_file of
          { file : string
          ; out_channel : Out_channel.t
          }
    [@@deriving sexp_of]
  end

  type t =
    { language : Language.t
    ; mode : Mode.t
    }
  [@@deriving sexp_of]

  let create ~(output_mode : Output_mode.t) ~language =
    let mode : Mode.t =
      match output_mode with
      | In_directory d -> In_directory d
      | To_buffer b -> To_buffer b
      | To_channel c -> To_channel c
      | To_file file -> To_file { file; out_channel = Out_channel.create file }
    in
    { language; mode }
  ;;

  let output_circuit (blackbox : bool) t circuit hierarchy_path =
    try
      let output, close =
        match t.mode with
        | In_directory directory ->
          (* New file created for each circuit. *)
          let name = Circuit.name circuit in
          let file_name =
            Filename.concat directory (name ^ Language.file_extension t.language)
          in
          let chan = Out_channel.create file_name in
          Out_channel.output_string chan, fun () -> Out_channel.close chan
        | To_buffer buffer -> Buffer.add_string buffer, Fn.id
        | To_channel out_channel ->
          (* Note; up to caller to flush. *)
          Out_channel.output_string out_channel, Fn.id
        | To_file { file = _; out_channel } ->
          (* File is created before any processing occurs, then closed with the top
             level module. *)
          ( Out_channel.output_string out_channel
          , fun () ->
            if Hierarchy_path.is_top_circuit hierarchy_path circuit
            then Out_channel.close out_channel )
      in
      Language.output blackbox t.language output circuit;
      close ()
    with
    | exn ->
      raise_s
        [%message
          "Error while writing circuit"
            ~circuit_name:(Circuit.name circuit : string)
            (hierarchy_path : Hierarchy_path.t)
            ~output:(t : t)
            (exn : exn)]
  ;;
end

module Blackbox = struct
  type t =
    | None
    | Top
    | Instantiations
  [@@deriving sexp_of]
end

let output ?output_mode ?database ?(blackbox = Blackbox.None) language circuit =
  let output_mode =
    Option.value
      output_mode
      ~default:
        (Output_mode.To_file (Circuit.name circuit ^ Language.file_extension language))
  in
  let output =
    try Output.create ~language ~output_mode with
    | exn ->
      raise_s
        [%message
          "Error while initializing output mode."
            ~circuit_name:(Circuit.name circuit)
            (language : Language.t)
            (output_mode : Output_mode.t)
            (exn : exn)]
  in
  let database = Option.value database ~default:(Circuit_database.create ()) in
  let circuits_already_output = Hash_set.create (module String) in
  let rec output_circuit blackbox circuit hierarchy_path =
    let circuit_name = Circuit.name circuit in
    if not (Hash_set.mem circuits_already_output circuit_name)
    then (
      Hash_set.add circuits_already_output circuit_name;
      let hierarchy_path = Hierarchy_path.push hierarchy_path circuit_name in
      match (blackbox : Blackbox.t) with
      | None ->
        output_instantitions (None : Blackbox.t) circuit hierarchy_path;
        Output.output_circuit false output circuit hierarchy_path
      | Top -> Output.output_circuit true output circuit hierarchy_path
      | Instantiations ->
        output_instantitions Top circuit hierarchy_path;
        Output.output_circuit false output circuit hierarchy_path)
  and output_instantitions (blackbox : Blackbox.t) circuit hierarchy_path =
    Signal_graph.iter (Circuit.signal_graph circuit) ~f:(fun signal ->
      match signal with
      | Inst { instantiation; _ } ->
        (match
           Circuit_database.find database ~mangled_name:instantiation.inst_name
         with
         | None ->
           (* No hardcaml implementation available.  Downstream tooling will provide the
              implmentation. *)
           ()
         | Some circuit -> output_circuit blackbox circuit hierarchy_path)
      | _ -> ())
  in
  output_circuit blackbox circuit Hierarchy_path.empty
;;

let print ?database ?blackbox language circuit =
  output
    ~output_mode:(To_channel Out_channel.stdout)
    ?database
    ?blackbox
    language
    circuit
;;
