open! Import
open! Signal_intf

module Signal_op = struct
  type t =
    | Signal_add
    | Signal_sub
    | Signal_mulu
    | Signal_muls
    | Signal_and
    | Signal_or
    | Signal_xor
    | Signal_eq
    | Signal_not
    | Signal_lt
    | Signal_cat
    | Signal_mux
  [@@deriving compare, sexp_of]

  let equal = [%compare.equal: t]
end

type signal_op = Signal_op.t =
  | Signal_add
  | Signal_sub
  | Signal_mulu
  | Signal_muls
  | Signal_and
  | Signal_or
  | Signal_xor
  | Signal_eq
  | Signal_not
  | Signal_lt
  | Signal_cat
  | Signal_mux

module Uid = struct
  module T = struct
    type t = int64
    [@@deriving compare, hash, sexp_of]
  end

  include T
  include Comparator.Make (T)

  let equal = [%compare.equal: t]
end

module Uid_map = Map.Make (Uid)

module Uid_set = struct
  type t = Set.M(Uid).t [@@deriving sexp_of]

  let empty = Set.empty (module Uid)
end

type signal_id =
  { s_id            : Uid.t
  ; mutable s_names : string list
  ; s_width         : int
  ; mutable s_deps  : t list }

and t =
  | Empty
  | Const of signal_id * string
  | Op of signal_id * signal_op
  | Wire of signal_id * t ref
  | Select of signal_id * int * int
  | Reg of signal_id * register
  | Mem of signal_id * Uid.t * register * memory
  | Inst of signal_id * Uid.t * instantiation

(* These types are used to define a particular type of register as per the following
   template, where each part is optional:

   {v
       always @(?edge clock, ?edge reset)
         if (reset == reset_level) d <= reset_value;
         else if (clear == clear_level) d <= clear_value;
         else if (enable) d <= ...;
     v} *)
and register =
  { reg_clock       : t (* clock                    *)
  ; reg_clock_level : t (* active clock edge        *)
  ; reg_reset       : t (* asynchronous reset       *)
  ; reg_reset_level : t (* asynchronous reset level *)
  ; reg_reset_value : t (* asychhronous reset value *)
  ; reg_clear       : t (* synchronous reset        *)
  ; reg_clear_level : t (* synchronous reset level  *)
  ; reg_clear_value : t (* sychhronous reset value  *)
  ; reg_enable      : t (* global system enable     *) }

and memory =
  { mem_size          : int
  ; mem_read_address  : t
  ; mem_write_address : t }

and instantiation =
  { inst_name     : string                      (* name of circuit *)
  ; inst_instance : string                      (* instantiation label *)
  ; inst_generics : Parameter.t list            (* [ Parameter.create ~name:"ram_type" ~value:(String "auto") ] *)
  ; inst_inputs   : (string * t) list           (* name and input signal *)
  ; inst_outputs  : (string * (int * int)) list (* name, width and low index of output *)
  ; inst_lib      : string
  ; inst_arch     : string }

let is_empty = function Empty -> true | _ -> false

let uid s =
  match s with
  | Empty -> 0L
  | Const (s, _)
  | Select (s, _, _)
  | Reg (s, _)
  | Mem (s, _, _, _)
  | Wire (s, _)
  | Inst (s, _, _)
  | Op (s, _) -> s.s_id

let deps s =
  match s with
  | Empty | Const _ -> []
  | Select (s, _, _)
  | Reg (s, _)
  | Mem (s, _, _, _)
  | Inst (s, _, _)
  | Op (s, _) -> s.s_deps
  | Wire (_, s) -> [ !s ]

let names s =
  match s with
  | Empty -> raise_s [%message "cannot get [names] from the empty signal"]
  | Const (s, _)
  | Select (s, _, _)
  | Reg (s, _)
  | Mem (s, _, _, _)
  | Wire (s, _)
  | Inst (s, _, _)
  | Op (s, _) -> s.s_names

let has_name t = not (List.is_empty (names t))

let width s =
  match s with
  | Empty -> 0
  | Const (s, _)
  | Select (s, _, _)
  | Reg (s, _)
  | Mem (s, _, _, _)
  | Wire (s, _)
  | Inst (s, _, _)
  | Op (s, _) -> s.s_width

let is_reg = function Reg _ -> true | _ -> false
let is_const = function Const _ -> true | _ -> false
let is_select = function Select _ -> true | _ -> false
let is_wire = function Wire _ -> true | _ -> false
let is_op op = function Op (_, o) -> Signal_op.equal o op | _ -> false
let is_mem = function Mem _ -> true | _ -> false
let is_inst = function Inst _ -> true | _ -> false

let new_id, reset_id =
  let id = ref 1L in
  let new_id () =
    let x = !id in
    id := (Int64.add (!id) 1L);
    x
  in
  let reset_id () =
    id := 1L
  in
  new_id, reset_id

let make_id w deps =
  { s_id    = new_id ()
  ; s_names = []
  ; s_width = w
  ; s_deps  = deps }

let string_of_op = function
  | Signal_add -> "add"
  | Signal_sub -> "sub"
  | Signal_mulu -> "mulu"
  | Signal_muls -> "muls"
  | Signal_and -> "and"
  | Signal_or -> "or"
  | Signal_xor -> "xor"
  | Signal_eq -> "eq"
  | Signal_not -> "not"
  | Signal_lt -> "lt"
  | Signal_cat -> "cat"
  | Signal_mux -> "mux"

let to_string signal =
  let names s =
    List.fold (names s) ~init:"" ~f:(fun a s ->
      if String.is_empty a then s else a ^ "," ^ s) in
  let deps s =
    List.fold (deps s) ~init:"" ~f:(fun a s ->
      let s = Int64.to_string (uid s) in
      if String.is_empty a then s else a ^ "," ^ s)
  in
  let sid s =
    "id:" ^ Int64.to_string (uid s) ^ " bits:" ^ Int.to_string (width s) ^
    " names:" ^ names s ^ " deps:" ^ deps s ^ "" in
  match signal with
  | Empty -> "Empty"
  | Const (_, v) -> "Const[" ^ sid signal ^ "] = " ^ v
  | Op (_, o) -> "Op[" ^ sid signal ^ "] = " ^ string_of_op o
  | Wire (_, d) ->
    "Wire[" ^ sid signal ^ "] -> " ^ Int64.to_string (uid !d)
  | Select (_, h, l) ->
    "Select[" ^ sid signal ^ "] " ^ Int.to_string h ^ ".." ^ Int.to_string l
  | Reg _ -> "Reg[" ^ sid signal ^ "]"
  | Mem _ -> "Mem[" ^ sid signal ^ "]"
  | Inst _ -> "Inst" ^ sid signal ^ "]"

let structural_compare
      ?(check_names=true) ?(check_deps=true) a b =
  let rec structural_compare
            s a b =
    if Set.mem s (uid a)
    then true
    else
      let s = Set.add s (uid a) in
      (* check we have the same type of node *)
      let typ () =
        match a, b with
        | Empty, Empty ->
          true
        | Const (_, a), Const (_, b) ->
          String.equal a b
        | Select (_, h0, l0), Select (_, h1, l1) ->
          (h0=h1) && (l0=l1)
        | Reg (_, _), Reg (_, _) ->
          true
        | Mem (_, _, _, m0), Mem (_, _, _, m1) ->
          m0.mem_size=m1.mem_size
        (* XXX check if inputs have same names ? *)
        | Wire (_, _), Wire (_, _) ->
          true
        | Inst (_, _, i0), Inst (_, _, i1) ->
          (String.equal i0.inst_name i1.inst_name) &&
          (*i0.inst_instance=i1.inst_instance &&*)
          ([%compare.equal: Parameter.t list]
             i0.inst_generics i1.inst_generics)
          && ([%compare.equal: (string * (int * int)) list]
                i0.inst_outputs i1.inst_outputs)
        (* inst_inputs=??? *)
        | Op (_, o0), Op (_, o1) ->
          Signal_op.equal o0 o1
        | _ -> false
      in
      let wid () = width a = width b in
      let names () =
        if check_names
        then try [%compare.equal: string list] (names a) (names b) with _ -> true
        else true
      in
      let deps () =
        if check_deps
        then
          try
            List.fold2_exn (deps a) (deps b) ~init:true ~f:(fun b x y ->
              b && (structural_compare s x y))
          with _ ->
            false
        else
          false
      in
      typ () && wid () && names () && deps ()
  in
  structural_compare (Set.empty (module Uid)) a b

let rec sexp_of_instantiation_recursive ?show_uids ~depth inst =
  let sexp_of_next s = sexp_of_signal_recursive ?show_uids ~depth:(depth-1) s in
  let name =
    if String.is_empty inst.inst_lib
    then inst.inst_name
    else inst.inst_lib ^ "." ^ inst.inst_name in
  let name =
    if String.is_empty inst.inst_arch
    then name
    else name ^ "(" ^ inst.inst_arch ^ ")" in
  let sexp_of_output_width (w, _) = [%sexp (w : int)] in
  [%message
    name
      ~parameters:(inst.inst_generics : Parameter.t list)
      ~inputs:(inst.inst_inputs : (string * next) list)
      ~outputs:(inst.inst_outputs: (string * output_width) list)]

and sexp_of_register_recursive ?show_uids ~depth reg =
  let sexp_of_next s = sexp_of_signal_recursive ?show_uids ~depth:(depth-1) s in
  let sexp_of_opt g s =
    match g with
    | Empty -> None
    | _ -> Some (sexp_of_next s)
  in
  let sexp_of_edge ?(rising="rising") ?(falling="falling") g s =
    match g with
    | Empty -> None
    | _ ->
      match s with
      | Const (_, "1") -> Some [%sexp (rising : string)]
      | Const (_, "0") -> Some [%sexp (falling : string)]
      | _ -> Some (sexp_of_next s)
  in
  let sexp_of_level = sexp_of_edge ~rising:"high" ~falling:"low" in
  [%message
    ""
      ~clock:      (sexp_of_next  reg.reg_clock                           : Sexp.t)
      ~clock_edge: (sexp_of_edge  reg.reg_clock_level reg.reg_clock_level : Sexp.t sexp_option)
      ~reset:      (sexp_of_opt   reg.reg_reset       reg.reg_reset       : Sexp.t sexp_option)
      ~reset_edge: (sexp_of_edge  reg.reg_reset       reg.reg_reset_level : Sexp.t sexp_option)
      ~reset_value:(sexp_of_opt   reg.reg_reset       reg.reg_reset_value : Sexp.t sexp_option)
      ~clear:      (sexp_of_opt   reg.reg_clear       reg.reg_clear       : Sexp.t sexp_option)
      ~clear_edge: (sexp_of_level reg.reg_clear       reg.reg_clear_level : Sexp.t sexp_option)
      ~clear_value:(sexp_of_opt   reg.reg_clear       reg.reg_clear_value : Sexp.t sexp_option)
      ~enable:     (sexp_of_opt   reg.reg_enable      reg.reg_enable      : Sexp.t sexp_option)]

and sexp_of_memory_recursive
      ?show_uids
      ~depth
      (size, write_address, read_address, write_enable) =
  let sexp_of_signal s = sexp_of_signal_recursive ?show_uids ~depth:(depth-1) s in
  [%message
    ""
      (size : int)
      (write_address : signal)
      (read_address  : signal)
      (write_enable  : signal)]

and sexp_of_signal_recursive ?(show_uids=false) ~depth signal =
  let display_const c =
    if String.length c <= 8
    then "0b" ^ c
    else "0x" ^ Utils.hstr_of_bstr Unsigned c
  in
  let tag =
    match signal with
    | Empty         -> "empty"
    | Const  _      -> "const"
    | Op    (_, op) -> string_of_op op
    | Wire   _      -> "wire"
    | Select _      -> "select"
    | Inst   _      -> "instantiation"
    | Reg    _      -> "register"
    | Mem    _      -> "memory"
  in
  if depth = 0 || is_empty signal
  then (
    match signal with
    | Empty -> [%message "empty"]
    | Const (_, value) -> [%sexp (display_const value : string)]
    | _ ->
      match names signal with
      | [] ->
        if show_uids
        then [%sexp (uid signal : Uid.t)]
        else [%sexp (tag : string)]
      | [ name ] -> [%sexp (name : string)]
      | names -> [%sexp (names : string list)])
  else (
    let sexp_of_next          = sexp_of_signal_recursive        ~show_uids ~depth:(depth-1) in
    let sexp_of_instantiation = sexp_of_instantiation_recursive ~show_uids ~depth           in
    let sexp_of_memory        = sexp_of_memory_recursive        ~show_uids ~depth           in
    let sexp_of_register      = sexp_of_register_recursive      ~show_uids ~depth           in
    let uid = if show_uids then Some (uid signal) else None in
    let names = match names signal with [] -> None | names -> Some names in
    let width = width signal in
    let create
          ?value
          ?arguments
          ?select
          ?data
          ?range
          ?instantiation
          ?register
          ?memory
          ?data_in
          constructor =
      [%message
        constructor
          (uid              : Uid.t         sexp_option)
          (names            : string list   sexp_option)
          (width            : int)
          (value            : string        sexp_option)
          (range            : (int*int)     sexp_option)
          (select           : next          sexp_option)
          (data             : next list     sexp_option)
          ~_:(instantiation : instantiation sexp_option)
          ~_:(register      : register      sexp_option)
          ~_:(memory        : memory        sexp_option)
          (arguments        : next list     sexp_option)
          (data_in          : next          sexp_option)]
    in
    match signal with
    | Empty -> create "empty"
    | Const (_, value) -> create tag ~value:(display_const value)
    | Op (_, Signal_mux) ->
      (match deps signal with
       | select :: data -> create tag ~select ~data
       | deps -> create "MUX IS BADLY FORMED" ~arguments:deps)
    | Op _  -> create tag ~arguments:(deps signal)
    | Wire (_, s) -> create tag ~data_in:!s
    | Select (_, high, low) ->
      (match deps signal with
       | [ data_in ] -> create tag ~data_in ~range:(high, low)
       | deps -> create "SELECT IS BADLY FORMED" ~arguments:deps)
    | Inst (_, _, instantiation) -> create tag ~instantiation
    | Reg (_, register) ->
      (match deps signal with
       | data :: _ -> create tag ~register ~data_in:data
       | deps -> create "REGISTER IS BADLY FORMED" ~arguments:deps)
    | Mem (_, _, register, memory) ->
      (match deps signal with
       | data :: write_address :: read_address :: write_enable :: _ ->
         create tag ~register ~data_in:data
           ~memory:(memory.mem_size, write_address, read_address, write_enable)
       | deps -> create "MEMORY IS BADLY FORMED" ~arguments:deps))

let sexp_of_t s = sexp_of_signal_recursive ~show_uids:false ~depth:1 s

let sexp_of_register register = sexp_of_register_recursive register ~depth:1

type signal = t [@@deriving sexp_of]

let const_value =
  let sexp_of_signal = sexp_of_signal_recursive ~depth:1 in
  function
  | Const (_, v) -> v
  | signal ->
    raise_s [%message
      "cannot get the value of a non-constant signal" ~_:(signal : signal)]

module Base = struct

  (* TODO: instantiations, memories *)

  type nonrec t = t

  let equal (t1 : t) t2 =
    match t1, t2 with
    | Empty        , Empty         -> true
    | Const (_, c1), Const (_, c2) -> String.equal c1 c2
    | _ -> Uid.equal (uid t1) (uid t2)

  let width = width

  let to_string = to_string

  let sexp_of_t = sexp_of_t

  let to_int signal =
    if is_const signal
    then Utils.int_of_bstr (const_value signal)
    else raise_s [%message "cannot use [to_int] on non-constant signal" ~_:(signal : t)]

  let to_bstr signal =
    if is_const signal
    then const_value signal
    else raise_s [%message "cannot use [to_bstr] on non-constant signal" ~_:(signal : t)]

  let empty = Empty

  let is_empty = function Empty -> true | _ -> false

  (* XXX warning!!! internal state is kept here - reset_id will no longer work *)
  let const =
    let optimise = false in
    if not optimise
    then (fun b -> Const (make_id (String.length b) [], b))
    else
      let map = ref (Map.empty (module String)) in
      let tryfind b _ = Map.find !map b in
      let f b =
        match tryfind b !map with
        | None ->
          let s = Const (make_id (String.length b) [], b) in
          map := Map.set !map ~key:b ~data:s;
          s
        | Some x -> x
      in
      f

  let (--) signal name =
    match signal with
    | Empty ->
      raise_s [%message
        "attempt to set the name of the empty signal" ~to_:(name : string)]
    | Const (s, _)
    | Op (s, _)
    | Reg (s, _)
    | Select (s, _, _)
    | Mem (s, _, _, _)
    | Inst (s, _, _)
    | Wire (s, _) ->
      s.s_names <- name :: s.s_names;
      signal
  let op2 op len a b = Op (make_id len [ a; b ], op)
  let concat a =
    (* automatically concatenate successive constants *)
    let rec optimise_consts l =
      match l with
      | [] -> []
      | a :: [] -> [ a ]
      | a :: b :: tl ->
        if (is_const a) && (is_const b)
        then optimise_consts ((const (const_value a ^ const_value b)) :: tl)
        else a :: optimise_consts (b :: tl)
    in
    let a = optimise_consts a in
    match a with
    | [ a ] -> a
    | _ ->
      let len = List.fold a ~init:0 ~f:(fun acc a -> acc + width a) in
      Op (make_id len a, Signal_cat)
  let select a hi lo = Select (make_id (hi-lo+1) [ a ], hi, lo)
  let (+:) a b = op2 Signal_add (width a) a b
  let (-:) a b = op2 Signal_sub (width a) a b
  let ( *: ) a b = op2 Signal_mulu (width a + width b) a b
  let ( *+ ) a b = op2 Signal_muls (width a + width b) a b
  let (&:) a b = op2 Signal_and (width a) a b
  let (|:) a b = op2 Signal_or (width a) a b
  let (^:) a b = op2 Signal_xor (width a) a b
  let (~:) a = Op (make_id (width a) [ a ], Signal_not)
  let (==:) a b = op2 Signal_eq 1 a b
  let (<:) a b = op2 Signal_lt 1 a b

  let mux sel l =
    Op (make_id (width (List.hd_exn l)) (sel :: l), Signal_mux)
end

module Comb_make = Comb.Make
module Comb = Comb.Make (Base)

include (Comb : (module type of struct include Comb end with type t := t))

let (<==) a b =
  match a with
  | Wire (_, d) ->
    if not (is_empty !d)
    then
      raise_s [%message
        "attempt to assign wire multiple times"
          ~already_assigned_wire:(a : t)
          ~expression:(b : t)];
    if width a <> width b
    then
      raise_s [%message
        "attempt to assign expression to wire of different width"
          ~wire_width:(width a : int)
          ~expression_width:(width b : int)
          ~wire:(a : t)
          ~expression:(b : t)];
    d := b
  | _ ->
    raise_s [%message
      "attempt to assign non-wire"
        ~assignment_target:(a : t)
        ~expression:(b : t)]

let assign = ( <== )

let wire w = Wire (make_id w [], ref Empty)

let wireof s =
  let x = wire (width s) in
  x <== s;
  x

let input name width = wire width -- name

let output name s =
  let w = wire (width s) -- name in
  w <== s;
  w

let clock = (wire 1) -- "clock"
let reset = (wire 1) -- "reset"
let clear = (wire 1) -- "clear"
let enable = (wire 1) -- "enable"

module Const_prop = struct

  module Base = struct

    include Comb

    let cv s = Bits.const (const_value s)
    let eqs s n =
      let d = Bits.(==:) (cv s) (Bits.consti (width s) n) in
      Bits.to_int d = 1
    let cst b = const (Bits.to_string b)

    let (+:) a b =
      match is_const a, is_const b with
      | true, true -> cst (Bits.(+:) (cv a) (cv b))
      | true, false when eqs a 0 -> b (* 0+b *)
      | false, true when eqs b 0 -> a (* a+0 *)
      | _ -> (+:) a b

    let (-:) a b =
      match is_const a, is_const b with
      | true, true -> cst (Bits.(-:) (cv a) (cv b))
      (* | true, false when eqs a 0 -> b *)
      | false, true when eqs b 0 -> a (* a-0 *)
      | _ -> (-:) a b

    let ( *: ) a b =
      let w = width a + width b in
      let opt d c =
        if eqs c 0
        then zero w
        else if eqs c 1
        then (zero (width c)) @: d
        else (
          let c = cv c in
          if Bits.to_int @@ Bits.popcount c <> 1
          then ( *: ) a b
          else (
            let p = Bits.to_int @@ (Bits.floor_log2 c).value in
            if p = 0
            then uresize d w
            else uresize (d @: (zero p)) w))
      in
      match is_const a, is_const b with
      | true, true -> cst (Bits.( *: ) (cv a) (cv b))
      | true, false -> opt b a
      | false, true -> opt a b
      | _ -> ( *: ) a b

    let ( *+ ) a b =
      match is_const a, is_const b with
      | true, true -> cst (Bits.( *+ ) (cv a) (cv b))
      (* | we could do certain optimisations here *)
      | _ -> ( *+ ) a b

    let (&:) a b =
      let opt d c =
        if eqs c 0
        then zero (width a)
        else if eqs c (-1)
        then d
        else (&:) a b
      in
      match is_const a, is_const b with
      | true, true -> cst (Bits.(&:) (cv a) (cv b))
      | true, false -> opt b a
      | false, true -> opt a b
      | _ -> (&:) a b

    let (|:) a b =
      let opt d c =
        if eqs c 0
        then d
        else if eqs c (-1)
        then ones (width a)
        else (|:) a b
      in
      match is_const a, is_const b with
      | true, true -> cst (Bits.(|:) (cv a) (cv b))
      | true, false -> opt b a
      | false, true -> opt a b
      | _ -> (|:) a b

    let (^:) a b =
      match is_const a, is_const b with
      | true, true -> cst (Bits.(^:) (cv a) (cv b))
      | _ -> (^:) a b

    let (==:) a b =
      match is_const a, is_const b with
      | true, true -> cst (Bits.(==:) (cv a) (cv b))
      | _ -> (==:) a b

    let (~:) a =
      match is_const a with
      | true -> cst (Bits.(~:) (cv a))
      | _ -> (~:) a

    let (<:) a b =
      match is_const a, is_const b with
      | true, true -> cst (Bits.(<:) (cv a) (cv b))
      | _ -> (<:) a b

    let concat l =
      let rec f l nl =
        match l with
        | [] -> List.rev nl
        | h :: t when is_const h ->
          (match nl with
           | h' :: t' when is_const h' ->
             f t (cst (Bits.concat [ cv h'; cv h ]) :: t')
           | _ -> f t (h :: nl))
        | h :: t -> f t (h :: nl)
      in
      concat (f l [])

    (* {[
         let is_rom els =
           List.fold (fun b s -> b && is_const s) true els

         let opt_rom sel els =
           let len = List.length els in
           let len' = 1 lsl (width sel) in
           let els =
             if len' <> len
             then
               let e = List.nth els (len'-1) in
               els @ linit (len'-len) (fun _ -> e)
             else
               els
           in
           mux sel els
       ]} *)

    let mux sel els =
      let len = List.length els in
      (*let len' = 1 lsl (width sel) in*)
      if is_const sel
      then
        let x = Bits.to_int (cv sel) in
        let x = min x (len-1) in (* clip select *)
        List.nth_exn els x
        (* {[
           else if is_rom els && len <= len'
             then
               opt_rom sel els
           ]} *)
      else
        mux sel els

    let select d h l =
      if is_const d
      then cst (Bits.select (cv d) h l)
      else if l=0 && h = width d - 1
      then d
      else select d h l
  end

  module Comb = Comb_make (Base)
end

(* register with async reset *)
let r_async =
  { reg_clock       = clock
  ; reg_clock_level = vdd
  ; reg_reset       = reset
  ; reg_reset_level = vdd
  ; reg_reset_value = empty
  ; reg_clear       = empty
  ; reg_clear_level = empty
  ; reg_clear_value = empty
  ; reg_enable      = empty }

(* register with sync clear *)
let r_sync =
  { reg_clock       = clock
  ; reg_clock_level = vdd
  ; reg_reset       = empty
  ; reg_reset_level = empty
  ; reg_reset_value = empty
  ; reg_clear       = clear
  ; reg_clear_level = vdd
  ; reg_clear_value = empty
  ; reg_enable      = empty }

let r_full =
  { reg_clock       = clock
  ; reg_clock_level = vdd
  ; reg_reset       = reset
  ; reg_reset_level = vdd
  ; reg_reset_value = empty
  ; reg_clear       = clear
  ; reg_clear_level = vdd
  ; reg_clear_value = empty
  ; reg_enable      = empty }

let r_none =
  { reg_clock       = clock
  ; reg_clock_level = vdd
  ; reg_reset       = empty
  ; reg_reset_level = empty
  ; reg_reset_value = empty
  ; reg_clear       = empty
  ; reg_clear_level = empty
  ; reg_clear_value = empty
  ; reg_enable      = empty }

(* error checking *)
let assert_width signal w msg =
  if width signal <> w
  then
    raise_s [%message
      msg
        ~info:"signal has unexpected width"
        ~expected_width:(w : int)
        (signal : t)]

let assert_width_or_empty signal w msg =
  if not (is_empty signal) && width signal <> w
  then
    raise_s [%message
      msg
        ~info:"signal should have expected width or be empty"
        ~expected_width:(w : int)
        (signal : t)]

let assert_vdd_gnd_empty signal msg =
  if not (is_gnd signal || is_vdd signal || is_empty signal)
  then
    raise_s [%message
      msg
        ~info:("signal should be [vdd], [gnd] or [empty]." : string)
        (signal : t)]

let form_spec spec enable d =
  assert_width          spec.reg_clock              1  "clock is invalid";
  assert_vdd_gnd_empty  spec.reg_clock_level           "clock level is invalid";
  assert_width_or_empty spec.reg_reset              1  "reset is invalid";
  assert_vdd_gnd_empty  spec.reg_reset_level           "reset level is invalid";
  assert_width_or_empty spec.reg_reset_value (width d) "reset value is invalid";
  assert_width_or_empty spec.reg_clear              1  "clear signal is invalid";
  assert_vdd_gnd_empty  spec.reg_clear_level           "clear level is invalid";
  assert_width_or_empty spec.reg_clear_value (width d) "clear value is invalid";
  assert_width_or_empty spec.reg_enable             1  "enable is invalid";
  assert_width_or_empty enable                      1  "enable is invalid";
  { spec with
    reg_clock_level = if is_empty spec.reg_clock_level then vdd else spec.reg_clock_level
  ; reg_reset_level = if is_empty spec.reg_reset_level then vdd else spec.reg_reset_level
  ; reg_reset_value =
      if is_empty spec.reg_reset_value then zero (width d) else spec.reg_reset_value
  ; reg_clear_value =
      if is_empty spec.reg_clear_value then zero (width d) else spec.reg_clear_value
  ; reg_enable =
      let e0 = if is_empty spec.reg_enable then vdd else spec.reg_enable in
      let e1 = if is_empty enable then vdd else enable in
      if is_vdd e0 && is_vdd e1
      then vdd
      else if is_vdd e0
      then e1
      else if is_vdd e1
      then e0
      else e0 &: e1 }

module Reg_spec_ = struct
  type t = register [@@deriving sexp_of]

  let override
        ?clk ?clkl
        ?r ?rl ?rv
        ?c ?cl ?cv
        ?ge spec =
    { reg_clock       = Option.value clk  ~default:spec.reg_clock
    ; reg_clock_level = Option.value clkl ~default:spec.reg_clock_level
    ; reg_reset       = Option.value r    ~default:spec.reg_reset
    ; reg_reset_level = Option.value rl   ~default:spec.reg_reset_level
    ; reg_reset_value = Option.value rv   ~default:spec.reg_reset_value
    ; reg_clear       = Option.value c    ~default:spec.reg_clear
    ; reg_clear_level = Option.value cl   ~default:spec.reg_clear_level
    ; reg_clear_value = Option.value cv   ~default:spec.reg_clear_value
    ; reg_enable      = Option.value ge   ~default:spec.reg_enable }

  let create ?clr ?rst () ~clk =
    let spec =
      match clr, rst with
      | None     , None     -> r_none
      | None     , Some rst -> { r_async with reg_reset = rst }
      | Some clr , None     -> { r_sync with reg_clear = clr }
      | Some clr , Some rst -> { r_full with reg_reset = rst; reg_clear = clr } in
    { spec with reg_clock = clk }
end

let reg spec ~e:enable d =
  let spec = form_spec spec enable d in
  let deps =
    [ spec.reg_clock
    ; spec.reg_reset; spec.reg_reset_value; spec.reg_reset_level
    ; spec.reg_clear; spec.reg_clear_value; spec.reg_clear_level
    ; spec.reg_enable ]
  in
  Reg (make_id (width d) (d :: deps), spec)

let reg_fb spec ~e ~w:width f =
  let d = wire width in
  let q = reg spec ~e d in
  d <== (f q);
  q

let rec pipeline spec ~n ~e d =
  if n=0
  then d
  else reg spec ~e (pipeline ~n:(n-1) spec ~e d)

module Ram_spec_ = struct
  type t = register [@@deriving sexp_of]

  let create () ~clk = { r_none with reg_clock = clk }

  let override = Reg_spec_.override
end

let memory spec size ~we ~wa ~d ~ra =
  assert (width we = 1);
  assert (size <= (1 lsl width wa));
  assert (size > 0);
  assert (width wa = width ra);
  let spec = form_spec spec we d in
  let deps =
    [ d; wa; ra; we
    ; spec.reg_clock
    ; spec.reg_reset; spec.reg_reset_value; spec.reg_reset_level
    ; spec.reg_clear; spec.reg_clear_value; spec.reg_clear_level
    ; spec.reg_enable ]
  in
  Mem (make_id (width d) deps,
       new_id (),
       spec,
       { mem_size          = size
       ; mem_write_address = wa
       ; mem_read_address  = ra })

let ram_rbw spec size ~we ~wa ~d ~re ~ra =
  reg spec ~e:re (memory spec size ~we ~wa ~d ~ra)

let ram_wbr spec size ~we ~wa ~d ~re ~ra =
  memory spec size ~we ~wa ~d ~ra:(reg spec ~e:re ra)
