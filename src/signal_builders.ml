open! Core0

module Const_prop (Unoptimized : sig
    include Comb.S

    val is_const : t -> bool
    val const_value : t -> Bits.t
  end) =
struct
  module Base = struct
    include Unoptimized

    let cv s = const_value s

    let eqs s n =
      let d = Bits.( ==: ) (cv s) (Bits.of_int_trunc ~width:(width s) n) in
      Bits.to_int_trunc d = 1
    ;;

    let cst b = of_constant (Bits.to_constant b)

    let ( +: ) a b =
      match is_const a, is_const b with
      | true, true -> cst (Bits.( +: ) (cv a) (cv b))
      | true, false when eqs a 0 -> b (* 0+b *)
      | false, true when eqs b 0 -> a (* a+0 *)
      | _ -> a +: b
    ;;

    let ( -: ) a b =
      match is_const a, is_const b with
      | true, true -> cst (Bits.( -: ) (cv a) (cv b))
      (*=| true, false when eqs a 0 -> b *)
      | false, true when eqs b 0 -> a (* a-0 *)
      | _ -> a -: b
    ;;

    let ( *: ) a b =
      let w = width a + width b in
      let opt d c =
        if eqs c 0
        then zero w
        else if eqs c 1
        then zero (width c) @: d
        else (
          let c = cv c in
          if Bits.to_int_trunc @@ Bits.popcount c <> 1
          then a *: b
          else (
            let p = Bits.to_int_trunc @@ (Bits.floor_log2 c).value in
            if p = 0 then uresize d ~width:w else uresize (d @: zero p) ~width:w))
      in
      match is_const a, is_const b with
      | true, true -> cst (Bits.( *: ) (cv a) (cv b))
      | true, false -> opt b a
      | false, true -> opt a b
      | _ -> a *: b
    ;;

    let ( *+ ) a b =
      match is_const a, is_const b with
      | true, true -> cst (Bits.( *+ ) (cv a) (cv b))
      (*=| we could do certain optimisations here *)
      | _ -> a *+ b
    ;;

    let ( &: ) a b =
      let opt d c =
        if eqs c 0 then zero (width a) else if eqs c (-1) then d else a &: b
      in
      match is_const a, is_const b with
      | true, true -> cst (Bits.( &: ) (cv a) (cv b))
      | true, false -> opt b a
      | false, true -> opt a b
      | _ -> a &: b
    ;;

    let ( |: ) a b =
      let opt d c =
        if eqs c 0 then d else if eqs c (-1) then ones (width a) else a |: b
      in
      match is_const a, is_const b with
      | true, true -> cst (Bits.( |: ) (cv a) (cv b))
      | true, false -> opt b a
      | false, true -> opt a b
      | _ -> a |: b
    ;;

    let ( ^: ) a b =
      match is_const a, is_const b with
      | true, true -> cst (Bits.( ^: ) (cv a) (cv b))
      | _ -> a ^: b
    ;;

    let ( ==: ) a b =
      match is_const a, is_const b with
      | true, true -> cst (Bits.( ==: ) (cv a) (cv b))
      | _ -> a ==: b
    ;;

    let ( ~: ) a =
      match is_const a with
      | true -> cst (Bits.( ~: ) (cv a))
      | _ -> ~:a
    ;;

    let ( <: ) a b =
      match is_const a, is_const b with
      | true, true -> cst (Bits.( <: ) (cv a) (cv b))
      | _ -> a <: b
    ;;

    let concat_msb l =
      let optimise_consts l =
        List.group l ~break:(fun a b -> not (Bool.equal (is_const a) (is_const b)))
        |> List.map ~f:(function
          | [] -> []
          | [ x ] -> [ x ]
          | h :: _ as l ->
            if is_const h
            then [ List.map l ~f:const_value |> Bits.concat_msb |> cst ]
            else l)
        |> List.concat
      in
      concat_msb (optimise_consts l)
    ;;

    (* {[
         let is_rom els = List.fold (fun b s -> b && is_const s) true els

         let opt_rom sel els =
           let len = List.length els in
           let len' = 1 lsl width sel in
           let els =
             if len' <> len
             then (
               let e = List.nth els (len' - 1) in
               els @ linit (len' - len) (fun _ -> e))
             else els
           in
           mux sel els
         ;;
       ]} *)

    let mux sel els =
      let len = List.length els in
      (* let len' = 1 lsl (width sel) in *)
      if is_const sel
      then (
        let x = Bits.to_int_trunc (cv sel) in
        let x = min x (len - 1) in
        (* clip select *)
        List.nth_exn els x
        (* {[
             else if is_rom els && len <= len'
               then
                 opt_rom sel els
           ]} *))
      else mux sel els
    ;;

    let select d ~high:h ~low:l =
      if is_const d
      then cst (Bits.select (cv d) ~high:h ~low:l)
      else if l = 0 && h = width d - 1
      then d
      else select d ~high:h ~low:l
    ;;
  end

  include Comb.Make (Base)
end

module Conversion_functions (Comb : Comb.S) = struct
  open Comb

  let of_bits x = of_constant (Bits.to_constant x)
  let to_bits x = Bits.of_constant (to_constant x)
end

module Memories (Comb : sig
    include Comb.S
    module Reg_spec : Reg_spec.S with type signal := t

    val reg
      :  ?enable:t
      -> ?initialize_to:Bits.t
      -> ?reset_to:Bits.t
      -> ?clear:t
      -> ?clear_to:t
      -> Reg_spec.t
      -> t
      -> t

    val multiport_memory_prim
      :  ?name:string
      -> ?attributes:Rtl_attribute.t list
      -> ?initialize_to:Bits.t array
      -> int
      -> remove_unused_write_ports:bool
      -> data_width:int
      -> write_ports:t Write_port.t array
      -> read_addresses:t array
      -> t array
  end) =
struct
  include Multiport_memory.Make (Comb)

  let memory ?attributes size ~write_port ~read_address =
    (multiport_memory
       ?attributes
       size
       ~write_ports:[| write_port |]
       ~read_addresses:[| read_address |]).(0)
  ;;

  let ram_rbw ?name ?attributes ~write_port ~(read_port : _ Read_port.t) size =
    let spec = Comb.Reg_spec.create ~clock:read_port.read_clock () in
    Comb.reg
      spec
      ~enable:read_port.read_enable
      (multiport_memory
         ?name
         ?attributes
         size
         ~write_ports:[| write_port |]
         ~read_addresses:[| read_port.read_address |]).(0)
  ;;

  let ram_wbr ?name ?attributes ~write_port ~(read_port : _ Read_port.t) size =
    let spec = Comb.Reg_spec.create ~clock:read_port.read_clock () in
    (multiport_memory
       size
       ?name
       ?attributes
       ~write_ports:[| write_port |]
       ~read_addresses:
         [| Comb.reg spec ~enable:read_port.read_enable read_port.read_address |]).(0)
  ;;
end

module Registers (Pre : sig
    include Comb.S

    type info

    val add_attribute : t -> Rtl_attribute.t -> t

    module Reg_spec : Reg_spec.S with type signal := t

    val reg__with_signal_reset
      :  ?enable:t
      -> ?initialize_to:Bits.t
      -> ?reset_to:t
      -> ?clear:t
      -> ?clear_to:t
      -> Reg_spec.t
      -> t
      -> t

    val wire : int -> t
    val assign : t -> t -> unit
    val to_rep : t -> Signal__type.t * info
    val update_rep : t -> info:info -> t
  end) =
struct
  open Pre

  let reg ?enable ?initialize_to ?reset_to ?clear ?clear_to spec d =
    let reset_to = Option.map reset_to ~f:(Fn.compose of_constant Bits.to_constant) in
    reg__with_signal_reset ?enable ?initialize_to ?reset_to ?clear ?clear_to spec d
  ;;

  let rec pipeline
    ?(attributes = [])
    ?enable
    ?initialize_to
    ?reset_to
    ?clear
    ?clear_to
    spec
    ~n
    d
    =
    let maybe_add_attributes s = List.fold attributes ~init:s ~f:add_attribute in
    if n = 0
    then d
    else
      maybe_add_attributes
        (reg
           ?enable
           ?initialize_to
           ?reset_to
           ?clear
           ?clear_to
           spec
           (pipeline
              ~attributes
              ?enable
              ?initialize_to
              ?reset_to
              ?clear
              ?clear_to
              spec
              ~n:(n - 1)
              d))
  ;;

  let prev ?enable ?initialize_to ?reset_to ?clear ?clear_to spec d =
    let tbl = Hashtbl.create (module Int) in
    Hashtbl.set tbl ~key:0 ~data:d;
    let rec f n =
      if n < 0
      then raise_s [%message "[Signal.prev] cannot accept a negative value" (n : int)];
      match Hashtbl.find tbl n with
      | Some x -> x
      | None ->
        let p = f (n - 1) in
        let r = reg spec ?enable ?initialize_to ?reset_to ?clear ?clear_to p in
        Hashtbl.set tbl ~key:n ~data:r;
        r
    in
    Staged.stage f
  ;;

  let cut_through_reg ?initialize_to ?reset_to ?clear ?clear_to spec ~enable d =
    let reg = reg ?initialize_to ?reset_to ?clear ?clear_to spec ~enable d in
    mux2 enable d reg
  ;;

  let reg_fb ?enable ?initialize_to ?reset_to ?clear ?clear_to spec ~width ~f =
    let _, info = to_rep (Reg_spec.clock spec) in
    let d = wire width |> update_rep ~info in
    let q = reg spec ?enable ?initialize_to ?reset_to ?clear_to ?clear d in
    assign d (f q);
    q
  ;;

  let counter ?enable ?initialize_to ?reset_to ?clear ?clear_to ?(by = 1) spec ~width =
    reg_fb ?enable ?initialize_to ?reset_to ?clear ?clear_to spec ~width ~f:(fun d ->
      d +: of_int_trunc ~width by)
  ;;
end
