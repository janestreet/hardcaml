open! Import
open Signal

include Fifo_intf.T
module Kinded_fifo = Fifo_intf.Kinded_fifo

(* Generates wbr memory with explicit collision detection to gurantee [wbr] behaviour.
   Despite what's suggested by Vivado's BRAM documentation, [write_first] are not
   respected, even in SDP RAM mode.
*)
let ram_wbr_safe capacity ~write_port ~read_port ~ram_attributes =
  let open Signal in
  let collision =
    reg
      (Reg_spec.create ~clock:write_port.write_clock ())
      ~enable:read_port.read_enable
      (write_port.write_enable
       &: read_port.read_enable
       &: (write_port.write_address ==: read_port.read_address))
  in
  mux2
    collision
    (reg
       (Reg_spec.create ~clock:write_port.write_clock ())
       ~enable:read_port.read_enable
       write_port.write_data)
    (ram_rbw capacity ~attributes:ram_attributes ~write_port ~read_port)
;;

let capacity_and_used_bits showahead ram_capacity =
  (* to be consistent with Vivado's FIFO implementation, when instantiating a fwft FIFO,
     it's actual capacity is one more due to the additional register in the prefetch
     buffer register. *)
  let actual_capacity = if showahead then ram_capacity + 1 else ram_capacity in
  let used_bits = num_bits_to_represent actual_capacity in
  actual_capacity, used_bits
;;

let create
      ?(showahead = false)
      ?(nearly_empty = 1)
      ?nearly_full
      ?(overflow_check = true)
      ?(reset = Signal.empty)
      ?(underflow_check = true)
      ?(ram_attributes = [ Rtl_attribute.Vivado.Ram_style.block ])
      ?scope
      ()
      ~capacity:ram_capacity
      ~clock
      ~clear
      ~wr
      ~d
      ~rd
  =
  let ( -- ) =
    match scope with
    | Some scope -> Scope.naming scope
    | None -> ( -- )
  in
  if Signal.is_empty clear && Signal.is_empty reset
  then
    raise_s
      [%message
        "[Fifo.create] requires either a synchronous clear or asynchronous reset"];
  let reg_spec = Reg_spec.create ~clock ~clear ~reset () in
  let reg ?clear_to ~enable d = reg (Reg_spec.override reg_spec ?clear_to) ~enable d in
  let abits = address_bits_for ram_capacity in
  let actual_capacity, used_bits = capacity_and_used_bits showahead ram_capacity in
  (* get nearly full/empty levels *)
  let nearly_full =
    match nearly_full with
    | None -> actual_capacity - 1
    | Some x -> x
  in
  let empty, full = wire 1, wire 1 in
  (* safe rd/wr signals assuming fifo neither full or empty *)
  let rd = if underflow_check then (rd &: ~:empty) -- "RD_INT" else rd in
  let wr = if overflow_check then (wr &: ~:full) -- "WR_INT" else wr in
  (* read or write, but not both *)
  let enable = rd ^: wr in
  (* fill level of fifo *)
  let used = wire used_bits in
  let used_next =
    mux2 enable (mux2 rd (used -:. 1) (used +:. 1)) used
    (* read+write, or none *)
  in
  used <== reg ~enable (used_next -- "USED_NEXT");
  (* full empty flags *)
  empty <== reg ~enable ~clear_to:vdd (used_next ==:. 0);
  full <== reg ~enable (used_next ==:. actual_capacity);
  (* nearly full/empty flags *)
  let nearly_empty = reg ~enable ~clear_to:vdd (used_next <:. nearly_empty) in
  let nearly_full = reg ~enable (used_next >=:. nearly_full) in
  (* read/write addresses within fifo *)
  let addr_count enable name =
    let a = wire abits in
    let an = mod_counter ~max:(ram_capacity - 1) a in
    a <== reg ~enable an;
    a -- name, an -- (name ^ "_NEXT")
  in
  let q =
    if showahead
    then (
      let used_is_one = reg ~enable:(rd ^: wr) (used_next ==:. 1) in
      let used_gt_one = reg ~enable:(rd ^: wr) (used_next >:. 1) in
      let memory =
        let wr = wr &: (used_gt_one |: (used_is_one &: ~:rd)) in
        let rd = rd &: used_gt_one in
        let ra, ra_n = addr_count rd "READ_ADDRESS" in
        let ra = mux2 rd ra_n ra -- "RA" in
        let wa, _ = addr_count wr "WRITE_ADDRESS" in
        ram_wbr_safe
          ~ram_attributes
          ram_capacity
          ~write_port:
            { write_clock = clock
            ; write_enable = wr
            ; write_address = wa
            ; write_data = d
            }
          ~read_port:{ read_clock = clock; read_enable = vdd; read_address = ra }
      in
      let bypass_cond = empty &: wr |: (used_is_one &: wr &: rd) in
      mux2 bypass_cond d memory |> reg ~enable:(bypass_cond |: rd))
    else (
      let ra, _ = addr_count rd "READ_ADDRESS" in
      let wa, _ = addr_count wr "WRITE_ADDRESS" in
      ram_rbw
        ~attributes:ram_attributes
        ram_capacity
        ~write_port:
          { write_clock = clock; write_enable = wr; write_address = wa; write_data = d }
        ~read_port:{ read_clock = clock; read_enable = rd; read_address = ra })
  in
  { q; full; empty; nearly_full; nearly_empty; used }
;;

let create_classic_with_extra_reg
      ?nearly_empty
      ?nearly_full
      ?overflow_check
      ?reset
      ?underflow_check
      ?ram_attributes
      ?scope
      ()
      ~capacity
      ~clock
      ~clear
      ~wr
      ~d
      ~rd
  =
  let spec = Reg_spec.create ~clock ~clear () in
  let fifo_valid = wire 1 in
  let middle_valid = wire 1 in
  let fifo_rd_en = wire 1 in
  let empty = ~:(fifo_valid |: middle_valid) in
  let will_update_dout = ~:empty &: rd in
  let will_update_middle = fifo_valid &: (middle_valid ==: will_update_dout) in
  let fifo =
    create
      ~showahead:false
      ?nearly_empty
      ?nearly_full
      ?overflow_check
      ?reset
      ?underflow_check
      ?ram_attributes
      ?scope
      ()
      ~capacity
      ~clock
      ~clear
      ~wr
      ~d
      ~rd:fifo_rd_en
  in
  let middle_dout = reg spec ~enable:will_update_middle fifo.q in
  fifo_rd_en <== (~:(fifo.empty) &: ~:(middle_valid &: fifo_valid));
  fifo_valid
  <== reg spec ~enable:(fifo_rd_en |: will_update_middle |: will_update_dout) fifo_rd_en;
  middle_valid
  <== reg spec ~enable:(will_update_middle |: will_update_dout) will_update_middle;
  { fifo with
    q = reg spec ~enable:will_update_dout (mux2 middle_valid middle_dout fifo.q)
  ; empty
  }
;;

let showahead_fifo_of_classic_fifo
      (create_fifo :
         capacity:int
       -> write_clock:Signal.t
       -> read_clock:Signal.t
       -> clear:Signal.t
       -> wr:Signal.t
       -> d:Signal.t
       -> rd:Signal.t
       -> (Signal.t, [ `Classic ]) Kinded_fifo.t)
  =
  Staged.stage (fun ~capacity ~write_clock ~read_clock ~clear ~wr ~d ~rd ->
    let spec = Reg_spec.create ~clock:read_clock ~clear () in
    let fifo_rd_en = wire 1 in
    let (Classic fifo) =
      create_fifo ~capacity ~write_clock ~read_clock ~clear ~wr ~d ~rd:fifo_rd_en
    in
    let dout_valid = reg spec ~enable:(fifo_rd_en |: rd) fifo_rd_en in
    let empty = ~:dout_valid in
    fifo_rd_en <== (~:(fifo.empty) &: (~:dout_valid |: rd));
    Kinded_fifo.Showahead { fifo with empty })
;;

let create_showahead_from_classic
      ?nearly_empty
      ?nearly_full
      ?overflow_check
      ?reset
      ?underflow_check
      ?ram_attributes
      ?scope
      ()
  =
  let create_fifo ~capacity ~write_clock ~read_clock ~clear ~wr ~d ~rd =
    assert (Signal.equal write_clock read_clock);
    create
      ~showahead:false
      ?nearly_empty
      ?nearly_full
      ?overflow_check
      ?reset
      ?underflow_check
      ?ram_attributes
      ?scope
      ()
      ~capacity
      ~clock:write_clock
      ~clear
      ~wr
      ~d
      ~rd
    |> Kinded_fifo.Classic
  in
  let create_showahead_fifo =
    Staged.unstage (showahead_fifo_of_classic_fifo create_fifo)
  in
  fun ~capacity ~clock ~clear ~wr ~d ~rd ->
    let write_clock = clock in
    let read_clock = clock in
    match create_showahead_fifo ~capacity ~write_clock ~read_clock ~clear ~wr ~d ~rd with
    | Kinded_fifo.Showahead fifo -> fifo
;;

let create_showahead_with_extra_reg
      ?nearly_empty
      ?nearly_full
      ?overflow_check
      ?reset
      ?underflow_check
      ?ram_attributes
      ?scope
      ()
      ~capacity
      ~clock
      ~clear
      ~wr
      ~d
      ~rd
  =
  let spec = Reg_spec.create ~clock ~clear () in
  let fifo_rd_en = wire 1 in
  let fifo =
    create
      ~showahead:false
      ?nearly_empty
      ?nearly_full
      ?overflow_check
      ?reset
      ?underflow_check
      ?ram_attributes
      ?scope
      ()
      ~capacity
      ~clock
      ~clear
      ~wr
      ~d
      ~rd:fifo_rd_en
  in
  let fifo_valid = wire 1 in
  let middle_valid = wire 1 in
  let dout_valid = wire 1 in
  let will_update_dout = middle_valid |: fifo_valid &: (rd |: ~:dout_valid) in
  let will_update_middle = fifo_valid &: (middle_valid ==: will_update_dout) in
  let empty = ~:dout_valid in
  let middle_dout = reg spec ~enable:will_update_middle fifo.q in
  let dout = reg spec ~enable:will_update_dout (mux2 middle_valid middle_dout fifo.q) in
  fifo_rd_en <== (~:(fifo.empty) &: ~:(middle_valid &: dout_valid &: fifo_valid));
  fifo_valid
  <== reg spec ~enable:(fifo_rd_en |: will_update_middle |: will_update_dout) fifo_rd_en;
  middle_valid
  <== reg spec ~enable:(will_update_middle |: will_update_dout) will_update_middle;
  dout_valid <== reg spec ~enable:(will_update_dout |: rd) will_update_dout;
  { fifo with q = dout; empty }
;;

module type Config = Fifo_intf.Config

module With_interface (Config : Config) = struct
  let _actual_capacity, used_bits =
    capacity_and_used_bits Config.showahead Config.capacity
  ;;

  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; wr : 'a
      ; d : 'a [@bits Config.data_width]
      ; rd : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type nonrec 'a t = 'a t

    include Interface.Make (struct
        include Fifo_intf.T

        let t = { t with q = "q", Config.data_width; used = "used", used_bits }
      end)
  end

  let create_fn
        ?nearly_empty
        ?nearly_full
        ?overflow_check
        ?reset
        ?underflow_check
        ?ram_attributes
        ?scope
        ~f
        (i : _ I.t)
    =
    f
      ?nearly_empty
      ?nearly_full
      ?overflow_check
      ?reset
      ?underflow_check
      ?ram_attributes
      ?scope
      ()
      ~capacity:Config.capacity
      ~clock:i.clock
      ~clear:i.clear
      ~wr:i.wr
      ~d:i.d
      ~rd:i.rd
  ;;

  let classic ?(extra_reg = false) =
    match extra_reg, Config.showahead with
    | false, false -> create_fn ~f:(create ~showahead:false)
    | true, false -> create_fn ~f:create_classic_with_extra_reg
    | false, true -> create_fn ~f:create_showahead_from_classic
    | true, true -> create_fn ~f:create_showahead_with_extra_reg
  ;;

  let create = create_fn ~f:(create ~showahead:Config.showahead)
end
