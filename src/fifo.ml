open! Import


type t =
  { q            : Signal.t
  ; full         : Signal.t
  ; empty        : Signal.t
  ; nearly_full  : Signal.t
  ; nearly_empty : Signal.t
  ; used         : Signal.t }
[@@deriving sexp_of]

let create
      ?(nearly_empty = 1)
      ?(nearly_full)
      ?(overflow_check = true)
      ?(rst = Signal.empty)
      ?(showahead = false)
      ?(underflow_check = true)
      ()
      ~capacity ~clk ~clr ~wr ~d ~rd =
  let open Signal in
  if Signal.is_empty clr && Signal.is_empty rst
  then raise_s [%message
         "[Fifo.create] requires either a synchronous clear or asynchronous reset"];
  let reg_spec = Reg_spec.create ~clk ~clr ~rst () in
  let ram_spec = Ram_spec.create ~clk () in
  let reg ?cv ~e d = reg (Reg_spec.override reg_spec ?cv) ~e d in
  let abits = Int.ceil_log2 capacity in
  let ubits = Utils.nbits capacity in
  (* get nearly full/empty levels *)
  let nearly_full = match nearly_full with None -> capacity-1 | Some x -> x in
  let empty, full = wire 1, wire 1 in
  (* safe rd/wr signals assuming fifo neither full or empty *)
  let rd = if underflow_check then (rd &: ~: empty) -- "RD_INT" else rd in
  let wr = if overflow_check then (wr &: ~: full) -- "WR_INT" else wr in
  (* read or write, but not both *)
  let e = rd ^: wr in
  (* fill level of fifo *)
  let used = wire ubits in
  let used_next =
    mux2 e
      (mux2 rd (used -:. 1) (used +:. 1))
      used (* read+write, or none *)
  in
  used <== reg ~e (used_next -- "USED_NEXT");
  (* full empty flags *)
  empty <== reg ~e ~cv:vdd (used_next ==:. 0);
  full <== reg ~e (used_next ==:. capacity);
  (* nearly full/empty flags *)
  let nearly_empty = reg ~e ~cv:vdd (used_next <:. nearly_empty) in
  let nearly_full = reg ~e (used_next >=:. nearly_full) in
  (* read/write addresses within fifo *)
  let addr_count e name =
    let a = wire abits in
    let an = mod_counter (capacity-1) a in
    a <== reg ~e an;
    a -- name, an -- (name ^ "_NEXT")
  in
  let ra, ra_n = addr_count rd "READ_ADDRESS" in
  let wa, _ = addr_count wr "WRITE_ADDRESS" in
  (* fifo memory *)
  let q =
    if showahead
    then
      let re = (rd |: (wr &: empty)) -- "RE" in
      let ra = (mux2 rd ra_n ra) -- "RA" in
      ram_wbr ram_spec ~we:wr ~re ~wa ~ra ~d capacity
    else
      ram_rbw ram_spec ~we:wr ~re:rd ~wa ~ra ~d capacity
  in
  { q
  ; full
  ; empty
  ; nearly_full
  ; nearly_empty
  ; used }
