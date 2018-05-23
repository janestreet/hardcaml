open! Import

module Config = struct
  type t =
    | Serial
    | Sklansky
    | Brent_kung
    | Kogge_stone
  [@@deriving enumerate, sexp_of]
end

open Config

let serial (+:) l =
  match l with
  | [] -> assert false
  | h :: t ->
    let o = List.fold t ~init:[ h ] ~f:(fun accum a -> (List.hd_exn accum +: a) :: accum) in
    List.rev o

let split_pow2 l =
  match List.length l with
  | 0 -> [], []
  | 1 -> l, []
  | w -> List.split_n l (Int.floor_pow2 (w - 1))

let rec sklansky (+:) l =
  match l with
  | [] -> failwith "sklansky"
  | [ a ] -> [ a ]
  | _ ->
    let s, t = split_pow2 l in
    let s = sklansky (+:) s in
    let t = sklansky (+:) t in
    let s_last = List.last_exn s in
    s @ List.map t ~f:(fun t -> s_last +: t)

let rec pairs = function
  | [] -> []
  | a :: b :: tl -> (a, b) :: pairs tl
  | _ -> failwith "pairs"

let rec brent_kung (+:) l =
  match l with
  | [] -> failwith "brent_kung"
  | [ a ] -> [ a ]
  | _ ->
    let p = pairs l in
    let l = List.map p ~f:(fun (a, b) -> a +: b) in
    let l = brent_kung (+:) l in
    let p = List.map p ~f:fst in
    match p with
    | [] -> assert false
    | ph :: pt ->
      let lt, lh =
        match List.rev l with
        | [] -> assert false
        | h :: t -> List.rev t, h
      in
      let o = List.concat (List.map2_exn lt pt ~f:(fun a b -> [ a; a +: b ])) in
      ph :: (o @ [lh])

let kogge_stone (+:) l =
  let length = List.length l in (* must be power of two *)
  if not (Int.is_pow2 length)
  then raise_s [%message
         "Kogge_stone adder requires input list's length to be a power of two"
           (length : int)];
  let rec loop n =
    if n = 0
    then l
    else
      let l = loop (n/2) in
      let l0, l1, l2 = List.take l n, List.drop l n, List.take l (length-n) in
      l0 @ (List.map2_exn l2 l1 ~f:(+:))
  in
  loop (length / 2)

let network = function
  | Serial -> serial
  | Sklansky -> sklansky
  | Brent_kung -> brent_kung
  | Kogge_stone -> kogge_stone

let eval ~config ~operator inputs = network config operator inputs

let create (type t) (module Bits : Hardcaml.Comb.S with type t = t)
      ~config ~input1 ~input2 ~carry_in =
  let open Bits in
  let module A = Add.Make (Bits) in
  let open A.Add_result in
  let module Gp = struct
    type t =
      { (* [g] is short for "generate carry without including [carry_in]"*)
        g : Bits.t
      (* [p] is short for "[carry_in] is propagated" *)
      ; p : Bits.t }
    let ( + ) t1 t2 =
      { g = t2.g |: (t1.g &: t2.p)
      ; p = t1.p &: t2.p }
      (* Proof of associativity of [+].
         {[
           (t1 + t2) + t3
           = { g = t2.g |: (t1.g &: t2.p); p = t1.p &: t2.p } + t3
           = { g = t3.g |: ((t2.g |: (t1.g &: t2.p)) &: t3.p)
             ; p = t1.p &: t2.p &: t3.p }
         ]}
         {[
           t1 + (t2 + t3)
           = t1 + { g = t3.g |: (t2.g &: t3.p); p = t2.p &: t3.p }
           = { g = t3.g |: (t2.g &: t3.p) |: (t1.g & t2.p &: t3.p)
             ; p = t1.p &: t2.p &: t3.p }
         ]}
         So we need:
         {[
           (t2.g |: (t1.g &: t2.p)) &: t3.p
           = (t2.g &: t3.p) |: (t1.g & t2.p &: t3.p)
         ]}
         which follows by distributing [&:] over [|:]. *)
  end in
  let open Gp in
  let input_bits = List.rev (List.zip_exn (bits input1) (bits input2)) in
  let half_adders = List.map input_bits ~f:(fun (a, b) -> A.half_adder a b) in
  (* The i'th element of [prefix_sums] has [p] set iff the first i half adders
     have [sum = 1].  It has [g] set iff the sum of the low i bits of [input1]
     and [input2] is >= 2^i, i.e. will generate a carry.  *)
  let prefix_sums =
    network config ( + )
      (List.map half_adders ~f:(fun { A.Add_result. carry; sum } ->
         { g = carry; p = sum })) in
  let input_bit_sums = List.map half_adders ~f:(fun { sum; _ } -> sum) @ [ gnd ] in
  let carry_ins =
    carry_in
    :: List.map prefix_sums ~f:(fun { g; p } -> g |: (p &: carry_in)) in
  let sums =
    List.map2_exn input_bit_sums carry_ins
      ~f:(fun input_bit_sum carry_in -> input_bit_sum ^: carry_in) in
  concat (List.rev sums)
