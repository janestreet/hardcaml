open! Import
include Fixed_intf

module Make (B : Comb.S) = struct
  open B

  type unsigned
  type signed
  type 'a round = int -> B.t -> B.t
  type 'a overflow = int -> int -> B.t -> B.t

  module type Round = Round with module B := B
  module type Overflow = Overflow with module B := B
  module type Fixed = Fixed_point with module B := B

  let get_int fp s = select s (width s - 1) fp
  let get_frac fp s = if fp = 0 then empty else select s (fp - 1) 0
  let floor = get_int

  let ceil fp s =
    let ib = width s - fp in
    let max_frac = concat_msb_e [ zero ib; ones fp ] in
    get_int fp (s +: max_frac)
  ;;

  let half fp s =
    let ib = width s - fp in
    zero ib @: reverse (one fp)
  ;;

  module Unsigned = struct
    module Round = struct
      type t = unsigned round

      let neg_infinity fp s = floor fp (ue s)
      let pos_infinity fp s = ceil fp (ue s)
      let to_zero fp s = floor fp (ue s)
      let away_from_zero fp s = ceil fp (ue s)

      let tie_to_neg_infinity fp s =
        let half = half fp (ue s) in
        ceil fp (ue s -: half)
      ;;

      let tie_to_pos_infinity fp s =
        let half = half fp (ue s) in
        floor fp (ue s +: half)
      ;;

      let tie_to_zero fp s =
        let half = half fp (ue s) in
        ceil fp (ue s -: half)
      ;;

      let tie_away_from_zero fp s =
        let half = half fp (ue s) in
        floor fp (ue s +: half)
      ;;

      let tie_to_nearest_even fp s =
        let half = half fp (ue s) in
        let lsb = lsb (get_int fp s) in
        mux2 lsb (floor fp (ue s +: half)) (ceil fp (ue s -: half))
      ;;

      let tie_to_nearest_odd fp s =
        let half = half fp (ue s) in
        let lsb = lsb (get_int fp s) in
        mux2 lsb (ceil fp (ue s -: half)) (floor fp (ue s +: half))
      ;;

      let generic sel fp s =
        let s = ue s in
        let z = zero (width s) in
        let half = half fp s in
        let lsb = lsb (get_int fp s) in
        let rnd = mux sel [ z; z; z; z; half ] in
        let ceil = ceil fp (s -: rnd) in
        let floor = floor fp (s +: rnd) in
        let sel =
          mux
            sel
            [ vdd
            ; gnd
            ; vdd
            ; gnd (* directed rounding *)
            ; gnd
            ; vdd
            ; gnd
            ; vdd
            ; lsb
            ; ~:lsb
              (* round with tie break *)
            ]
        in
        mux2 sel floor ceil
      ;;

      let eval f = f
    end

    module Overflow = struct
      type t = unsigned overflow

      let wrap fp ib s = concat_msb_e [ select (get_int fp s) (ib - 1) 0; get_frac fp s ]

      let saturate fp ib s =
        let i = get_int fp s in
        let f = get_frac fp s in
        if width i = ib
        then s
        else if width i < ib
        then
          (*failwith "Overflow.Unsigned.Saturate"*)
          concat_msb_e [ zero (ib - width i); i; f ]
        else (
          let dropped = select i (width i - 1) ib in
          let remaining = select i (ib - 1) 0 in
          let overflow = reduce ~f:( |: ) (bits_msb dropped) in
          let clipped = mux2 overflow (ones (ib + fp)) (concat_msb_e [ remaining; f ]) in
          clipped)
      ;;

      let eval f = f
    end

    module type Spec = sig
      val round : unsigned round
      val overflow : unsigned overflow
    end

    module Make (S : Spec) = struct
      type t =
        { s : B.t
        ; fp : int
        }

      let mk fp s =
        if B.width s <= fp
        then
          (* could drop this requirement ... *)
          failwith "Fixed.Signal.mk: there must be at least 1 integer bit";
        { s; fp }
      ;;

      let int s = B.select s.s (B.width s.s - 1) s.fp

      let frac s =
        if s.fp < 0
        then failwith "Fixed.Unsigned.frac fp < 0"
        else if s.fp = 0
        then B.empty
        else B.select s.s (s.fp - 1) 0
      ;;

      let signal s = s.s
      let width_int s = B.width (int s)
      let width_frac s = B.width (frac s)

      let to_float s =
        let fp = 2. ** Float.of_int s.fp in
        let i = Float.of_int (B.to_int s.s) in
        i /. fp
      ;;

      let extend s n =
        if n < 0
        then failwith "Fixed.Unsigned.extend"
        else if n = 0
        then s
        else { s = B.concat_msb [ B.zero n; s.s ]; fp = s.fp }
      ;;

      let select_int s i =
        if i <= 0
        then failwith "Fixed.Unsigned.select_int i<=0"
        else (
          let si = int s in
          let wi = width_int s in
          if i <= wi then B.select si (i - 1) 0 else B.concat_msb [ B.zero (i - wi); si ])
      ;;

      let select_frac s f =
        if f < 0
        then failwith "Fixed.Unsigned.select_frac f<0"
        else if f = 0
        then B.empty
        else (
          let wf = width_frac s in
          if wf = 0
          then B.zero f
          else (
            let sf = frac s in
            if f <= wf
            then B.select sf (wf - 1) (wf - f)
            else B.concat_msb [ sf; B.zero (f - wf) ]))
      ;;

      let select s i f =
        let i' = select_int s i in
        let f' = select_frac s f in
        mk f (B.concat_msb_e [ i'; f' ])
      ;;

      let norm l =
        let i = List.fold l ~init:0 ~f:(fun a b -> max a (B.width (int b))) in
        let f = List.fold l ~init:0 ~f:(fun a b -> max a (B.width (frac b))) in
        List.map l ~f:(fun s -> select s i f)
      ;;

      let norm2 a b =
        let l = norm [ a; b ] in
        match l with
        | [ a; b ] -> a, b
        | _ -> failwith "Fixed.Unsigned.norm2"
      ;;

      let const ip fp f =
        let fp' = Float.of_int fp in
        let fp' = 2.0 ** fp' in
        mk fp (B.of_int ~width:(ip + fp) (Int.of_float (f *. fp')))
      ;;

      (* basic arithmetic *)

      let ( +: ) a b =
        let a, b = norm2 a b in
        let a, b = extend a 1, extend b 1 in
        { s = B.( +: ) a.s b.s; fp = a.fp }
      ;;

      let ( -: ) a b =
        let a, b = norm2 a b in
        let a, b = extend a 1, extend b 1 in
        { s = B.( -: ) a.s b.s; fp = a.fp }
      ;;

      let ( *: ) a b = { s = B.( *: ) a.s b.s; fp = a.fp + b.fp }

      (* comparison *)
      let ( ==: ) a b =
        let a, b = norm2 a b in
        B.( ==: ) a.s b.s
      ;;

      let ( <>: ) a b =
        let a, b = norm2 a b in
        B.( <>: ) a.s b.s
      ;;

      let ( <: ) a b =
        let a, b = norm2 a b in
        B.( <: ) a.s b.s
      ;;

      let ( <=: ) a b =
        let a, b = norm2 a b in
        B.( <=: ) a.s b.s
      ;;

      let ( >: ) a b =
        let a, b = norm2 a b in
        B.( >: ) a.s b.s
      ;;

      let ( >=: ) a b =
        let a, b = norm2 a b in
        B.( >=: ) a.s b.s
      ;;

      (* mux *)
      let mux sel l =
        let l = norm l in
        let fp = width_frac (List.hd_exn l) in
        let q = B.mux sel (List.map l ~f:signal) in
        mk fp q
      ;;

      (* resize with rounding and saturation control *)
      let resize s i f =
        let i' = width_int s in
        let f' = width_frac s in
        (* perform rounding *)
        let s = if f >= f' then select s i' f else mk f (S.round (f' - f) s.s) in
        (* perform overflow control *)
        mk f (S.overflow f i s.s)
      ;;
    end
  end

  module Signed = struct
    module Round = struct
      type t = signed round

      let neg_infinity fp s = floor fp (se s)
      let pos_infinity fp s = ceil fp (se s)

      let to_zero fp s =
        let sign = msb s in
        mux2 sign (ceil fp (se s)) (floor fp (se s))
      ;;

      let away_from_zero fp s =
        let sign = msb s in
        mux2 sign (floor fp (se s)) (ceil fp (se s))
      ;;

      let tie_to_neg_infinity fp s =
        let half = half fp (se s) in
        ceil fp (se s -: half)
      ;;

      let tie_to_pos_infinity fp s =
        let half = half fp (se s) in
        floor fp (se s +: half)
      ;;

      let tie_to_zero fp s =
        let half = half fp (se s) in
        let sign = msb s in
        mux2 sign (floor fp (se s +: half)) (ceil fp (se s -: half))
      ;;

      let tie_away_from_zero fp s =
        let half = half fp (se s) in
        let sign = msb s in
        mux2 sign (ceil fp (se s -: half)) (floor fp (se s +: half))
      ;;

      let tie_to_nearest_even fp s =
        let half = half fp (se s) in
        let lsb = lsb (get_int fp s) in
        mux2 lsb (floor fp (se s +: half)) (ceil fp (se s -: half))
      ;;

      let tie_to_nearest_odd fp s =
        let half = half fp (se s) in
        let lsb = lsb (get_int fp s) in
        mux2 lsb (ceil fp (se s -: half)) (floor fp (se s +: half))
      ;;

      let generic sel fp s =
        let s = se s in
        let z = zero (width s) in
        let half = half fp s in
        let lsb = lsb (get_int fp s) in
        let sign = msb s in
        let rnd = mux sel [ z; z; z; z; half ] in
        let ceil = ceil fp (s -: rnd) in
        let floor = floor fp (s +: rnd) in
        let sel =
          mux
            sel
            [ vdd
            ; gnd
            ; ~:sign
            ; sign (* directed rounding *)
            ; gnd
            ; vdd
            ; sign
            ; ~:sign
            ; lsb
            ; ~:lsb
              (* round with tie break *)
            ]
        in
        mux2 sel floor ceil
      ;;

      let eval f = f
    end

    module Overflow = struct
      type t = signed overflow

      let wrap fp ib s = concat_msb_e [ select (get_int fp s) (ib - 1) 0; get_frac fp s ]

      let saturate fp ib s =
        let i = get_int fp s in
        let f = get_frac fp s in
        if width i = ib
        then s
        else if width i < ib
        then
          (*failwith "Overflow.Signed.Saturate"*)
          concat_msb_e [ repeat (msb i) (ib - width i); i; f ]
        else (
          let dropped = select i (width i - 1) ib in
          let remaining = select i (ib - 1) 0 in
          let overflow_n = repeat (msb remaining) (width dropped) ==: dropped in
          let min = reverse (one (ib + fp)) in
          let max = ~:min in
          let clipped =
            mux2 overflow_n (concat_msb_e [ remaining; f ]) (mux2 (msb dropped) min max)
          in
          clipped)
      ;;

      let eval f = f
    end

    module type Spec = sig
      val round : signed round
      val overflow : signed overflow
    end

    module Make (S : Spec) = struct
      type t =
        { s : B.t
        ; fp : int
        }

      let mk fp s =
        if B.width s <= fp
        then
          (* could drop this requirement ... *)
          failwith "Fixed.Signal.mk: there must be at least 1 integer bit";
        { s; fp }
      ;;

      let int s = B.select s.s (B.width s.s - 1) s.fp

      let frac s =
        if s.fp < 0
        then failwith "Fixed.Signed.frac fp < 0"
        else if s.fp = 0
        then B.empty
        else B.select s.s (s.fp - 1) 0
      ;;

      let signal s = s.s
      let width_int s = B.width (int s)
      let width_frac s = B.width (frac s)

      let to_float s =
        let fp = 2. ** Float.of_int s.fp in
        let s = B.sresize s.s Nativeint.num_bits in
        let i = Float.of_int (B.to_int s) in
        i /. fp
      ;;

      let extend s n =
        if n < 0
        then failwith "Fixed.Signed.extend"
        else if n = 0
        then s
        else { s = B.concat_msb [ B.repeat (B.msb s.s) n; s.s ]; fp = s.fp }
      ;;

      let select_int s i =
        if i <= 0
        then failwith "Fixed.Signed.select_int i<=0"
        else (
          let si = int s in
          let wi = width_int s in
          if i <= wi
          then B.select si (i - 1) 0
          else B.concat_msb [ B.repeat (B.msb si) (i - wi); si ])
      ;;

      let select_frac s f =
        if f < 0
        then failwith "Fixed.Signed.select_frac f<0"
        else if f = 0
        then B.empty
        else (
          let wf = width_frac s in
          if wf = 0
          then B.zero f
          else (
            let sf = frac s in
            if f <= wf
            then B.select sf (wf - 1) (wf - f)
            else B.concat_msb [ sf; B.zero (f - wf) ]))
      ;;

      let select s i f =
        let i' = select_int s i in
        let f' = select_frac s f in
        mk f (B.concat_msb_e [ i'; f' ])
      ;;

      let norm l =
        let i = List.fold l ~init:0 ~f:(fun a b -> max a (B.width (int b))) in
        let f = List.fold l ~init:0 ~f:(fun a b -> max a (B.width (frac b))) in
        List.map l ~f:(fun s -> select s i f)
      ;;

      let norm2 a b =
        let l = norm [ a; b ] in
        match l with
        | [ a; b ] -> a, b
        | _ -> failwith "Fixed.Signed.norm2"
      ;;

      let const ip fp f =
        let fp' = Float.of_int fp in
        let fp' = 2.0 ** fp' in
        mk fp (B.of_int ~width:(ip + fp) (Int.of_float (f *. fp')))
      ;;

      (* basic arithmetic *)

      let ( +: ) a b =
        let a, b = norm2 a b in
        let a, b = extend a 1, extend b 1 in
        { s = B.( +: ) a.s b.s; fp = a.fp }
      ;;

      let ( -: ) a b =
        let a, b = norm2 a b in
        let a, b = extend a 1, extend b 1 in
        { s = B.( -: ) a.s b.s; fp = a.fp }
      ;;

      let ( *: ) a b = { s = B.( *+ ) a.s b.s; fp = a.fp + b.fp }

      (* comparison *)
      let ( ==: ) a b =
        let a, b = norm2 a b in
        B.( ==: ) a.s b.s
      ;;

      let ( <>: ) a b =
        let a, b = norm2 a b in
        B.( <>: ) a.s b.s
      ;;

      let ( <: ) a b =
        let a, b = norm2 a b in
        B.( <+ ) a.s b.s
      ;;

      let ( <=: ) a b =
        let a, b = norm2 a b in
        B.( <=+ ) a.s b.s
      ;;

      let ( >: ) a b =
        let a, b = norm2 a b in
        B.( >+ ) a.s b.s
      ;;

      let ( >=: ) a b =
        let a, b = norm2 a b in
        B.( >=+ ) a.s b.s
      ;;

      (* mux *)
      let mux sel l =
        let l = norm l in
        let fp = width_frac (List.hd_exn l) in
        let q = B.mux sel (List.map l ~f:signal) in
        mk fp q
      ;;

      (* resize with rounding and saturation control *)
      let resize s i f =
        let i' = width_int s in
        let f' = width_frac s in
        (* perform rounding *)
        let s = if f >= f' then select s i' f else mk f (S.round (f' - f) s.s) in
        (* perform overflow control *)
        mk f (S.overflow f i s.s)
      ;;
    end
  end
end
