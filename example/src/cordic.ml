open! Import

include Cordic_intf

(* (1/2) * log2 ((1+t) / (1-t)) *)
let atanh t =
  0.5 *. (Float.log ((1.0 +. t) /. (1.0 -. t)))

module System = struct
  include Cordic_reference.System

  let const = function
    | Circular   -> "00"
    | Hyperbolic -> "01"
    | Linear     -> "10"

  let to_signal t = Signal.const (const t)
end

module Mode = struct
  include Cordic_reference.Mode

  let const = function
    | Rotation  -> "00"
    | Vectoring -> "01"
    | Inverse   -> "11"

  let to_signal t = Signal.const (const t)
end

module Make (Fixnum_spec : Fixnum.Spec) = struct

  module Architecture = Architecture

  module Fixnum = Fixnum.Make (Fixnum_spec)

  module Make_unrolled (B : Comb.S) = struct

    let iter = Cordic_reference.iter

    let atan iterations =
      Array.init iterations ~f:(fun i -> Fixnum.of_float Float.(atan (2. **. - of_int i)))

    let atanh iterations =
      iter ~iterations ~init:[]
        ~f:(fun ~i:_ ~ih l -> Fixnum.of_float Float.(atanh (2. **. - of_int ih)) :: l)
      |> Array.of_list_rev

    let t iterations =
      Array.init iterations ~f:(fun i -> Fixnum.pow2 (- i))

    let step ~x ~xsft ~y ~ysft ~z ~d ~m ~e =
      let open B in
      let x' = mux2 (msb m) x (mux2 ((lsb m) ^: d) (x +: ysft) (x -: ysft)) in
      let y' = mux2 d (y -: xsft) (y +: xsft) in
      let z' = mux2 d (z +: e) (z -: e) in
      x', y', z'

    let cordic ?(pipe=Fn.id) ~system ~mode ~iterations ~c ~x ~y ~z =
      let open B in
      assert (width x = width y);
      assert (width x = width z);
      let m = system in
      let e = [ atan iterations; atanh iterations; t iterations ] in
      let is_hyper = system ==: B.constb (System.const Hyperbolic) in
      iter ~iterations ~init:(x, y, z)
        ~f:(fun ~i ~ih (x,y,z) ->
          let e =
            mux system (List.map e ~f:(fun e ->
              B.constb (e.(i) |> Fixnum.constb))) in
          let d = mux mode [ z <+. 0; y >=+. 0; y >=+ c ] in
          let xsft = mux2 is_hyper (sra x ih) (sra x i) in
          let ysft = mux2 is_hyper (sra y ih) (sra y i) in
          let x, y, z = step ~x ~xsft ~y ~ysft ~z ~d ~m ~e:e in
          (pipe x), (pipe y), (pipe z))
  end

  module Iterative = struct

    module S = Signal
    (*module S = Const_prop.Comb*)
    module C = Make_unrolled (S)
    open S

    let hyper_iter ~reg_spec ~enable ~ld ~system ~iter =
      let is_hyper = system ==: Signal.constb (System.const Hyperbolic) in
      let iterh = wire (width iter) -- "iterh" in
      let k = wire (width iter + 2) -- "k" in
      let repeat = (k ==: ((zero 2) @: iterh)) -- "repeated_step" in
      let upd v ~init t f =
        v <== reg reg_spec ~e:enable
                (mux2 ld
                   (consti (width t) init)
                   (mux2 repeat t f))
      in
      upd k     ~init:4 (k +: (sll k 1) +:. 1) k;
      upd iterh ~init:1 iterh                  (iterh +:. 1);
      (mux2 is_hyper iterh iter) -- "iter_sel"

    let cordic ~reg_spec ~enable ~ld ~system ~mode ~iterations ~c ~x ~y ~z =
      let wi = num_bits (iterations-1) in
      let iter =
        reg_fb reg_spec ~e:enable ~w:wi (fun d -> mux2 ld (zero wi) (d +:. 1)) -- "iter" in
      let table_lookup table =
        mux iter (Array.to_list
                    (Array.map table ~f:(fun c ->
                       S.constb (c |> Fixnum.constb)))) in
      let atan  = table_lookup (C.atan  iterations) in
      let atanh = table_lookup (C.atanh iterations) in
      let t     = table_lookup (C.t     iterations) in
      let xw, yw, zw = wire Fixnum.width, wire Fixnum.width, wire Fixnum.width in
      let m = system in
      let e = mux system [ atan; atanh; t ] -- "e" in
      let d = mux mode [ zw <+. 0; yw >=+. 0; yw >=+ c ] in
      let iter = hyper_iter ~reg_spec ~enable ~ld ~system ~iter in
      let xsft = log_shift sra xw iter in
      let ysft = log_shift sra yw iter in
      let xs, ys, zs = C.step ~x:xw ~xsft ~y:yw ~ysft ~z:zw ~d ~m ~e in
      xw <== reg reg_spec ~e:enable (mux2 ld x xs);
      yw <== reg reg_spec ~e:enable (mux2 ld y ys);
      zw <== reg reg_spec ~e:enable (mux2 ld z zs);
      xw, yw, zw
  end

  module Unrolled =
    Make_unrolled
      (struct
        type t = Signal.t
        include Signal.Const_prop.Comb
      end)

  module I = struct
    type 'a t =
      { clk    : 'a
      ; clr    : 'a
      ; enable : 'a [@bits 1]
      ; ld     : 'a [@bits 1] (* load [x], [y], [z], and initialize state *)
      ; system : 'a [@bits 2]
      ; mode   : 'a [@bits 2]
      ; c      : 'a [@bits Fixnum.width]
      ; x      : 'a [@bits Fixnum.width]
      ; y      : 'a [@bits Fixnum.width]
      ; z      : 'a [@bits Fixnum.width] }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t =
      { xo : 'a [@bits Fixnum.width]
      ; yo : 'a [@bits Fixnum.width]
      ; zo : 'a [@bits Fixnum.width] }
    [@@deriving sexp_of, hardcaml]
  end

  let create (config : Config.t) (i : Signal.t I.t) =
    let reg_spec = Reg_spec.create () ~clk:i.clk ~clr:i.clr in
    let iterations = config.iterations in
    let xo, yo, zo =
      let system, mode = i.system, i.mode in
      let x, y, z, c = i.x, i.y, i.z, i.c in
      match config.architecture with
      | Combinational ->
        Unrolled.cordic ?pipe:None ~system ~mode ~iterations ~c ~x ~y ~z
      | Pipelined ->
        Unrolled.cordic ~pipe:(Signal.reg reg_spec ~e:i.enable)
          ~system ~mode ~iterations ~c ~x ~y ~z
      | Iterative ->
        Iterative.cordic ~reg_spec ~enable:i.enable ~ld:i.ld ~system ~mode ~iterations ~c ~x ~y ~z
    in
    { O. xo; yo; zo }
end
