open! Import

(* twos-complement normalization -- returns the narrowest [Bits.t] value that represents
   the same integer as the input.  *)
let rec narrow x =
  let open Bits in
  if width x = 1
  then x
  else
    let m, l = msb x, lsbs x in
    if Bits.equal m (msb l)
    then narrow l
    else x

(* [make_rom coefs] returns a list of values of length [2^n], where [n] is [length coefs],
   the value in list element [i] is the sum of the values in [coefs] at the indices of the
   one bits in the representation of [i].  That is, the result holds all possible sums of
   subsets of [coefs]. *)
let make_rom coefs =
  let open Bits in
  let n_coefs = List.length coefs in
  let (+:) a b = Signed.(to_signal (of_signal a +: of_signal b)) in
  let last = ones n_coefs in
  let rec rom i =
    let sum =
      reduce (+:)
        (List.map2_exn
           (List.rev (bits i))
           coefs
           ~f:(fun i c -> mux2 i c (zero (width c)))) in
    sum :: (if Bits.equal i last
            then []
            else rom (i +:. 1)) in
  let rom = List.map (rom (zero n_coefs)) ~f:narrow in
  let max_width = List.fold (List.map rom ~f:width) ~init:0 ~f:max in
  List.map rom ~f:(fun s -> sresize s max_width)

open Signal

module Mode = struct
  type t =
    | Fixed
    | Integer
  [@@deriving sexp_of]
end

module type Config = sig
  val mode             : Mode.t
  val accumulator_bits : int
  val data_bits        : int
  val num_coefs        : int
  val rom_shift        : int
end

module Make (Config : Config) = struct
  open Config

  module I = struct
    type 'a t =
      { clk    : 'a[@bits 1]
      ; clr    : 'a[@bits 1]
      ; en     : 'a[@bits 1]
      ; ld     : 'a[@bits 1]
      ; addsub : 'a[@bits 1]
      ; x      : 'a array[@length num_coefs][@bits data_bits];
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t =
      { q : 'a[@bits accumulator_bits] }
    [@@deriving sexp_of, hardcaml]
  end

  let rac reg_spec ~en ~ld ~addsub ~romcoefs ~x =
    (* parallel in, serial out register *)
    let piso xi =
      match mode with
      | Fixed   -> lsb (reg_fb reg_spec ~e:en ~w:data_bits (fun d -> mux2 ld xi (srl d 1)))
      | Integer -> msb (reg_fb reg_spec ~e:en ~w:data_bits (fun d -> mux2 ld xi (sll d 1)))
    in
    (* rom address *)
    let addr = concat (List.rev (List.map x ~f:piso)) -- "piso_addr" in
    (* build and index rom *)
    let coef = mux addr romcoefs -- "rom_coef" in
    (* accumulator *)
    reg_fb reg_spec ~e:en ~w:accumulator_bits
      (fun acc ->
         let acc = (match mode with Fixed -> sra | Integer -> sll) acc 1 in
         let coef = sll (sresize coef accumulator_bits) rom_shift in
         mux2 ld
           (zero accumulator_bits)
           (mux2 addsub (acc -: coef) (acc +: coef)))

  let create ~coefs (i : Signal.t I.t) =
    let romcoefs = make_rom (Array.to_list coefs) in
    let q =
      rac
        (Reg_spec.create () ~clk:i.clk ~clr:i.clr)
        ~en:i.en
        ~ld:i.ld
        ~addsub:i.addsub
        ~romcoefs:(List.map romcoefs ~f:(fun coef -> Signal.constb (Bits.to_bstr coef)))
        ~x:(Array.to_list i.x)
    in
    { O.q }

end
