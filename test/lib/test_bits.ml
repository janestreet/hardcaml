(* Compare 2 different combinational API implementations by running random tests on the
   following operations;

   - add, sub, mulu, muls, and, or, xor, not, eq, lt, select, mux, concat

   These are the core operations from which the full [Comb.S] API is built.

   This is in preparation for testing either a Zarith based version, or a version built on
   [int array].  The end goal will be to choose the fastest and delete all the rest. *)

open! Import
open! Bits

include Test_bits_intf

let%expect_test "[compare]" =
  List.iter
    [ "0", "0"
    ; "0", "1"
    ; "1", "0"
    ; "0", "00"
    ; "00", "01"
    ; "11", "01" ]
    ~f:(fun (s1, s2) ->
      print_s [%message "compare" ~_:(s1 : string) ~_:(s2 : string)
                          "=" ~_:(compare (constb s1) (constb s2) : int)]);
  [%expect {|
    (compare 0 0 = 0)
    (compare 0 1 = -1)
    (compare 1 0 = 1)
    (compare 0 00 = -1)
    (compare 00 01 = -1)
    (compare 11 01 = 1) |}]
;;

let%expect_test "set of [Bits.t]s" =
  print_s [%sexp (Set.of_list (module Bits)
                    (List.map [ "0"; "1"; "01"; "11" ] ~f:constb)
                  : Set.M(Bits).t)];
  [%expect {|
    (0 1 01 11) |}];
;;

let%expect_test "[floor_log2], [popcount]" =
  let or_error_of_with_valid (t : Bits.t With_valid.t) =
    if Bits.to_int t.valid = 1
    then Ok (Bits.to_int t.value)
    else error_s [%message "With_valid.valid = 0"]
  in
  List.iter [ "0"; "1"
            ; "00"; "01"; "10"; "11"
            ; String.make 62 '1'
            ; "1" ^ String.make 62 '0'
            ; String.make 63 '1'
            ] ~f:(fun s ->
    let t = s |> Bits.const in
    print_s [%message
      ""
        ~input:(s : string)
        ~popcount:(Bits.popcount t |> Bits.to_int : int)
        ~floor_log2:(Bits.floor_log2 t |> or_error_of_with_valid : int Or_error.t)]);
  [%expect {|
    ((input    0)
     (popcount 0)
     (floor_log2 (Error "With_valid.valid = 0")))
    ((input    1)
     (popcount 1)
     (floor_log2 (Ok 0)))
    ((input    00)
     (popcount 0)
     (floor_log2 (Error "With_valid.valid = 0")))
    ((input    01)
     (popcount 1)
     (floor_log2 (Ok 0)))
    ((input    10)
     (popcount 1)
     (floor_log2 (Ok 1)))
    ((input    11)
     (popcount 2)
     (floor_log2 (Ok 1)))
    ((input 11111111111111111111111111111111111111111111111111111111111111)
     (popcount 62)
     (floor_log2 (Ok 61)))
    ((input 100000000000000000000000000000000000000000000000000000000000000)
     (popcount 1)
     (floor_log2 (Ok 62)))
    ((input 111111111111111111111111111111111111111111111111111111111111111)
     (popcount 63)
     (floor_log2 (Ok 62))) |}]
;;

let test_sexp_of_constb (module M : Hardcaml.Comb.S) =
  List.iter
    [ "0"
    ; "1"
    ; "01"
    ; "10"
    ; String.make 31 '1'
    ; String.make 32 '1'
    ; String.make 33 '1'
    ; String.make 63 '1'
    ; String.make 64 '1'
    ; String.make 65 '1' ]
    ~f:(fun s ->
      print_s [%message
        ""
          ~_:(s : string)
          ~_:(Or_error.try_with (fun () -> M.constb s) : M.t Or_error.t)])
;;

let%expect_test "[IntbitsList.constb]" =
  test_sexp_of_constb (module IntbitsList);
  [%expect {|
    (0 (Ok 0))
    (1 (Ok 1))
    (01 (Ok 01))
    (10 (Ok 10))
    (1111111111111111111111111111111 (Ok 1111111111111111111111111111111))
    (11111111111111111111111111111111 (Ok 11111111111111111111111111111111))
    (111111111111111111111111111111111 (Ok 111111111111111111111111111111111))
    (111111111111111111111111111111111111111111111111111111111111111
     (Ok 111111111111111111111111111111111111111111111111111111111111111))
    (1111111111111111111111111111111111111111111111111111111111111111
     (Ok 1111111111111111111111111111111111111111111111111111111111111111))
    (11111111111111111111111111111111111111111111111111111111111111111
     (Ok 11111111111111111111111111111111111111111111111111111111111111111)) |}];
;;

let%expect_test "[Bits.constb]" =
  test_sexp_of_constb (module Bits);
  [%expect {|
    (0 (Ok 0))
    (1 (Ok 1))
    (01 (Ok 01))
    (10 (Ok 10))
    (1111111111111111111111111111111 (Ok 1111111111111111111111111111111))
    (11111111111111111111111111111111 (Ok 11111111111111111111111111111111))
    (111111111111111111111111111111111 (Ok 111111111111111111111111111111111))
    (111111111111111111111111111111111111111111111111111111111111111
     (Ok 111111111111111111111111111111111111111111111111111111111111111))
    (1111111111111111111111111111111111111111111111111111111111111111
     (Ok 1111111111111111111111111111111111111111111111111111111111111111))
    (11111111111111111111111111111111111111111111111111111111111111111
     (Ok 11111111111111111111111111111111111111111111111111111111111111111)) |}];
;;

module Primitive_op = struct
  type t =
    | Add
    | Sub
    | Mulu
    | Muls
    | And
    | Or
    | Xor
    | Not
    | Eq
    | Lt
    | Sel
    | Mux
    | Cat
  [@@deriving enumerate, sexp_of]

  let name t = [%sexp (t : t)] |> Sexp.to_string |> String.lowercase

  let arg_type = Command.Arg_type.of_alist_exn (List.map all ~f:(fun t -> (name t, t)))
end

module Bits_module = struct

  type t =
    | IntbitsList
    | Bits
    | Mutable_ArraybitsInt
    | BadPrimitives
  [@@deriving enumerate]

  type bits_module =
    { name       : string
    ; short_name : string
    ; module_    : (module Comb.S) sexp_opaque }
  [@@deriving sexp_of]

  let v_IntbitsList =
    { name       = "IntbitsList"
    ; short_name = "intbits"
    ; module_    = (module IntbitsList) }
  let v_Bits =
    { name       = "Bits"
    ; short_name = "bits"
    ; module_    = (module Bits) }
  let v_Mutable_ArraybitsInt =
    { name       = "Mutable.ArraybitsInt"
    ; short_name = "raw-int"
    ; module_    = (module Bits.Mutable.Comb) }

  let v_BadPrimitives =
    let module BadPrimitives = struct
      include Bits
      let (+:) a b = ~: (a +: b)
      let ( *: ) a b = ~: (a *: b)
      let concat d = ~: (concat d)
      let mux s d = ~: (mux s d)
      let select d h l = ~: (select d h l)
    end in
    { name       = "BadPrimitives"
    ; short_name = "bad"
    ; module_    = (module BadPrimitives) }

  let select = function
    | IntbitsList      -> v_IntbitsList
    | Bits             -> v_Bits
    | Mutable_ArraybitsInt -> v_Mutable_ArraybitsInt
    | BadPrimitives    -> v_BadPrimitives

  let module_    t = (select t).module_
  let name       t = (select t).name
  let short_name t = (select t).short_name

  let sexp_of_t t = sexp_of_bits_module (select t)

  let arg_type =
    Command.Arg_type.of_alist_exn (List.map all ~f:(fun t -> (short_name t, t)))
end

module Config = struct
  type t =
    { bits1         : Bits_module.t
    ; bits2         : Bits_module.t
    ; prims         : Primitive_op.t list
    ; iterations    : int
    ; min_bit_width : int
    ; max_bit_width : int }
  [@@deriving sexp_of]
end

module type Test = Test_ with type config := Config.t

module Expect_test_require = struct
  let require here eq ~error_message =
    require here eq ~if_false_then_print_s:error_message
end

module Expect_test_require_nocr = struct
  let require here eq ~error_message =
    require here eq ~cr:CR.Comment ~if_false_then_print_s:error_message
end

module Make (R : Require) = struct

  module Ops (B1 : Comb.S) (B2 : Comb.S) = struct

    let brand min max = min + Random.int (max-min+1)
    let srand b = Bits.(random ~width:b |> to_string)
    let const1 s = B1.const s
    let const2 s = B2.const s

    let bits_equal x y = String.equal (B1.to_bstr x) (B2.to_bstr y)

    let require ~error_message here eq =
      R.require ~error_message here eq;
      eq

    let mismatch operation result1 result2 =
      [%message "mismatch" (operation : Sexp.t) (result1 : B1.t) (result2 : B2.t)]

    let require_bits_equal here result1 result2 ~operation =
      require here (bits_equal result1 result2)
        ~error_message:(lazy (mismatch (force operation) result1 result2))

    (* test binary operators that require both operands to have the same width *)
    let op2 here op op1 op2 min_bit_width max_bit_width =
      let bits = brand min_bit_width max_bit_width in
      let a = srand bits in
      let b = srand bits in
      let result1 = op1 (const1 a) (const1 b) in
      let result2 = op2 (const2 a) (const2 b) in
      require_bits_equal here result1 result2 ~operation:(lazy [%message a op b])

    let add  here = op2 here "+:"  B1.(+: ) B2.(+: )
    let sub  here = op2 here "-:"  B1.(-: ) B2.(-: )
    let and_ here = op2 here "&:"  B1.(&: ) B2.(&: )
    let or_  here = op2 here "|:"  B1.(|: ) B2.(|: )
    let xor  here = op2 here "^:"  B1.(^: ) B2.(^: )
    let eq   here = op2 here "==:" B1.(==:) B2.(==:)
    let lt   here = op2 here "<:"  B1.(<: ) B2.(<: )

    (* test the multiplication operator, for which the operands may be different widths *)
    let opm here op op1 op2 min_bit_width max_bit_width =
      let bits1 = brand min_bit_width max_bit_width in
      let bits2 = brand min_bit_width max_bit_width in
      let a = srand bits1 in
      let b = srand bits2 in
      require_bits_equal here
        (op1 (const1 a) (const1 b))
        (op2 (const2 a) (const2 b))
        ~operation:(lazy [%message a op b])

    let mulu here = opm here "*:" B1.( *: ) B2.( *: )
    let muls here = opm here "*+" B1.( *+ ) B2.( *+ )

    (* test the not operator *)
    let not_  here min_bit_width max_bit_width =
      let bits = brand min_bit_width max_bit_width in
      let arg = srand bits in
      require_bits_equal here
        (B1.(~:) (const1 arg))
        (B2.(~:) (const2 arg))
        ~operation:(lazy [%message "~:" arg])

    (* test selection *)
    let sel here min_bit_width max_bit_width =
      let bits = brand min_bit_width max_bit_width in
      let b1 = Random.int bits in
      let b2 = Random.int bits in
      let high = max b1 b2 in
      let low  = min b1 b2 in
      let arg = srand bits in
      require_bits_equal here
        (B1.select (const1 arg) high low)
        (B2.select (const2 arg) high low)
        ~operation:(lazy [%message "select" arg ~_:(high : int) ~_:(low : int)])

    (* test multiplexing, with a data max size of 256 elements *)
    let mux here min_bit_width max_bit_width =
      let sel_bits = brand 1 8 in (* limit size of mux *)
      let data_bits = brand min_bit_width max_bit_width in
      let data_size =
        let n = 1 lsl (sel_bits-1) in
        max 2 (n + Random.int (n+1))
      in
      let sel = srand sel_bits in
      let data = Array.to_list @@ Array.init data_size ~f:(fun _ -> srand data_bits) in
      require_bits_equal here
        (B1.mux (const1 sel) (List.map data ~f:const1))
        (B2.mux (const2 sel) (List.map data ~f:const2))
        ~operation:(lazy [%message "mux" sel ~_:(data : string list)])

    (* test concatenation *)
    let cat here min_bit_width max_bit_width =
      let concat1 d = B1.concat @@ List.map d ~f:const1 in
      let concat2 d = B2.concat @@ List.map d ~f:const2 in
      let cnt = 1 + Random.int 8 in
      let args =
        Array.to_list
        @@ Array.init cnt ~f:(fun _ ->
          srand
          @@ brand min_bit_width max_bit_width) in
      require_bits_equal here
        (concat1 args)
        (concat2 args)
        ~operation:(lazy [%message "concat" ~_:(args : string list)])

    let run (op : Primitive_op.t) =
      match op with
      | Add  -> add
      | Sub  -> sub
      | Mulu -> mulu
      | Muls -> muls
      | And  -> and_
      | Or   -> or_
      | Xor  -> xor
      | Not  -> not_
      | Eq   -> eq
      | Lt   -> lt
      | Sel  -> sel
      | Mux  -> mux
      | Cat  -> cat

  end

  (* run each operation test until the first error. *)
  let test ?(stop_on_first_primitive_error = true) here (config : Config.t) =
    let module Bits1 = (val (Bits_module.module_ config.bits1)) in
    let module Bits2 = (val (Bits_module.module_ config.bits2)) in
    let module T = Ops (Bits1) (Bits2) in
    print_s [%message (config : Config.t)];
    List.iter config.prims ~f:(fun op ->
      let rec loop i =
        if i < config.iterations
        && (T.run op here config.min_bit_width config.max_bit_width
            || not stop_on_first_primitive_error)
        then loop (i+1)
      in
      loop 0)
end

(* test the tests *)
let%expect_test "BadPrimitives" =
  let module Test = Make (Expect_test_require_nocr) in
  let config =
    { Config.
      bits1         = Bits
    ; bits2         = BadPrimitives
    ; prims         = Primitive_op.all
    ; iterations    = 1
    ; min_bit_width = 1
    ; max_bit_width = 20 };
  in
  Test.test [%here] config;
  [%expect {|
    (config (
      (bits1 (
        (name       Bits)
        (short_name bits)
        (module_    <opaque>)))
      (bits2 (
        (name       BadPrimitives)
        (short_name bad)
        (module_    <opaque>)))
      (prims (Add Sub Mulu Muls And Or Xor Not Eq Lt Sel Mux Cat))
      (iterations    1)
      (min_bit_width 1)
      (max_bit_width 20)))
    (* require-failed: lib/hardcaml/test/lib/test_bits.ml:LINE:COL. *)
    (mismatch
      (operation (110111000101100 +: 001000001100100))
      (result1 111111010010000)
      (result2 000000101101111))
    (* require-failed: lib/hardcaml/test/lib/test_bits.ml:LINE:COL. *)
    (mismatch
      (operation (101 *: 0000))
      (result1 0000000)
      (result2 1111111))
    (* require-failed: lib/hardcaml/test/lib/test_bits.ml:LINE:COL. *)
    (mismatch
      (operation (select 0111100110000011 10 3))
      (result1 00110000)
      (result2 11001111))
    (* require-failed: lib/hardcaml/test/lib/test_bits.ml:LINE:COL. *)
    (mismatch
      (operation (mux 0 (0011 0000)))
      (result1 0011)
      (result2 1100))
    (* require-failed: lib/hardcaml/test/lib/test_bits.ml:LINE:COL. *)
    (mismatch
      (operation (
        concat (1100101111 00001100011100 11101011010 00 01101101011111101000)))
      (result1 110010111100001100011100111010110100001101101011111101000)
      (result2 001101000011110011100011000101001011110010010100000010111)) |}]

module Test = Make (Expect_test_require)

(* Various files named [test_bits_int64_*.ml] compare int64 against all the other
   implementations.  Using separate files allows all those tests to run in parallel. *)
