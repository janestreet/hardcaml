(* Compare 2 different combinational API implementations by running random tests on the
   following operations;

   - add, sub, mulu, muls, and, or, xor, not, eq, lt, select, mux, concat

   These are the core operations from which the full [Comb.S] API is built.

   This is in preparation for testing either a Zarith based version, or a version built on
   [int array]. The end goal will be to choose the fastest and delete all the rest. *)

open! Import
open! Bits
include Test_bits_intf

let%expect_test "[compare]" =
  let compare_and_print cases =
    List.iter cases ~f:(fun (s1, s2) ->
      print_s
        [%message
          "compare"
            ~_:(s1 : string)
            ~_:(s2 : string)
            "="
            ~_:(compare (of_bit_string s1) (of_bit_string s2) : int)])
  in
  compare_and_print [ "0", "0"; "0", "1"; "1", "0"; "0", "00"; "00", "01"; "11", "01" ];
  [%expect
    {|
    (compare 0 0 = 0)
    (compare 0 1 = -1)
    (compare 1 0 = 1)
    (compare 0 00 = -1)
    (compare 00 01 = -1)
    (compare 11 01 = 1)
    |}];
  (* different widths *)
  compare_and_print [ "11", "000"; "001", "10" ];
  [%expect
    {|
    (compare 11 000 = -1)
    (compare 001 10 = 1)
    |}];
  (* Random value comparisons at same width *)
  let bits_compare a b =
    (* reference comparison implementation when a and b are the same widths. *)
    if to_bool (a ==: b) then 0 else if to_bool (a <: b) then -1 else 1
  in
  for _ = 1 to 10_000 do
    let width = 1 + Random.int 200 in
    let a = Bits.random ~width in
    let b = Bits.random ~width in
    let expected = bits_compare a b in
    let got = Bits.compare a b in
    if expected <> got
    then
      raise_s
        [%message
          "unsigned comparison mismatch" (a : t) (b : t) (expected : int) (got : int)]
  done;
  (* differently sized equal values *)
  for _ = 1 to 100 do
    let width = 1 + Random.int 200 in
    let a = Bits.random ~width in
    let got = Bits.compare a a in
    if got <> 0 then raise_s [%message "unsigned comparison mismatch" (a : t)]
  done
;;

let%expect_test "Bits are compared by size first, and then value" =
  let x_str = "10000001" in
  let x = Bits.of_string x_str in
  let y = Bits.of_string ("0" ^ x_str) in
  print_s [%message (Bits.compare x y : int)];
  [%expect {| ("Bits.compare x y" -1) |}]
;;

let%expect_test "set of [Bits.t]s" =
  print_s
    [%sexp
      (Set.of_list (module Bits) (List.map [ "0"; "1"; "01"; "11" ] ~f:of_bit_string)
       : Set.M(Bits).t)];
  [%expect {| (0 1 01 11) |}]
;;

let%expect_test "[floor_log2], [popcount]" =
  let or_error_of_with_valid (t : Bits.t With_valid.t) =
    if Bits.to_bool t.valid
    then Ok (Bits.to_int_trunc t.value)
    else error_s [%message "With_valid.valid = 0"]
  in
  List.iter
    [ "0"
    ; "1"
    ; "00"
    ; "01"
    ; "10"
    ; "11"
    ; String.make 62 '1'
    ; "1" ^ String.make 62 '0'
    ; String.make 63 '1'
    ]
    ~f:(fun s ->
      let t = s |> Bits.of_string in
      print_s
        [%message
          ""
            ~input:(s : string)
            ~popcount:(Bits.popcount t |> Bits.to_int_trunc : int)
            ~floor_log2:(Bits.floor_log2 t |> or_error_of_with_valid : int Or_error.t)]);
  [%expect
    {|
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
     (floor_log2 (Ok 62)))
    |}]
;;

let%expect_test "[byte_qualifier]" =
  let e num_bytes how_many_bytes_set =
    Bits.binary_to_byte_qualifier
      ~num_bytes
      (Bits.of_unsigned_int ~width:(num_bits_to_represent num_bytes) how_many_bytes_set)
  in
  (* Self tests *)
  Sequence.range 1 8
  |> Sequence.iter ~f:(fun num_bytes ->
    Sequence.range ~stop:`inclusive 0 num_bytes
    |> Sequence.iter ~f:(fun how_many_bytes_set ->
      let encoded = e num_bytes how_many_bytes_set in
      assert (Bits.to_unsigned_int (popcount encoded) = how_many_bytes_set);
      assert (Bits.to_unsigned_int (trailing_ones encoded) = how_many_bytes_set)));
  (* Usage demonstration *)
  let p num_bytes test_cases =
    List.map test_cases ~f:(fun input ->
      let result = e num_bytes input in
      [%message (input : int) (result : Bits.t)])
    |> Expectable.print
  in
  p 4 [ 0; 2; 4 ];
  [%expect
    {|
    ┌───────┬────────┐
    │ input │ result │
    ├───────┼────────┤
    │ 0     │ 0000   │
    │ 2     │ 0011   │
    │ 4     │ 1111   │
    └───────┴────────┘
    |}];
  p 6 [ 0; 3; 6 ];
  [%expect
    {|
    ┌───────┬────────┐
    │ input │ result │
    ├───────┼────────┤
    │ 0     │ 000000 │
    │ 3     │ 000111 │
    │ 6     │ 111111 │
    └───────┴────────┘
    |}];
  p 8 [ 0; 4; 8 ];
  [%expect
    {|
    ┌───────┬──────────┐
    │ input │ result   │
    ├───────┼──────────┤
    │ 0     │ 00000000 │
    │ 4     │ 00001111 │
    │ 8     │ 11111111 │
    └───────┴──────────┘
    |}]
;;

let test_sexp_of_bit_string (module M : Hardcaml.Comb.S) =
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
    ; String.make 65 '1'
    ]
    ~f:(fun s ->
      print_s
        [%message
          ""
            ~_:(s : string)
            ~_:(Or_error.try_with (fun () -> M.of_bit_string s) : M.t Or_error.t)])
;;

let%expect_test "[Constant] sexp_of_t roundtrips" =
  let const = Constant.of_binary_string "10010101" in
  let sexp = Constant.sexp_of_t const in
  let const' = Constant.t_of_sexp sexp in
  [%test_result: Constant.t] const' ~expect:const
;;

let%expect_test "[Bits_list.of_bit_string]" =
  test_sexp_of_bit_string (module Bits_list.Int_comb);
  [%expect
    {|
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
     (Ok 11111111111111111111111111111111111111111111111111111111111111111))
    |}]
;;

let%expect_test "[Bits.of_bit_string]" =
  test_sexp_of_bit_string (module Bits);
  [%expect
    {|
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
     (Ok 11111111111111111111111111111111111111111111111111111111111111111))
    |}]
;;

module%test Test_for_collection = struct
  open Core

  let gen_bits =
    let open Quickcheck.Generator.Let_syntax in
    let%bind width = Int.gen_incl 1 20 in
    let%bind data = Int.gen_incl 0 ((1 lsl width) - 1) in
    return (Bits.of_unsigned_int ~width data)
  ;;

  let gen_bits_list =
    let open Quickcheck.Generator.Let_syntax in
    let%bind cnt = Int.gen_incl 1 8 in
    List.gen_with_length cnt gen_bits
  ;;

  let gen_bits_and_part_width =
    let open Quickcheck.Generator.Let_syntax in
    let%bind width = Int.gen_incl 1 40 in
    let%bind data = Int.gen_incl 0 ((1 lsl min width 30) - 1) in
    let%bind part_width = Int.gen_incl 1 width in
    return (Bits.of_unsigned_int ~width data, part_width)
  ;;

  (* Test mux, mux_strict *)
  let gen_mux_inputs =
    let open Core.Quickcheck.Generator.Let_syntax in
    let%bind sel_width = Int.gen_incl 1 4 in
    let num_cases = 1 lsl sel_width in
    let%bind sel_val = Int.gen_incl 0 (num_cases - 1) in
    let%bind data_width = Int.gen_incl 1 16 in
    let%bind cases =
      List.gen_with_length num_cases (Int.gen_incl 0 ((1 lsl data_width) - 1))
    in
    return
      ( Bits.of_unsigned_int ~width:sel_width sel_val
      , List.map cases ~f:(fun v -> Bits.of_unsigned_int ~width:data_width v) )
  ;;

  module type For_collection = sig
    include Bits.For_collection

    val of_list : Bits.t list -> Bits.t collection
    val to_list : Bits.t collection -> Bits.t list
  end

  let test (module For_collection : For_collection) =
    Quickcheck.test gen_bits_list ~f:(fun values_list ->
      let values_array = For_collection.of_list values_list in
      let list_msb = Bits.concat_msb values_list in
      [%test_result: Bits.t] (For_collection.concat_msb values_array) ~expect:list_msb;
      let list_lsb = Bits.concat_lsb values_list in
      [%test_result: Bits.t] (For_collection.concat_lsb values_array) ~expect:list_lsb);
    Quickcheck.test gen_bits ~f:(fun bits ->
      let list_msb = Bits.bits_msb bits in
      let list_lsb = Bits.bits_lsb bits in
      [%test_result: Bits.t list]
        (For_collection.to_list (For_collection.bits_msb bits))
        ~expect:list_msb;
      [%test_result: Bits.t list]
        (For_collection.to_list (For_collection.bits_lsb bits))
        ~expect:list_lsb);
    Quickcheck.test gen_bits_and_part_width ~f:(fun (bits, part_width) ->
      let list_lsb = Bits.split_lsb ~exact:false ~part_width bits in
      let list_msb = Bits.split_msb ~exact:false ~part_width bits in
      [%test_result: Bits.t list]
        (For_collection.to_list (For_collection.split_lsb ~exact:false ~part_width bits))
        ~expect:list_lsb;
      [%test_result: Bits.t list]
        (For_collection.to_list (For_collection.split_msb ~exact:false ~part_width bits))
        ~expect:list_msb);
    Quickcheck.test gen_mux_inputs ~f:(fun (sel, cases_list) ->
      let cases_array = For_collection.of_list cases_list in
      let list_mux = Bits.mux sel cases_list in
      let list_mux_strict = Bits.mux_strict sel cases_list in
      [%test_result: Bits.t] (For_collection.mux sel cases_array) ~expect:list_mux;
      [%test_result: Bits.t]
        (For_collection.mux_strict sel cases_array)
        ~expect:list_mux_strict)
  ;;

  let%expect_test "For_array" =
    test
      (module struct
        type 'a collection = 'a array

        let of_list = Array.of_list
        let to_list = Array.to_list

        include Bits.For_array
      end)
  ;;

  let%expect_test "For_iarray" =
    test
      (module struct
        type 'a collection = 'a iarray

        let of_list = Iarray.of_list
        let to_list = Iarray.to_list

        include Bits.For_iarray
      end)
  ;;
end

module%test Test_generate = struct
  open Core

  let%expect_test "generate raises for non-positive widths" =
    require_does_raise (fun () -> generate 0);
    [%expect {| ("Bits.generate: width must be positive" (width 0)) |}];
    require_does_raise (fun () -> generate (-1));
    [%expect {| ("Bits.generate: width must be positive" (width -1)) |}]
  ;;

  let%expect_test "generate produces values of the requested width in range" =
    List.iter [ 1; 3; 8; 17; 64; 65 ] ~f:(fun width ->
      Quickcheck.test
        (Bits.generate width)
        ~sexp_of:[%sexp_of: Bits.t]
        ~trials:200
        ~f:(fun bits ->
          [%test_result: int] (Bits.width bits) ~expect:width;
          let as_bigint = Bits.to_bigint ~signedness:Unsigned bits in
          if Bigint.(as_bigint < zero || as_bigint >= one lsl width)
          then
            raise_s [%message "value out of range" (width : int) (as_bigint : Bigint.t)]))
  ;;

  let print_example_values ~width ~num_samples =
    let random = Splittable_random.of_int 0x0ead_beef in
    let generator = Bits.generate width in
    for _ = 1 to num_samples do
      let bits = Quickcheck.Generator.generate generator ~size:10 ~random in
      print_endline (Bits.Binary.to_string bits)
    done
  ;;

  let%expect_test "generate is approximately uniform over all bit patterns" =
    print_example_values ~width:4 ~num_samples:10;
    [%expect
      {|
      4'b1110
      4'b0111
      4'b0000
      4'b0111
      4'b1001
      4'b1100
      4'b1011
      4'b1110
      4'b0011
      4'b0011
      |}];
    print_example_values ~width:70 ~num_samples:10;
    [%expect
      {|
      70'b0001111110100011101010101100110010100000110111011101110100000001001110
      70'b0001110100101100010100100101011011000010011101011010001010111001010000
      70'b1111001010000011010010010110100011001010101101101011100100110000101001
      70'b1011101000100011110011000010001010010010001000110000000111011111101011
      70'b0100111001110111001100100000000000111101111101100001010110111101100011
      70'b0110011110011001000101111011010101000111101010110000111001000110110100
      70'b0000000111110010000010101011011100010101011101001000001011101110100000
      70'b0101010100101101000010001101010000100111100011011100101010110101010001
      70'b0101011011100101101011010110001010111101011010011010010110110101001000
      70'b0001110100101001011011101101110101111001000111100001111110101101001000
      |}]
  ;;
end

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

  let arg_type =
    Command.Arg_type.of_alist_exn
      ~list_values_in_help:false
      (List.map all ~f:(fun t -> name t, t))
  ;;
end

module Bits_module = struct
  type t =
    | Bits_list
    | Bool_list
    | X_list
    | Bits
    | BadPrimitives
  [@@deriving enumerate]

  type bits_module =
    { name : string
    ; short_name : string
    ; module_ : ((module Comb.S)[@sexp.opaque])
    }
  [@@deriving sexp_of]

  let v_Bits_list =
    { name = "Int_list"; short_name = "intlist"; module_ = (module Bits_list.Int_comb) }
  ;;

  let v_Bool_list =
    { name = "Bool_list"
    ; short_name = "boollist"
    ; module_ = (module Bits_list.Bool_comb)
    }
  ;;

  let v_X_list =
    { name = "X_list"; short_name = "xlist"; module_ = (module Bits_list.X_comb) }
  ;;

  let v_Bits = { name = "Bits"; short_name = "bits"; module_ = (module Bits) }

  let v_BadPrimitives =
    let module BadPrimitives = struct
      include Bits

      let ( +: ) a b = ~:(a +: b)
      let ( *: ) a b = ~:(a *: b)
      let concat_msb d = ~:(concat_msb d)
      let mux s d = ~:(mux s d)
      let select d ~high ~low = ~:(select d ~high ~low)
    end
    in
    { name = "BadPrimitives"; short_name = "bad"; module_ = (module BadPrimitives) }
  ;;

  let select = function
    | Bits_list -> v_Bits_list
    | Bool_list -> v_Bool_list
    | X_list -> v_X_list
    | Bits -> v_Bits
    | BadPrimitives -> v_BadPrimitives
  ;;

  let module_ t = (select t).module_
  let name t = (select t).name
  let short_name t = (select t).short_name
  let sexp_of_t t = sexp_of_bits_module (select t)

  let arg_type =
    Command.Arg_type.of_alist_exn
      ~list_values_in_help:false
      (List.map all ~f:(fun t -> short_name t, t))
  ;;
end

module Config = struct
  type t =
    { bits1 : Bits_module.t
    ; bits2 : Bits_module.t
    ; prims : Primitive_op.t list
    ; iterations : int
    ; min_bit_width : int
    ; max_bit_width : int
    }
  [@@deriving sexp_of]
end

module type Test = Test_ with type config := Config.t

module Expect_test_require = struct
  let require here eq ~error_message =
    require ~here eq ~if_false_then_print_s:error_message
  ;;
end

module Expect_test_require_nocr = struct
  let require here eq ~error_message =
    require ~here eq ~cr:CR.Comment ~if_false_then_print_s:error_message
  ;;
end

module Make (R : Require) = struct
  module Ops (B1 : Comb.S) (B2 : Comb.S) = struct
    let brand min max = min + Random.int (max - min + 1)
    let srand b = Bits.(random ~width:b |> to_string)
    let const1 s = B1.of_string s
    let const2 s = B2.of_string s
    let bits_equal x y = String.equal (B1.to_bstr x) (B2.to_bstr y)

    let require ~error_message here eq =
      R.require ~error_message here eq;
      eq
    ;;

    let mismatch operation result1 result2 =
      [%message "mismatch" (operation : Sexp.t) (result1 : B1.t) (result2 : B2.t)]
    ;;

    let require_bits_equal here result1 result2 ~operation =
      require
        here
        (bits_equal result1 result2)
        ~error_message:(lazy (mismatch (force operation) result1 result2))
    ;;

    (* test binary operators that require both operands to have the same width *)
    let op2 here op op1 op2 min_bit_width max_bit_width =
      let bits = brand min_bit_width max_bit_width in
      let a = srand bits in
      let b = srand bits in
      let result1 = op1 (const1 a) (const1 b) in
      let result2 = op2 (const2 a) (const2 b) in
      require_bits_equal here result1 result2 ~operation:(lazy [%message a op b])
    ;;

    let add here = op2 here "+:" B1.( +: ) B2.( +: )
    let sub here = op2 here "-:" B1.( -: ) B2.( -: )
    let and_ here = op2 here "&:" B1.( &: ) B2.( &: )
    let or_ here = op2 here "|:" B1.( |: ) B2.( |: )
    let xor here = op2 here "^:" B1.( ^: ) B2.( ^: )
    let eq here = op2 here "==:" B1.( ==: ) B2.( ==: )
    let lt here = op2 here "<:" B1.( <: ) B2.( <: )

    (* test the multiplication operator, for which the operands may be different widths *)
    let opm here op op1 op2 min_bit_width max_bit_width =
      let bits1 = brand min_bit_width max_bit_width in
      let bits2 = brand min_bit_width max_bit_width in
      let a = srand bits1 in
      let b = srand bits2 in
      require_bits_equal
        here
        (op1 (const1 a) (const1 b))
        (op2 (const2 a) (const2 b))
        ~operation:(lazy [%message a op b])
    ;;

    let mulu here = opm here "*:" B1.( *: ) B2.( *: )
    let muls here = opm here "*+" B1.( *+ ) B2.( *+ )

    (* test the not operator *)
    let not_ here min_bit_width max_bit_width =
      let bits = brand min_bit_width max_bit_width in
      let arg = srand bits in
      require_bits_equal
        here
        (B1.( ~: ) (const1 arg))
        (B2.( ~: ) (const2 arg))
        ~operation:(lazy [%message "~:" arg])
    ;;

    (* test selection *)
    let sel here min_bit_width max_bit_width =
      let bits = brand min_bit_width max_bit_width in
      let b1 = Random.int bits in
      let b2 = Random.int bits in
      let high = max b1 b2 in
      let low = min b1 b2 in
      let arg = srand bits in
      require_bits_equal
        here
        (B1.select (const1 arg) ~high ~low)
        (B2.select (const2 arg) ~high ~low)
        ~operation:(lazy [%message "select" arg ~_:(high : int) ~_:(low : int)])
    ;;

    (* test multiplexing, with a data max size of 256 elements *)
    let mux here min_bit_width max_bit_width =
      let sel_bits = brand 1 8 in
      (* limit size of mux *)
      let data_bits = brand min_bit_width max_bit_width in
      let data_size =
        let n = 1 lsl (sel_bits - 1) in
        max 2 (n + Random.int (n + 1))
      in
      let sel = srand sel_bits in
      let data = Array.to_list @@ Array.init data_size ~f:(fun _ -> srand data_bits) in
      require_bits_equal
        here
        (B1.mux (const1 sel) (List.map data ~f:const1))
        (B2.mux (const2 sel) (List.map data ~f:const2))
        ~operation:(lazy [%message "mux" sel ~_:(data : string list)])
    ;;

    (* test concatenation *)
    let cat here min_bit_width max_bit_width =
      let concat1 d = B1.concat_msb @@ List.map d ~f:const1 in
      let concat2 d = B2.concat_msb @@ List.map d ~f:const2 in
      let cnt = 1 + Random.int 8 in
      let args =
        Array.to_list
        @@ Array.init cnt ~f:(fun _ -> srand @@ brand min_bit_width max_bit_width)
      in
      require_bits_equal
        here
        (concat1 args)
        (concat2 args)
        ~operation:(lazy [%message "concat" ~_:(args : string list)])
    ;;

    let run (op : Primitive_op.t) =
      match op with
      | Add -> add
      | Sub -> sub
      | Mulu -> mulu
      | Muls -> muls
      | And -> and_
      | Or -> or_
      | Xor -> xor
      | Not -> not_
      | Eq -> eq
      | Lt -> lt
      | Sel -> sel
      | Mux -> mux
      | Cat -> cat
    ;;
  end

  (* run each operation test until the first error. *)
  let test ?(stop_on_first_primitive_error = true) here (config : Config.t) =
    let module Bits1 = (val Bits_module.module_ config.bits1) in
    let module Bits2 = (val Bits_module.module_ config.bits2) in
    let module T = Ops (Bits1) (Bits2) in
    print_s [%message (config : Config.t)];
    List.iter config.prims ~f:(fun op ->
      let rec loop i =
        if i < config.iterations
           && (T.run op here config.min_bit_width config.max_bit_width
               || not stop_on_first_primitive_error)
        then loop (i + 1)
      in
      loop 0)
  ;;
end

(* test the tests *)
let%expect_test "BadPrimitives" =
  let module Test = Make (Expect_test_require_nocr) in
  let config =
    { Config.bits1 = Bits
    ; bits2 = BadPrimitives
    ; prims = Primitive_op.all
    ; iterations = 1
    ; min_bit_width = 1
    ; max_bit_width = 20
    }
  in
  Test.test [%here] config;
  [%expect
    {|
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
    (* require-failed: lib/hardcaml/hardcaml/test/lib/test_bits.ml:LINE:COL. *)
    (mismatch
      (operation (11000111011001011000 +: 00101010100100111110))
      (result1 11110001111110010110)
      (result2 00001110000001101001))
    (* require-failed: lib/hardcaml/hardcaml/test/lib/test_bits.ml:LINE:COL. *)
    (mismatch
      (operation (10 *: 01100))
      (result1 0011000)
      (result2 1100111))
    (* require-failed: lib/hardcaml/hardcaml/test/lib/test_bits.ml:LINE:COL. *)
    (mismatch
      (operation (select 100100100110101 12 0))
      (result1 0100100110101)
      (result2 1011011001010))
    (* require-failed: lib/hardcaml/hardcaml/test/lib/test_bits.ml:LINE:COL. *)
    (mismatch
      (operation (
        mux 111111 (
          001110000101111001
          000101010000111111
          110001111100101001
          111001000101000101
          101011000111011011
          110111110110110001
          101100110100000100
          111100100000000010
          010100111000101100
          111001010001011010
          010110110011110000
          010000100111001101
          101011111110010111
          001100010000101100
          010010111000010101
          011011000010111010
          010110001110111011
          010000001101101000
          101110001100110101
          110010011110111111
          100010010101001100
          111011000000001100
          000000111110101001
          100000100100010011
          101111011011010000
          000100111001111001
          111011110000110110
          000000011100011100
          100101000101001111
          110001101000000111
          001101011111010001
          000101001100000110
          000001011100011011
          101111111011101001
          111000010111110111
          010000111111011100
          110101011110010011
          110010001100001010
          111001001111010001
          101011001111111110
          010111111111111000
          110001001111011100
          111110000101101110
          111000101110001000
          011110101001100111
          010010011101010001
          000111010111000101
          100100101100110111
          110111010111000101
          011001010110111110
          010010101010011010
          111011000100111011
          100110111110001110
          000001101010111001
          111010110010010111
          100111000110111001
          100001111000000010
          011010001001110001
          010101110001110000
          000110000101001000
          101010000110111001)))
      (result1 101010000110111001)
      (result2 010101111001000110))
    (* require-failed: lib/hardcaml/hardcaml/test/lib/test_bits.ml:LINE:COL. *)
    (mismatch
      (operation (concat (0 000 110100001000111010)))
      (result1 0000110100001000111010)
      (result2 1111001011110111000101))
    |}]
;;

module Test = Make (Expect_test_require)
