open! Import

module Cases = struct
  type t =
    | C0
    | C1
    | C2
    | C3
    | C4
  [@@deriving sexp_of, compare ~localize, enumerate, variants, equal ~localize]
end

module Enum = Enum.Make_enums (Cases)

module Test (Enum : Hardcaml.Enum.S_enum with module Cases := Cases) = struct
  let%expect_test "unpack_exn and pack roundtrips" =
    List.iter Cases.all ~f:(fun enum ->
      let after = Enum.Of_bits.of_enum enum |> Enum.to_enum |> Or_error.ok_exn in
      assert (Cases.equal enum after))
  ;;

  let%expect_test "muxes as expected" =
    List.iter Cases.all ~f:(fun selector ->
      let value =
        Enum.Of_bits.match_
          (Enum.Of_bits.of_enum selector)
          ~default:(Bits.of_int_trunc ~width:16 1)
          [ C0, Bits.of_int_trunc ~width:16 0
          ; C2, Bits.of_int_trunc ~width:16 2
          ; C3, Bits.of_int_trunc ~width:16 3
          ; C4, Bits.of_int_trunc ~width:16 4
          ]
      in
      Core.print_s [%message (selector : Cases.t) (value : Bits.t)]);
    [%expect
      {|
      ((selector C0) (value 0000000000000000))
      ((selector C1) (value 0000000000000001))
      ((selector C2) (value 0000000000000010))
      ((selector C3) (value 0000000000000011))
      ((selector C4) (value 0000000000000100))
      |}]
  ;;

  let%expect_test "Raises when non exhaustive without default" =
    Expect_test_helpers_base.require_does_raise (fun () ->
      Enum.Of_bits.match_ (Enum.of_enum (module Bits) C0) []);
    [%expect
      {| (Failure "[mux] on enum cases not exhaustive, and [default] not provided") |}]
  ;;
end

include Test (Enum.Binary)
include Test (Enum.One_hot)

let%expect_test "Encodings and wave formats for binary" =
  let module E = Enum.Binary in
  print_s
    [%message
      ""
        ~_:
          (List.map Cases.all ~f:(fun c -> c, E.Of_bits.of_enum c)
           : (Cases.t * Bits.t E.t) list)];
  print_s [%message "" ~_:(E.wave_formats : Wave_format.t E.t)];
  [%expect
    {|
    ((C0 000)
     (C1 001)
     (C2 010)
     (C3 011)
     (C4 100))
    (Index (C0 C1 C2 C3 C4))
    |}]
;;

let%expect_test "Encodings and wave formats for one hot" =
  let module E = Enum.One_hot in
  print_s
    [%message
      ""
        ~_:
          (List.map Cases.all ~f:(fun c -> c, E.Of_bits.of_enum c)
           : (Cases.t * Bits.t E.t) list)];
  print_s [%message "" ~_:(E.wave_formats : Wave_format.t E.t)];
  [%expect
    {|
    ((C0 00001)
     (C1 00010)
     (C2 00100)
     (C3 01000)
     (C4 10000))
    (Map (
      (00001 C0)
      (00010 C1)
      (00100 C2)
      (01000 C3)
      (10000 C4)))
    |}]
;;
