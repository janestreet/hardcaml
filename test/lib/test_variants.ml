open! Import

module Cases = struct
  type t =
    | C0
    | C1
    | C2
    | C3
    | C4
  [@@deriving sexp_of, compare, enumerate, variants, equal]
end

module Enum = Interface.Make_enums (Cases)

module Test (Enum : Hardcaml.Interface.S_enum with module Enum := Cases) = struct
  let%expect_test "unpack_exn and pack roundtrips" =
    List.iter Cases.all ~f:(fun enum ->
      let after = Enum.of_enum (module Bits) enum |> Enum.to_enum in
      assert (Cases.equal enum after))
  ;;

  let%expect_test "muxes as expected" =
    List.iter Cases.all ~f:(fun selector ->
      let value =
        Enum.mux
          (module Bits)
          (Enum.of_enum (module Bits) selector)
          ~default:(Bits.of_int ~width:16 1)
          [ C0, Bits.of_int ~width:16 0
          ; C2, Bits.of_int ~width:16 2
          ; C3, Bits.of_int ~width:16 3
          ; C4, Bits.of_int ~width:16 4
          ]
      in
      Core_kernel.print_s [%message (selector : Cases.t) (value : Bits.t)]);
    [%expect
      {|
    ((selector C0) (value 0000000000000000))
    ((selector C1) (value 0000000000000001))
    ((selector C2) (value 0000000000000010))
    ((selector C3) (value 0000000000000011))
    ((selector C4) (value 0000000000000100)) |}]
  ;;
end

include Test (Enum.Binary)
include Test (Enum.One_hot)
