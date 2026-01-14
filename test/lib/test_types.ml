open Core
open Hardcaml

module Test_interface (Make : functor (_ : Types.Interface_with_length) -> Interface.S) =
struct
  module T = struct
    type 'a t =
      { a : 'a [@bits 1]
      ; b : 'a [@bits 2]
      ; c : 'a [@bits 3]
      }
    [@@deriving hardcaml]
  end

  module M = Make (struct
      include T

      let length = 2
    end)

  let%expect_test "show port_names_and_widths" =
    print_s [%message "" ~_:(M.port_names_and_widths : (string * int) M.t)];
    [%expect
      {| (((a (a_0 1)) (b (b_0 2)) (c (c_0 3))) ((a (a_1 1)) (b (b_1 2)) (c (c_1 3)))) |}]
  ;;
end

module%test Interface_list = Test_interface (Types.Interface_list)
module%test Interface_array = Test_interface (Types.Interface_array)
