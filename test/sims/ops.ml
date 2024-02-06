(* test all operators, at lots of different bit sizes, with and without
   optimisations. *)

open Base
open Hardcaml

let bit_sizes =
  (* test with a bunch of sizes < 64, plus sizes around words boundaries *)
  List.init 5 ~f:(fun i ->
    if i = 0
    then [ 1; 2; 3; 4; 8; 10; 15; 16; 17; 20; 25; 30; 43; 57 ]
    else List.init 5 ~f:(fun j -> (i * 64) + j - 2))
  |> List.concat
  |> List.filter ~f:(( < ) 0)
;;

let num_bit_sizes = List.length bit_sizes

module Sizes1 = Types.List (struct
  let length = num_bit_sizes
  let port_name = "d"
  let port_width = 1
end)

module Sizes =
  Interface.Update
    (Sizes1)
    (struct
      let port_names_and_widths =
        Sizes1.map2 Sizes1.port_names_and_widths bit_sizes ~f:(fun (n, _) b -> n, b)
      ;;
    end)

module Sizes_mul_ = Types.List (struct
  let length = num_bit_sizes * num_bit_sizes
  let port_name = "m"
  let port_width = 1
end)

module Sizes_mul =
  Interface.Update
    (Sizes_mul_)
    (struct
      let port_names_and_widths =
        List.map bit_sizes ~f:(fun a ->
          List.map bit_sizes ~f:(fun b -> [%string "m_%{a#Int}_%{b#Int}"], a + b))
        |> List.concat
      ;;
    end)

module I = struct
  type 'a t =
    { a : 'a Sizes.t
    ; b : 'a Sizes.t
    }
  [@@deriving hardcaml ~rtlmangle:"$"]
end

module Ops = struct
  module I = I

  module O = struct
    type 'a t =
      { add : 'a Sizes.t
      ; sub : 'a Sizes.t
      ; and_ : 'a Sizes.t
      ; or_ : 'a Sizes.t
      ; xor_ : 'a Sizes.t
      ; not_ : 'a Sizes.t
      ; eq : 'a Sizes1.t
      ; lt : 'a Sizes1.t
      }
    [@@deriving hardcaml ~rtlmangle:"$"]
  end

  open Signal

  let create (i : _ I.t) =
    let add = List.map2_exn i.a i.b ~f:( +: ) in
    let sub = List.map2_exn i.a i.b ~f:( -: ) in
    let and_ = List.map2_exn i.a i.b ~f:( &: ) in
    let or_ = List.map2_exn i.a i.b ~f:( |: ) in
    let xor_ = List.map2_exn i.a i.b ~f:( ^: ) in
    let not_ = List.map i.a ~f:( ~: ) in
    let eq = List.map2_exn i.a i.b ~f:( ==: ) in
    let lt = List.map2_exn i.a i.b ~f:( <: ) in
    { O.add; sub; and_; or_; xor_; not_; eq; lt }
  ;;

  module Circuit = Circuit.With_interface (I) (O)

  let circuit () = Circuit.create_exn ~name:"top" create
end

module Mul = struct
  module I = I

  module O = struct
    type 'a t =
      { mulu : 'a Sizes_mul.t
      ; muls : 'a Sizes_mul.t
      }
    [@@deriving hardcaml ~rtlmangle:"$"]
  end

  open Signal

  let create (i : _ I.t) =
    let mulu =
      List.map i.a ~f:(fun a -> List.map i.b ~f:(fun b -> a *: b)) |> List.concat
    in
    let muls =
      List.map i.a ~f:(fun a -> List.map i.b ~f:(fun b -> a *+ b)) |> List.concat
    in
    { O.mulu; muls }
  ;;

  module Circuit = Circuit.With_interface (I) (O)

  let circuit () = Circuit.create_exn ~name:"top" create
end

module type Test_arg = sig
  (* Some tests generate randomised interfaces. But they must be consistent between
     invocations so we need a stable random state. *)
  val random_state : Random.State.t
  val number_of_ops : int
end

module Make_concat (X : Test_arg) = struct
  open X
  module I = Sizes

  let number_of_concats = number_of_ops
  let random_int = Random.State.int X.random_state

  let concats =
    List.init number_of_concats ~f:(fun _ ->
      let count = 2 + random_int 10 in
      List.init count ~f:(fun _ -> random_int num_bit_sizes))
  ;;

  let output_widths =
    List.map concats ~f:(fun concat ->
      List.fold concat ~init:0 ~f:(fun acc idx -> acc + List.nth_exn I.port_widths idx))
  ;;

  module O' = Types.List (struct
    let port_name = "q"
    let port_width = 1
    let length = number_of_concats
  end)

  module O =
    Interface.Update
      (O')
      (struct
        let port_names_and_widths =
          List.map2_exn O'.port_names output_widths ~f:(fun n b -> n, b)
        ;;
      end)

  let create (i : _ I.t) =
    List.map concats ~f:(fun concat ->
      List.map concat ~f:(fun idx -> List.nth_exn i idx) |> Signal.concat_msb)
  ;;

  module Circuit = Circuit.With_interface (I) (O)

  let circuit () = Circuit.create_exn ~name:"top" create
end

module Default_test = struct
  let random_state = Random.State.make [| 1 |]
  let number_of_ops = 200
end

module Concat = Make_concat (Default_test)

module Make_select (X : Test_arg) = struct
  open X

  let number_of_selects = number_of_ops
  let random_int = Random.State.int X.random_state

  let selects =
    List.init number_of_selects ~f:(fun _ ->
      let index = random_int num_bit_sizes in
      let src_width = List.nth_exn bit_sizes index in
      let high = random_int src_width in
      let low = random_int (high + 1) in
      assert (low <= high);
      index, high, low)
  ;;

  let output_widths = List.map selects ~f:(fun (_, high, low) -> high - low + 1)

  module I = Sizes

  module O' = Types.List (struct
    let port_name = "q"
    let port_width = 1
    let length = number_of_selects
  end)

  module O =
    Interface.Update
      (O')
      (struct
        let port_names_and_widths =
          List.map2_exn O'.port_names output_widths ~f:(fun n b -> n, b)
        ;;
      end)

  let create (i : _ I.t) =
    List.map selects ~f:(fun (idx, high, low) ->
      Signal.select (List.nth_exn i idx) high low)
  ;;

  module Circuit = Circuit.With_interface (I) (O)

  let circuit () = Circuit.create_exn ~name:"top" create
end

module Select = Make_select (Default_test)
