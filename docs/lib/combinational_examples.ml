open Base
open Hardcaml
open Signal

module Rom = struct
  (* $MDX part-begin=rom *)
  let rom ~address =
    mux address (List.init 8 ~f:(fun i -> of_unsigned_int ~width:7 (i * 10)))
  ;;
  (* $MDX part-end *)
end

module Bin_to_gray = struct
  (* $MDX part-begin=bin_to_gray_1 *)
  let bin_to_gray_1 bin =
    let n = width bin in
    let bin = Array.of_list (bits_lsb bin) in
    let gray = Array.map bin ~f:(fun _ -> wire 1) in
    for i = 0 to n - 2 do
      gray.(i) <-- bin.(i) ^: bin.(i + 1)
    done;
    gray.(n - 1) <-- bin.(n - 1);
    concat_lsb (Array.to_list gray)
  ;;

  (* $MDX part-end *)

  (* $MDX part-begin=bin_to_gray_2 *)
  let bin_to_gray_2 bin = bin ^: srl bin ~by:1
  (* $MDX part-end *)

  let sim f =
    let sim =
      Cyclesim.create
        (Circuit.create_exn ~name:"bin_to_gray" [ output "gray" (f (input "bin" 3)) ])
    in
    let bin = Cyclesim.in_port sim "bin" in
    let gray = Cyclesim.out_port sim "gray" in
    Array.init 8 ~f:(fun i ->
      Bits.( <--. ) bin i;
      Cyclesim.cycle sim;
      !gray)
  ;;

  let%expect_test "bin to gray" =
    let q1 = sim bin_to_gray_1 in
    let q2 = sim bin_to_gray_2 in
    Stdio.print_s [%message (q1 : Bits.t array) (q2 : Bits.t array)];
    [%expect
      {|
      ((q1 (000 001 011 010 110 111 101 100))
       (q2 (000 001 011 010 110 111 101 100)))
      |}]
  ;;
end

module Priority_encoder = struct
  type fn =
    sel:Signal.t -> a:Signal.t -> b:Signal.t -> c:Signal.t -> d:Signal.t -> Signal.t

  (* $MDX part-begin=priority_encoder_1 *)
  let priority_encoder_1 ~sel ~a ~b ~c ~d =
    let out = Always.Variable.wire ~default:(zero 8) () in
    Always.(
      compile
        [ if_ sel.:(3) [ out <-- d ]
          @@ elif sel.:(2) [ out <-- c ]
          @@ elif sel.:(1) [ out <-- b ]
          @@ [ out <-- a ]
        ]);
    out.value
  ;;

  (* $MDX part-end *)

  (* $MDX part-begin=priority_encoder_2 *)
  let priority_encoder_2 ~sel ~a ~b ~c ~d =
    priority_select_with_default
      ~default:a
      With_valid.
        [ { valid = sel.:(3); value = d }
        ; { valid = sel.:(2); value = c }
        ; { valid = sel.:(1); value = b }
        ]
  ;;
  (* $MDX part-end *)
end

module Mux4 = struct
  (* $MDX part-begin=mux4 *)
  let mux4 ~address ~a ~b ~c ~d = mux address [ a; b; c; d ]
  (* $MDX part-end *)
end

module Full_add = struct
  let full_add_1 ~a ~b ~cin =
    let sum = ue a +: ue b +: uresize cin ~width:5 in
    msb sum, lsbs sum
  ;;

  let full_add_2 ~a ~b ~cin =
    let w = max (width a) (width b) in
    let sum = [ a; b; cin ] |> List.map ~f:(uresize ~width:(w + 1)) |> reduce ~f:( +: ) in
    msb sum, lsbs sum
  ;;
end

module Parity = struct
  type fn = d:Signal.t -> Signal.t

  (* $MDX part-begin=parity_1 *)
  let parity_1 ~d = reduce (bits_lsb d) ~f:( ^: )
  (* $MDX part-end *)

  (* $MDX part-begin=parity_2 *)
  let parity_2 ~d =
    let parity = ref gnd in
    for i = 0 to width d - 1 do
      parity := !parity ^: d.:(i)
    done;
    !parity
  ;;

  (* $MDX part-end *)

  (* $MDX part-begin=parity_3 *)
  let parity_3 ~d = tree ~arity:2 (bits_lsb d) ~f:(reduce ~f:( ^: ))
  (* $MDX part-end *)
end

module Alu = struct
  (* 8 Bit ALU

     {v
     op | fn
     --------
     0  | A AND B
     1  | A SUB B
     2  | A MUL B
     3  | A SHIFT_LEFT 1
     4  | A SHIFT_RIGHT 1
     5  | A AND B
     6  | A OR B
     7  | A XOR B
     8  | NOT A
     9  | 1 if A LESS_THAN B else 0
    10  | 1 if A EQUAL B else 0
     v}
  *)

  (* $MDX part-begin=alu *)
  let alu ~op ~a ~b =
    mux
      op
      ([ a +: b
       ; a -: b
       ; a *: b
       ; sll a ~by:1
       ; srl a ~by:1
       ; a &: b
       ; a |: b
       ; a ^: b
       ; ~:a
       ; a <: b
       ; a ==: b
       ; zero 8
       ]
       |> List.map ~f:(uresize ~width:8))
  ;;

  (* $MDX part-end *)

  (* $MDX part-begin=typed_alu *)
  module Op = struct
    module Enum = struct
      type t =
        | Add
        | Sub
        | Mul
        | Sll
        | Srl
        | And
        | Or
        | Xor
        | Not
        | Less
        | Equal
      [@@deriving sexp_of, compare ~localize, enumerate]
    end

    include Hardcaml.Enum.Make_enums (Enum)
  end

  module I = struct
    type 'a t =
      { op : 'a Op.Binary.t
      ; a : 'a [@bits 8]
      ; b : 'a [@bits 8]
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t = { q : 'a [@bits 8] } [@@deriving hardcaml]
  end

  let typed_alu ({ op; a; b } : _ I.t) =
    { O.q =
        Op.Binary.match_
          (module Signal)
          ~default:(zero 8)
          op
          [ Add, a +: b
          ; Sub, a -: b
          ; Mul, (a *: b).:[7, 0]
          ; Srl, srl a ~by:1
          ; Sll, sll a ~by:1
          ; And, a &: b
          ; Or, a |: b
          ; Xor, a ^: b
          ; Not, ~:a
          ; Less, uresize (a <: b) ~width:8
          ; Equal, uresize (a ==: b) ~width:8
          ]
    }
  ;;
  (* $MDX part-end *)
end
