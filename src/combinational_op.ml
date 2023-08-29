open Base

type create_fn = (Bits.Mutable.t list -> Bits.Mutable.t list -> unit[@sexp.opaque])
[@@deriving sexp_of]

type t =
  { name : string
  ; input_widths : int list
  ; output_widths : int list
  ; create_fn : create_fn
  }
[@@deriving fields ~getters, sexp_of]

let create ~name ~input_widths ~output_widths ~create_fn () =
  if List.is_empty output_widths
  then raise_s [%message "[Combinational_op]s require at least one output"];
  List.iter input_widths ~f:(fun width ->
    if width <= 0
    then raise_s [%message "[Combinational_op] input width <=0" (width : int)]);
  List.iter output_widths ~f:(fun width ->
    if width <= 0
    then raise_s [%message "[Combinational_op] output width <=0" (width : int)]);
  { name; input_widths; output_widths; create_fn }
;;

let create_fn_of_bits f src dst =
  let bits = f (List.map src ~f:Bits.Mutable.to_bits) in
  List.iter2_exn bits dst ~f:(fun bits dst -> Bits.Mutable.copy_bits ~src:bits ~dst)
;;

let instantiate t ~inputs =
  if List.length t.input_widths <> List.length inputs
  then
    raise_s
      [%message
        "[Combinational_op.instantiate] got mismatched input lengths"
          ~input_widths:(t.input_widths : int list)
          (inputs : Signal.t list)];
  let inputs = List.mapi inputs ~f:(fun i input -> "i" ^ Int.to_string i, input) in
  let outputs =
    List.mapi t.output_widths ~f:(fun i width -> "o" ^ Int.to_string i, width)
  in
  let instance = Instantiation.create () ~name:t.name ~inputs ~outputs in
  List.map outputs ~f:(fun (name, _) -> Map.find_exn instance name)
;;
