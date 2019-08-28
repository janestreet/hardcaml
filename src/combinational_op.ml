open! Import

type create_fn = (Bits.t list -> Bits.t list[@sexp.opaque]) [@@deriving sexp_of]

type create_fn_mutable =
  (Bits.Mutable.t list -> Bits.Mutable.t list -> unit[@sexp.opaque])
[@@deriving sexp_of]

module Native_or_derived = struct
  type 'a t =
    | Native of 'a
    | Derived of 'a
  [@@deriving sexp_of]

  let unwrap = function
    | Native t | Derived t -> t
  ;;
end

type t =
  { name : string
  ; input_widths : int list
  ; output_widths : int list
  ; create_fn : create_fn Native_or_derived.t
  ; create_fn_mutable : create_fn_mutable Native_or_derived.t
  }
[@@deriving fields, sexp_of]

let mutable_of_bits f src dst =
  let bits = f (List.map src ~f:Bits.Mutable.to_bits) in
  List.iter2_exn bits dst ~f:(fun bits dst -> Bits.Mutable.copy_bits ~src:bits ~dst)
;;

let bits_of_mutable ~input_widths ~output_widths f =
  let inputs = List.map input_widths ~f:Bits.Mutable.create in
  let outputs = List.map output_widths ~f:Bits.Mutable.create in
  fun input_bits ->
    List.iter2_exn input_bits inputs ~f:(fun src dst -> Bits.Mutable.copy_bits ~src ~dst);
    f inputs outputs;
    List.map outputs ~f:Bits.Mutable.to_bits
;;

let create ?create_fn ?create_fn_mutable () ~name ~input_widths ~output_widths =
  if List.is_empty output_widths
  then raise_s [%message "[Combinational_op]s require at least one output"];
  List.iter input_widths ~f:(fun width ->
    if width <= 0
    then raise_s [%message "[Combinational_op] input width <=0" (width : int)]);
  List.iter output_widths ~f:(fun width ->
    if width <= 0
    then raise_s [%message "[Combinational_op] output width <=0" (width : int)]);
  let (create_fn : _ Native_or_derived.t), (create_fn_mutable : _ Native_or_derived.t) =
    match create_fn, create_fn_mutable with
    | Some f1, Some f2 -> Native f1, Native f2
    | Some f, None -> Native f, Derived (mutable_of_bits f)
    | None, Some f -> Derived (bits_of_mutable ~input_widths ~output_widths f), Native f
    | None, None ->
      raise_s
        [%message
          "[Combinational_op.create] must be supplied either [create_fn] or \
           [create_fn_mutable]"]
  in
  { name; input_widths; output_widths; create_fn; create_fn_mutable }
;;

let create_fn t = Native_or_derived.unwrap t.create_fn
let create_fn_mutable t = Native_or_derived.unwrap t.create_fn_mutable

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
  List.map outputs ~f:(fun (name, _) -> instance#o name)
;;
