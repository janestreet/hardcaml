open! Core0

type create_mutable_fn =
  inputs:Bits.Mutable.t list -> outputs:Bits.Mutable.t list -> (unit -> unit) Staged.t

type create_fn = Bits.t list -> Bits.t list

type t =
  { name : string
  ; input_widths : int list
  ; output_widths : int list
  ; create_fn : (create_mutable_fn[@sexp.opaque])
  }
[@@deriving fields ~getters, sexp_of]

let create_mutable ~name ~input_widths ~output_widths ~create_fn () =
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

let create_fn_of_bits f ~inputs ~outputs =
  Staged.stage (fun () ->
    let bits = f (List.map inputs ~f:Bits.Mutable.to_bits) in
    List.iter2_exn bits outputs ~f:(fun bits dst -> Bits.Mutable.copy_bits ~src:bits ~dst))
;;

let create ~name ~input_widths ~output_widths ~create_fn () =
  create_mutable
    ~name
    ~input_widths
    ~output_widths
    ~create_fn:(create_fn_of_bits create_fn)
    ()
;;

let instantiate t ~inputs =
  if List.length t.input_widths <> List.length inputs
  then
    raise_s
      [%message
        "[Combinational_op.instantiate] got mismatched input lengths"
          ~input_widths:(t.input_widths : int list)
          (inputs : Signal.t list)];
  List.iter2_exn t.input_widths inputs ~f:(fun expected_width input_signal ->
    if expected_width <> Signal.width input_signal
    then
      raise_s
        [%message
          "[Combinational_op.instantiate] input signal is wrong width"
            (t.name : string)
            (expected_width : int)
            (input_signal : Signal.t)]);
  let inputs = List.mapi inputs ~f:(fun i input -> "i" ^ Int.to_string i, input) in
  let outputs =
    List.mapi t.output_widths ~f:(fun i width -> "o" ^ Int.to_string i, width)
  in
  let instance = Instantiation.create () ~name:t.name ~inputs ~outputs in
  List.map outputs ~f:(fun (name, _) -> Instantiation.output instance name)
;;

module With_interface (I : Interface.S) (O : Interface.S) = struct
  type create_fn = Bits.t I.t -> Bits.t O.t

  type create_mutable_fn =
    inputs:Bits.Mutable.t I.t -> outputs:Bits.Mutable.t O.t -> (unit -> unit) Staged.t

  let create_mutable ~name ~create_fn () =
    let input_widths = I.to_list I.port_widths in
    let output_widths = O.to_list O.port_widths in
    let create_fn ~inputs ~outputs =
      if List.length inputs <> List.length input_widths
      then
        raise_s
          [%message
            "[Combinational_op.With_interface] invalid number of inputs to op"
              ~expected:(List.length input_widths : int)
              ~got:(List.length inputs : int)];
      let inputs =
        I.scan I.port_widths ~init:inputs ~f:(fun lst _ ->
          match lst with
          | hd :: tl -> tl, hd
          | _ ->
            (* this shouldn't happen because we already checked the list length *)
            failwith "error")
      in
      if List.length outputs <> List.length output_widths
      then
        raise_s
          [%message
            "[Combinational_op.With_interface] invalid number of outputs to op"
              ~expected:(List.length output_widths : int)
              ~got:(List.length outputs : int)];
      let outputs =
        O.scan O.port_widths ~init:outputs ~f:(fun lst _ ->
          match lst with
          | hd :: tl -> tl, hd
          | _ ->
            (* this shouldn't happen because we already checked the list length *)
            failwith "error")
      in
      let create_fn = Staged.unstage (create_fn ~inputs ~outputs) in
      Staged.stage (fun () -> create_fn ())
    in
    create_mutable ~name ~input_widths ~output_widths ~create_fn ()
  ;;

  let create_fn_of_bits f ~inputs ~outputs =
    Staged.stage (fun () ->
      let bits = f (I.map inputs ~f:Bits.Mutable.to_bits) in
      O.iter2 bits outputs ~f:(fun bits dst -> Bits.Mutable.copy_bits ~src:bits ~dst))
  ;;

  let create ~name ~create_fn () =
    create_mutable ~name ~create_fn:(create_fn_of_bits create_fn) ()
  ;;

  let instantiate t inputs =
    let module Inst = Instantiation.With_interface (I) (O) in
    Inst.create ~name:t.name inputs
  ;;
end
