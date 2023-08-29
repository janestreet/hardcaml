open Base
open Cyclesim
module Out_channel = Stdio.Out_channel

let vcdcycle = 10

module Timescale = struct
  type t =
    | Fs of int
    | Ps of int
    | Ns of int
    | Us of int
    | Ms of int
    | S of int
  [@@deriving sexp_of]

  let to_string t =
    match t with
    | Fs i -> [%string "%{i#Int}fs"]
    | Ps i -> [%string "%{i#Int}ps"]
    | Ns i -> [%string "%{i#Int}ns"]
    | Us i -> [%string "%{i#Int}us"]
    | Ms i -> [%string "%{i#Int}ms"]
    | S i -> [%string "%{i#Int}s"]
  ;;
end

module Var = struct
  module Type = struct
    type t =
      | Event
      | Integer
      | Parameter
      | Real
      | Reg
      | Supply0
      | Supply1
      | Time
      | Tri
      | Triand
      | Trior
      | Trireg
      | Tri0
      | Tri1
      | Want
      | Wire
      | Wor
    [@@deriving sexp_of, compare, hash]

    let to_string t = sexp_of_t t |> Sexp.to_string |> String.lowercase
  end

  type t =
    { typ : Type.t
    ; name : string
    ; id : string
    ; width : int
    }
  [@@deriving sexp_of, fields ~getters, compare, hash]

  let create ?(typ = Type.Wire) ~name ~id ~width () = { typ; name; id; width }

  let define chan { typ; name; id; width } =
    Out_channel.output_string
      chan
      [%string "$var %{typ#Type} %{width#Int} %{id} %{name} $end\n"]
  ;;

  let write_string chan { typ = _; name = _; id; width } bits =
    if width = 1
    then Out_channel.output_string chan [%string "%{bits}%{id}\n"]
    else Out_channel.output_string chan [%string "b%{bits} %{id}\n"]
  ;;

  let write_bits chan ({ typ = _; name = _; id = _; width } as t) bits =
    if Bits.width bits <> width
    then raise_s [%message "Invalid bit width" (t : t) (bits : Bits.t)];
    write_string chan t (Bits.to_string bits)
  ;;

  let write_four_state_vector
        chan
        ({ typ = _; name = _; id = _; width } as t)
        (four_state : Logic.Four_state_vector.t)
    =
    if Logic.Four_state_vector.width four_state <> width
    then
      raise_s
        [%message "Invalid bit width" (t : t) (four_state : Logic.Four_state_vector.t)];
    let s = List.map four_state ~f:Logic.Four_state.to_char |> String.of_char_list in
    write_string chan t s
  ;;

  module Generator = struct
    type t = int ref

    let min_id_char = 33
    let max_id_char = 126
    let id_char_range = max_id_char - min_id_char + 1
    let create () = ref 0

    let rec create_identifier x =
      let y, left = x % id_char_range, x / id_char_range in
      let c = Char.of_int_exn (y + min_id_char) in
      if left <> 0 then c :: create_identifier (left - 1) else c :: []
    ;;

    let next t =
      let x = !t in
      Int.incr t;
      create_identifier x |> String.of_char_list
    ;;
  end
end

module Scope = struct
  module Type = struct
    type t =
      | Begin
      | Fork
      | Function
      | Module
      | Task
    [@@deriving sexp_of]

    let to_string t = sexp_of_t t |> Sexp.to_string |> String.lowercase
  end

  type t =
    { name : string
    ; typ : Type.t
    ; vars : Var.t list
    ; subscopes : t list
    }
  [@@deriving sexp_of, fields ~getters]

  let create ?(subscopes = []) ?(typ = Type.Module) ~name ~vars () =
    { name; typ; vars; subscopes }
  ;;

  let rec write chan { name; typ; vars; subscopes } =
    Out_channel.output_string chan [%string "$scope %{typ#Type} %{name} $end\n"];
    List.iter vars ~f:(Var.define chan);
    List.iter subscopes ~f:(write chan);
    Out_channel.output_string chan [%string "$upscope $end\n"]
  ;;

  let rec all_vars t = List.concat ([ t.vars ] @ List.map t.subscopes ~f:all_vars)
end

module Config = struct
  type t =
    { date : string
    ; version : string
    ; comment : string option
    ; timescale : Timescale.t
    }
  [@@deriving sexp_of]

  let default =
    { date = "..."
    ; version = "Hardcaml"
    ; comment = Some "Hardware design in ocaml"
    ; timescale = Ns 1
    }
  ;;

  let write chan t =
    Out_channel.output_string chan [%string "$date\n  %{t.date}\n$end\n"];
    Out_channel.output_string chan [%string "$version\n  %{t.version}\n$end\n"];
    Option.iter t.comment ~f:(fun comment ->
      Out_channel.output_string chan [%string "$comment\n  %{comment}\n$end\n"]);
    Out_channel.output_string chan [%string "$timescale %{t.timescale#Timescale} $end\n"]
  ;;
end

type trace =
  { var : Var.t
  ; data : Bits.t ref
  ; prev : string ref
  }

let enddefinitions chan = Out_channel.output_string chan "$enddefinitions $end\n"

let dumpvars_as_x chan vars =
  Out_channel.output_string chan "$dumpvars\n";
  List.iter vars ~f:(fun v ->
    Var.write_four_state_vector
      chan
      v
      (List.init v.width ~f:(Fn.const Logic.Four_state.X)));
  Out_channel.output_string chan "$end\n"
;;

let write_header chan ~config ~scopes =
  Config.write chan config;
  List.iter scopes ~f:(Scope.write chan);
  enddefinitions chan;
  dumpvars_as_x chan (List.concat (List.map scopes ~f:Scope.all_vars))
;;

let write_time chan time = Out_channel.output_string chan [%string "#%{time#Int}\n"]

let wrap chan sim =
  let var_generator = Var.Generator.create () in
  let clock =
    Var.create ~name:"clock" ~id:(Var.Generator.next var_generator) ~width:1 ()
  in
  let reset =
    Var.create ~name:"reset" ~id:(Var.Generator.next var_generator) ~width:1 ()
  in
  let write_var v d = Var.write_string chan v d in
  (* list of signals to trace *)
  let trace signals =
    let width s = String.length (Bits.to_bstr s) in
    let xs w = String.init w ~f:(fun _ -> 'x') in
    List.map signals ~f:(fun (n, s) ->
      { var =
          Var.create ~name:n ~id:(Var.Generator.next var_generator) ~width:(width !s) ()
      ; data = s
      ; prev = ref (xs (width !s))
      })
  in
  let trace_in = trace (in_ports sim) in
  let trace_out = trace (out_ports sim ~clock_edge:Before) in
  let trace_internal = trace (internal_ports sim) in
  (* filter out 'clock' and 'reset' *)
  let trace_in =
    List.filter trace_in ~f:(fun s ->
      (not (String.equal (Var.name s.var) "clock"))
      && not (String.equal (Var.name s.var) "reset"))
  in
  (* write the VCD header *)
  let scopes =
    [ Scope.create
        ~name:"inputs"
        ~vars:(clock :: reset :: List.map trace_in ~f:(fun t -> t.var))
        ()
    ; Scope.create ~name:"outputs" ~vars:(List.map trace_out ~f:(fun t -> t.var)) ()
    ; Scope.create ~name:"various" ~vars:(List.map trace_internal ~f:(fun t -> t.var)) ()
    ]
  in
  let write_header () = write_header chan ~config:Config.default ~scopes in
  let time = ref 0 in
  write_header ();
  (* reset *)
  let write_reset () =
    write_time chan !time;
    write_var clock "0";
    write_var reset "1";
    List.iter trace_in ~f:(fun t ->
      write_var t.var (Bits.to_bstr !(t.data));
      t.prev := Bits.to_bstr !(t.data));
    List.iter trace_out ~f:(fun t ->
      write_var t.var (Bits.to_bstr !(t.data));
      t.prev := Bits.to_bstr !(t.data));
    List.iter trace_internal ~f:(fun t ->
      write_var t.var (Bits.to_bstr !(t.data));
      t.prev := Bits.to_bstr !(t.data));
    time := !time + vcdcycle
  in
  (* cycle *)
  let write_cycle () =
    write_time chan !time;
    write_var clock "1";
    write_var reset "0";
    List.iter trace_in ~f:(fun t ->
      let data = Bits.to_bstr !(t.data) in
      if not (String.equal data !(t.prev))
      then (
        write_var t.var data;
        t.prev := data));
    List.iter trace_out ~f:(fun t ->
      let data = Bits.to_bstr !(t.data) in
      if not (String.equal data !(t.prev))
      then (
        write_var t.var data;
        t.prev := data));
    List.iter trace_internal ~f:(fun t ->
      let data = Bits.to_bstr !(t.data) in
      if not (String.equal data !(t.prev))
      then (
        write_var t.var data;
        t.prev := data));
    write_time chan (!time + (vcdcycle / 2));
    write_var clock "0";
    time := !time + vcdcycle
  in
  Private.modify sim [ After, Reset, write_reset; After, At_clock_edge, write_cycle ]
;;
