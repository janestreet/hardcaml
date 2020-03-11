open! Import
open Cyclesim

let vcdmin = 33
let vcdmax = 126
let vcdcycle = 10

type trace =
  { w : int
  ; id : string
  ; name : string
  ; data : Bits.t ref
  ; prev : string ref
  }

let wrap os sim =
  (*let name = "sim" in*)
  let osl s = os (s ^ "\n") in
  let si = Int.to_string in
  let ( ^:^ ) a b = a ^ " " ^ b in
  (* id generator *)
  let gen_id =
    let i = ref 2 in
    (* 0+1 are for clock and reset *)
    let range = vcdmax - vcdmin in
    let rec gen x =
      let d = x / range in
      let m = x mod range in
      if d = 0 then [ m ] else d :: gen (x - range)
    in
    let code x =
      List.fold (gen x) ~init:"" ~f:(fun a x ->
        String.init 1 ~f:(fun _ -> Char.of_int_exn (x + vcdmin)) ^ a)
    in
    fun () ->
      let x = !i in
      Int.incr i;
      code x
  in
  let write_var v d w = if w = 1 then osl (d ^ v) else osl ("b" ^ d ^:^ v) in
  (* list of signals to trace *)
  let trace signals =
    let width s = String.length (Bits.to_bstr s) in
    let xs w = String.init w ~f:(fun _ -> 'x') in
    List.map signals ~f:(fun (n, s) ->
      { w = width !s; id = gen_id (); name = n; data = s; prev = ref (xs (width !s)) })
  in
  let trace_in = trace (in_ports sim) in
  let trace_out = trace (out_ports sim ~clock_edge:Before) in
  let trace_internal = trace (internal_ports sim) in
  (* filter out 'clock' and 'reset' *)
  let trace_in =
    List.filter trace_in ~f:(fun s ->
      (not (String.equal s.name "clock")) && not (String.equal s.name "reset"))
  in
  (* write the VCD header *)
  let write_header () =
    os "$date\n  ...\n$end\n";
    os "$version\n  Hardcaml\n$end\n";
    os "$comment\n  Hardware design in ocaml\n$end\n";
    os "$timescale 1ns $end\n";
    os "$scope module inputs $end\n";
    os "$var wire 1 ! clock $end\n";
    os "$var wire 1 \" reset $end\n";
    let trv t = osl ("$var wire " ^ si t.w ^:^ t.id ^:^ t.name ^:^ "$end") in
    List.iter trace_in ~f:trv;
    os "$upscope $end\n";
    os "$scope module outputs $end\n";
    List.iter trace_out ~f:trv;
    os "$upscope $end\n";
    os "$scope module various $end\n";
    List.iter trace_internal ~f:trv;
    os "$upscope $end\n";
    os "$enddefinitions $end\n";
    os "$dumpvars\n";
    os "x!\n";
    os "x\"\n";
    List.iter trace_in ~f:(fun t -> write_var t.id !(t.prev) t.w);
    List.iter trace_out ~f:(fun t -> write_var t.id !(t.prev) t.w);
    List.iter trace_internal ~f:(fun t -> write_var t.id !(t.prev) t.w);
    os "$end\n"
  in
  let time = ref 0 in
  write_header ();
  (* reset *)
  let write_reset () =
    osl ("#" ^ si !time);
    osl "0!";
    osl "1\"";
    List.iter trace_in ~f:(fun t ->
      write_var t.id (Bits.to_bstr !(t.data)) t.w;
      t.prev := Bits.to_bstr !(t.data));
    List.iter trace_out ~f:(fun t ->
      write_var t.id (Bits.to_bstr !(t.data)) t.w;
      t.prev := Bits.to_bstr !(t.data));
    List.iter trace_internal ~f:(fun t ->
      write_var t.id (Bits.to_bstr !(t.data)) t.w;
      t.prev := Bits.to_bstr !(t.data));
    time := !time + vcdcycle
  in
  (* cycle *)
  let write_cycle () =
    osl ("#" ^ si !time);
    osl "1!";
    osl "0\"";
    List.iter trace_in ~f:(fun t ->
      let data = Bits.to_bstr !(t.data) in
      if not (String.equal data !(t.prev))
      then (
        write_var t.id data t.w;
        t.prev := data));
    List.iter trace_out ~f:(fun t ->
      let data = Bits.to_bstr !(t.data) in
      if not (String.equal data !(t.prev))
      then (
        write_var t.id data t.w;
        t.prev := data));
    List.iter trace_internal ~f:(fun t ->
      let data = Bits.to_bstr !(t.data) in
      if not (String.equal data !(t.prev))
      then (
        write_var t.id data t.w;
        t.prev := data));
    osl ("#" ^ si (!time + (vcdcycle / 2)));
    osl "0!";
    time := !time + vcdcycle
  in
  Private.modify sim [ After, Reset, write_reset; After, At_clock_edge, write_cycle ]
;;

module Gtkwave = struct
  let wrap chan sim =
    let o s =
      Out_channel.output_string chan s;
      Out_channel.flush chan
    in
    wrap o sim
  ;;

  let gtkwave ?(args = "") sim =
    let fifoname = Filename.temp_file "sim" "fifo" in
    printf "Created tempfile %s\n" fifoname;
    Unix.unlink fifoname;
    Unix.mkfifo fifoname 0o600;
    printf "Made fifo, launching shmidcat and gtkwave\n";
    ignore
      (Unix.open_process_in ("shmidcat " ^ fifoname ^ " | gtkwave -v -I " ^ args)
       : Stdio.In_channel.t);
    let fifo = Out_channel.create fifoname in
    at_exit (fun () ->
      printf "Destroying FIFO\n";
      Out_channel.close fifo;
      Unix.unlink fifoname);
    wrap fifo sim
  ;;
end
