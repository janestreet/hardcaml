(** fft.ml --- Cooley-Tukey fast Fourier transform algorithm *)

open Base
module Complex = Stdlib.Complex

(** [bitrev n i] bit-reverses [n]-digit integer [i]. *)
let bitrev =
  let rec aux acc n i =
    if n = 0 then acc else aux ((acc lsl 1) lor (i land 1)) (n - 1) (i lsr 1)
  in
  aux 0
;;

(* $MDX part-begin=swfft *)
let make_twiddle_factors len =
  let pi = 3.14159265358979 in
  let c = ~-.2.0 *. pi /. Float.of_int len in
  Array.init (len / 2) ~f:(fun i -> Complex.exp { re = 0.; im = c *. Float.of_int i })
;;

let fft x =
  let len = Array.length x in
  let n_bits = Int.ceil_log2 len in
  let w = make_twiddle_factors len in
  let y = Array.init len ~f:(fun i -> x.(bitrev n_bits i)) in
  for nb = 1 to n_bits do
    let n = 1 lsl nb in
    let m = 1 lsl (n_bits - nb) in
    for h = 0 to m - 1 do
      let ofs = n * h in
      for i = 0 to (n / 2) - 1 do
        let j, k = ofs + i, ofs + i + (n / 2) in
        let a, b = y.(j), y.(k) in
        let wm = w.(i * m) in
        let wb = Complex.mul wm b in
        y.(j) <- Complex.add a wb;
        y.(k) <- Complex.sub a wb
      done
    done
  done;
  y
;;

(* $MDX part-end *)

let ifft x =
  let c = 1.0 /. Float.of_int (Array.length x) in
  let normalize (z : Complex.t) = { Complex.re = c *. z.re; im = ~-.c *. z.im } in
  fft (Array.map ~f:normalize x)
;;

let test () =
  let x =
    [| 1.0
     ; 2.0
     ; 3.0
     ; 4.0
     ; 5.0
     ; 6.0
     ; 7.0
     ; 8.0
     ; 9.0
     ; 10.0
     ; 11.0
     ; 12.0
     ; 13.0
     ; 14.0
     ; 15.0
     ; 16.0
    |]
    |> Array.map ~f:(fun x -> { Complex.re = x; im = 0.0 })
  in
  let y = fft x in
  Stdio.printf "FFT =\n";
  Array.iteri ~f:(fun i yi -> Stdio.printf "[%d] %f %+fi\n" i yi.re yi.im) y;
  let z = ifft y in
  Stdio.printf "IFFT =\n";
  Array.iteri ~f:(fun i zi -> Stdio.printf "[%d] %f %+fi\n" i zi.re zi.im) z
;;

let%expect_test "" =
  test ();
  [%expect
    {|
    FFT =
    [0] 136.000000 +0.000000i
    [1] -8.000000 +40.218716i
    [2] -8.000000 +19.313708i
    [3] -8.000000 +11.972846i
    [4] -8.000000 +8.000000i
    [5] -8.000000 +5.345429i
    [6] -8.000000 +3.313708i
    [7] -8.000000 +1.591299i
    [8] -8.000000 +0.000000i
    [9] -8.000000 -1.591299i
    [10] -8.000000 -3.313708i
    [11] -8.000000 -5.345429i
    [12] -8.000000 -8.000000i
    [13] -8.000000 -11.972846i
    [14] -8.000000 -19.313708i
    [15] -8.000000 -40.218716i
    IFFT =
    [0] 1.000000 -0.000000i
    [1] 2.000000 -0.000000i
    [2] 3.000000 +0.000000i
    [3] 4.000000 +0.000000i
    [4] 5.000000 +0.000000i
    [5] 6.000000 -0.000000i
    [6] 7.000000 +0.000000i
    [7] 8.000000 +0.000000i
    [8] 9.000000 +0.000000i
    [9] 10.000000 +0.000000i
    [10] 11.000000 +0.000000i
    [11] 12.000000 -0.000000i
    [12] 13.000000 -0.000000i
    [13] 14.000000 -0.000000i
    [14] 15.000000 -0.000000i
    [15] 16.000000 -0.000000i
    |}]
;;

open Hardcaml
open Hardcaml_waveterm
open Signal

(* $MDX part-begin=dcomplex *)
module Dcomplex = struct
  type 'a t =
    { re : 'a [@bits 64]
    ; im : 'a [@bits 64]
    }
  [@@deriving hardcaml]

  open Cyclesim_float_ops.Double

  let add a b = { re = a.re +: b.re; im = a.im +: b.im }
  let sub a b = { re = a.re -: b.re; im = a.im -: b.im }

  let mul a b =
    { re = (a.re *: b.re) -: (a.im *: b.im); im = (a.re *: b.im) +: (a.im *: b.re) }
  ;;
end
(* $MDX part-end *)

module Write_port = struct
  type 'a t =
    { enable : 'a
    ; address : 'a [@bits 4]
    ; data : 'a Dcomplex.t
    }
  (* We use ~rtlmangle:false here to improve the readability of the printed waveform. This
     is fine to do here since this is a small module used only for demonstration. *)
  [@@deriving hardcaml ~rtlmangle:false]
end

module I = struct
  type 'a t =
    { clock : 'a
    ; clear : 'a
    ; start : 'a
    ; write : 'a Write_port.t [@rtlprefix "i$"]
    ; read_address : 'a [@bits 4]
    }
  [@@deriving hardcaml ~rtlmangle:false]
end

module O = struct
  type 'a t =
    { done_ : 'a
    ; data_out : 'a Dcomplex.t [@rtlprefix "o$"]
    }
  [@@deriving hardcaml ~rtlmangle:false]
end

(* $MDX part-begin=loop_controller *)
module State = struct
  type t =
    | Start
    | Loop
  [@@deriving sexp_of, compare ~localize, enumerate]
end

let loop_controller (i : _ I.t) =
  let spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
  let pass = Always.Variable.reg spec ~width:(Int.ceil_log2 4) in
  let count = Always.Variable.reg spec ~width:3 in
  let sm = Always.State_machine.create (module State) spec in
  Always.(
    compile
      [ sm.switch
          [ Start, [ pass <--. 0; count <--. 0; when_ i.start [ sm.set_next Loop ] ]
          ; ( Loop
            , [ count <-- count.value +:. 1
              ; when_
                  (count.value ==:. 7)
                  [ pass <-- pass.value +:. 1
                  ; when_ (pass.value ==:. 3) [ sm.set_next Start ]
                  ]
              ] )
          ]
      ]);
  pass.value, count.value, sm.is Start
;;

(* $MDX part-end *)

(* $MDX part-begin=index_generation *)
let index_generator ~pass ~count =
  let jk const =
    mux
      pass
      [ count @: const
      ; count.:[2, 1] @: const @: count.:[0, 0]
      ; count.:[2, 2] @: const @: count.:[1, 0]
      ; const @: count.:[2, 0]
      ]
  in
  let j = jk gnd in
  let k = jk vdd in
  let m =
    mux pass [ sll count ~by:3; sll count ~by:2; sll count ~by:1; sll count ~by:0 ]
  in
  j, k, m
;;

(* $MDX part-end *)

(* $MDX part-begin=butterfly *)
let butterfly a b w =
  let wb = Dcomplex.mul w b in
  Dcomplex.add a wb, Dcomplex.sub a wb
;;

(* $MDX part-end *)

(* $MDX part-begin=twiddle_rom *)
let bits_of_float (type a) (module Comb : Comb.S with type t = a) f : a =
  Int64.bits_of_float f |> Comb.of_int64_trunc ~width:64
;;

let float_of_bits b = Bits.to_int64_trunc b |> Int64.float_of_bits

let twiddle_factor_rom m =
  let w = make_twiddle_factors 16 in
  Array.to_list w
  |> List.map ~f:(fun (w : Complex.t) ->
    { Dcomplex.re = bits_of_float (module Signal) w.re
    ; im = bits_of_float (module Signal) w.im
    })
  |> Dcomplex.Of_signal.mux m
;;

(* $MDX part-end *)

(* $MDX part-begin=memory_buffer *)
let memory_buffer
  ~clock
  ~(ext_write : _ Write_port.t)
  ~(int_write_a : _ Write_port.t)
  ~(int_write_b : _ Write_port.t)
  ~int_read_a
  ~int_read_b
  ~ext_read
  =
  let q =
    multiport_memory
      16
      ~write_ports:
        [| { write_clock = clock
           ; write_data = Dcomplex.Of_signal.pack ext_write.data
           ; write_enable = ext_write.enable
           ; write_address = reverse ext_write.address
           }
         ; { write_clock = clock
           ; write_data = Dcomplex.Of_signal.pack int_write_a.data
           ; write_enable = int_write_a.enable
           ; write_address = int_write_a.address
           }
         ; { write_clock = clock
           ; write_data = Dcomplex.Of_signal.pack int_write_b.data
           ; write_enable = int_write_b.enable
           ; write_address = int_write_b.address
           }
        |]
      ~read_addresses:[| ext_read; int_read_a; int_read_b |]
  in
  Array.map q ~f:Dcomplex.Of_signal.unpack
;;

(* $MDX part-end *)

(* $MDX part-begin=core *)
let create (i : _ I.t) =
  let scope = Scope.create () in
  let%hw pass, count, done_ = loop_controller i in
  let%hw j, k, m = index_generator ~pass ~count in
  let%hw.Dcomplex.Of_signal w = twiddle_factor_rom m in
  let%hw.Dcomplex.Of_signal a = Dcomplex.Of_signal.wires () in
  let%hw.Dcomplex.Of_signal b = Dcomplex.Of_signal.wires () in
  let%hw.Dcomplex.Of_signal a_next, b_next = butterfly a b w in
  let memory_buffer =
    memory_buffer
      ~clock:i.clock
      ~ext_write:i.write
      ~int_write_a:{ enable = ~:done_; address = j; data = a_next }
      ~int_write_b:{ enable = ~:done_; address = k; data = b_next }
      ~int_read_a:j
      ~int_read_b:k
      ~ext_read:i.read_address
  in
  Dcomplex.Of_signal.assign a memory_buffer.(1);
  Dcomplex.Of_signal.assign b memory_buffer.(2);
  { O.done_; data_out = memory_buffer.(0) }
;;

(* $MDX part-end *)

module Sim = Cyclesim.With_interface (I) (O)

(* $MDX part-begin=display_rules *)
let display_rules =
  let float name =
    Display_rule.port_name_is
      name
      ~wave_format:(Custom (fun b -> float_of_bits b |> Float.to_string))
  in
  let complex name = [ float (name ^ "$re"); float (name ^ "$im") ] in
  let bit name = Display_rule.port_name_is name ~wave_format:Bit in
  let uint name = Display_rule.port_name_is name ~wave_format:Unsigned_int in
  [ [ bit "clock"
    ; bit "clear"
    ; bit "start"
    ; bit "done_"
    ; bit "i$enable"
    ; uint "pass"
    ; uint "count"
    ; uint "j"
    ; uint "k"
    ; uint "m"
    ; uint "i$address"
    ; uint "read_address"
    ]
  ; complex "i"
  ; complex "o"
  ; complex "a"
  ; complex "b"
  ; complex "w"
  ; complex "a_next"
  ; complex "b_next"
  ]
  |> List.concat
;;

(* $MDX part-end *)

open Bits

(* $MDX part-begin=testbench_fns *)

let clear_core (sim : Sim.t) =
  let inputs = Cyclesim.inputs sim in
  inputs.clear := vdd;
  Cyclesim.cycle sim;
  inputs.clear := gnd
;;

let load_fft_coefficients (sim : Sim.t) =
  let inputs = Cyclesim.inputs sim in
  inputs.write.enable := Bits.vdd;
  for i = 0 to 15 do
    inputs.write.address <--. i;
    inputs.write.data.re := bits_of_float (module Bits) (Float.of_int (i + 1));
    inputs.write.data.im := bits_of_float (module Bits) (Float.of_int 0);
    Cyclesim.cycle sim
  done;
  inputs.write.enable := Bits.gnd
;;

let run_fft (sim : Sim.t) =
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in
  inputs.start := Bits.vdd;
  Cyclesim.cycle sim;
  inputs.start := Bits.gnd;
  while not (Bits.to_bool !(outputs.done_)) do
    Cyclesim.cycle sim
  done
;;

let read_results (sim : Sim.t) =
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in
  for i = 0 to 15 do
    inputs.read_address <--. i;
    Cyclesim.cycle sim;
    let result =
      { Complex.re = float_of_bits !(outputs.data_out.re)
      ; im = float_of_bits !(outputs.data_out.im)
      }
    in
    Stdio.printf "[%d] %f %+fi\n" i result.re result.im
  done
;;

(* $MDX part-end *)

(* $MDX part-begin=testbench *)
let hw_test () =
  let sim =
    Sim.create
      ~config:
        { Cyclesim.Config.trace_all with
          combinational_ops_database = Cyclesim_float_ops.Double.database
        }
      create
  in
  let waves, sim = Waveform.create sim in
  clear_core sim;
  load_fft_coefficients sim;
  run_fft sim;
  read_results sim;
  Waveform.expect waves ~wave_width:2 ~display_rules ~start_cycle:38
;;

(* $MDX part-end *)

(* $MDX part-begin=waveform *)
let%expect_test "" =
  hw_test ();
  [%expect
    {|
    [0] 136.000000 +0.000000i
    [1] -8.000000 +40.218716i
    [2] -8.000000 +19.313708i
    [3] -8.000000 +11.972846i
    [4] -8.000000 +8.000000i
    [5] -8.000000 +5.345429i
    [6] -8.000000 +3.313708i
    [7] -8.000000 +1.591299i
    [8] -8.000000 +0.000000i
    [9] -8.000000 -1.591299i
    [10] -8.000000 -3.313708i
    [11] -8.000000 -5.345429i
    [12] -8.000000 -8.000000i
    [13] -8.000000 -11.972846i
    [14] -8.000000 -19.313708i
    [15] -8.000000 -40.218716i
    ┌Signals────────┐┌Waves──────────────────────────────────────────────┐
    │clock          ││┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──│
    │               ││   └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  │
    │clear          ││                                                   │
    │               ││───────────────────────────────────────────────────│
    │start          ││                                                   │
    │               ││───────────────────────────────────────────────────│
    │done_          ││                                                   │
    │               ││───────────────────────────────────────────────────│
    │i$enable       ││                                                   │
    │               ││───────────────────────────────────────────────────│
    │               ││────────────────────────┬──────────────────────────│
    │pass           ││ 2                      │3                         │
    │               ││────────────────────────┴──────────────────────────│
    │               ││──────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬──│
    │count          ││ 4    │5    │6    │7    │0    │1    │2    │3    │4 │
    │               ││──────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴──│
    │               ││──────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬──│
    │j              ││ 8    │9    │10   │11   │0    │1    │2    │3    │4 │
    │               ││──────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴──│
    │               ││──────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬──│
    │k              ││ 12   │13   │14   │15   │8    │9    │10   │11   │12│
    │               ││──────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴──│
    │               ││──────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬──│
    │m              ││ 0    │2    │4    │6    │0    │1    │2    │3    │4 │
    │               ││──────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴──│
    │               ││───────────────────────────────────────────────────│
    │i$address      ││ 15                                                │
    │               ││───────────────────────────────────────────────────│
    │               ││───────────────────────────────────────────────────│
    │read_address   ││ 0                                                 │
    │               ││───────────────────────────────────────────────────│
    │               ││───────────────────────────────────────────────────│
    │i$re           ││ 16.                                               │
    │               ││───────────────────────────────────────────────────│
    │               ││───────────────────────────────────────────────────│
    │i$im           ││ 0.                                                │
    │               ││───────────────────────────────────────────────────│
    │               ││──────────────────────────────┬────────────────────│
    │o$re           ││ 64.                          │136.                │
    │               ││──────────────────────────────┴────────────────────│
    │               ││───────────────────────────────────────────────────│
    │o$im           ││ 0.                                                │
    │               ││───────────────────────────────────────────────────│
    │               ││──────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬──│
    │a$re           ││ 32.  │-8.0.│-8.  │-7.9.│64.  │-8.0.│-8.0.│-8.0.│-8│
    │               ││──────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴──│
    │               ││──────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬──│
    │a$im           ││ 0.   │8.   │0.   │-8.  │0.   │19.3.│8.   │3.31.│0.│
    │               ││──────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴──│
    │               ││──────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬──│
    │b$re           ││ 40.  │-8.0.│-8.  │-7.9.│72.  │-8.0.│-8.0.│-8.0.│-8│
    │               ││──────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴──│
    │               ││──────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬──│
    │b$im           ││ 0.   │8.   │0.   │-8.  │0.   │19.3.│8.   │3.31.│0.│
    │               ││──────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴──│
    │               ││──────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬──│
    │w$re           ││ 1.   │0.70.│1.61.│-0.7.│1.   │0.92.│0.70.│0.38.│1.│
    │               ││──────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴──│
    │               ││──────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬──│
    │w$im           ││ -0.  │-0.7.│-1.  │-0.7.│-0.  │-0.3.│-0.7.│-0.9.│-1│
    │               ││──────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴──│
    │               ││──────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬──│
    │a_next$re      ││ 72.  │-8.0.│-8.0.│-8.0.│136. │-8.0.│-8.0.│-8.0.│-8│
    │               ││──────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴──│
    │               ││──────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬──│
    │a_next$im      ││ 0.   │19.3.│8.   │3.31.│0.   │40.2.│19.3.│11.9.│8.│
    │               ││──────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴──│
    │               ││──────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬──│
    │b_next$re      ││ -8.  │-7.9.│-7.9.│-7.9.│-8.  │-7.9.│-7.9.│-7.9.│-7│
    │               ││──────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴──│
    │               ││──────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬──│
    │b_next$im      ││ 0.   │-3.3.│-8.  │-19..│0.   │-1.5.│-3.3.│-5.3.│-8│
    │               ││──────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴──│
    └───────────────┘└───────────────────────────────────────────────────┘
    7a788a5d3036c98ecb7da5ed09414842
    |}]
;;

(* $MDX part-end *)
