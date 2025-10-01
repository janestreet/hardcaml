open! Core
open! Import

let%expect_test "circuits round trip through binable circuit" =
  Quickcheck.test
    ~trials:100
    (Generator.gen_circuit ~allow_inputs:true ~depth:5)
    ~f:(fun circuit ->
      let outputs = Circuit.outputs circuit in
      let binable_circuit = Binable_circuit.of_outputs outputs in
      let circuit_bin_string =
        Bin_prot.Writer.to_string Binable_circuit.bin_writer_t binable_circuit
      in
      let recovered_binable_circuit =
        Bin_prot.Reader.of_string Binable_circuit.bin_reader_t circuit_bin_string
      in
      let recovered_outputs = Binable_circuit.to_outputs recovered_binable_circuit in
      let sexp_of_outputs outputs =
        let signal_graph = Signal_graph.create outputs in
        Signal_graph.fold signal_graph ~init:[] ~f:(fun acc signal ->
          Signal.Type.sexp_of_signal_recursive
            ~show_uids:true
            ~show_locs:true
            ~depth:2
            signal
          :: acc)
      in
      [%test_result: Sexp.t list]
        (sexp_of_outputs recovered_outputs)
        ~expect:(sexp_of_outputs outputs))
;;

let%expect_test "custom wave format hi-jinks" =
  (* We cannot properly serialize this format, so we convert it into [Bits.to_string] *)
  let fmt_in = Hardcaml.Wave_format.Custom (fun _ -> "foo") in
  let serialized = Bin_prot.Writer.to_string Hardcaml.Wave_format.bin_writer_t fmt_in in
  let fmt_out = Bin_prot.Reader.of_string Hardcaml.Wave_format.bin_reader_t serialized in
  (match fmt_in, fmt_out with
   | Custom fmt_in, Custom fmt_out ->
     let bits = Bits.of_string "101" in
     print_s [%message (fmt_in bits : string) (fmt_out bits : string)]
   | _ -> raise_s [%message "serialization failed"]);
  [%expect
    {|
    (("fmt_in bits"  foo)
     ("fmt_out bits" 101))
    |}]
;;
