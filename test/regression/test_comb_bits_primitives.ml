(** Application to run tests between 2 different modules from [Bits.Comb]. *)

open! Import
module Bits_module = Test_bits.Bits_module
module Prim = Test_bits.Primitive_op

let command =
  Command.basic
    ~summary:"Test [Bits] module primitives."
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and bits =
       flag
         "-bits"
         (required Bits_module.arg_type)
         ~doc:
           ("MODULE bits implementation to test: "
            ^ String.concat ~sep:", " (List.map Bits_module.all ~f:Bits_module.short_name)
           )
     and bits_ref =
       flag
         "-bits-ref"
         (optional_with_default Bits_module.Bits_int_array Bits_module.arg_type)
         ~doc:"MODULE reference bits implementation (default is [bits-int])"
     and first_error_only =
       flag
         "-first-error-only"
         no_arg
         ~doc:" stop running a primitive test after first error"
     and iterations =
       flag
         "-iterations"
         (optional_with_default 10 int)
         ~doc:"N number of test iterations (default is [10])"
     and log =
       match%map
         flag "-log" (optional string) ~doc:"FILE results file (default is [stdout])"
       with
       | None -> stdout
       | Some f -> Out_channel.create f
     and max_bit_width =
       flag "-max-bit-width" (optional_with_default 100 int) ~doc:"N (default is 100)"
     and min_bit_width =
       flag "-min-bit-width" (optional_with_default 1 int) ~doc:"N (default is 1)"
     and prims =
       flag
         "-primitives"
         (optional_with_default
            Prim.all
            (Arg_type.comma_separated Prim.arg_type ~allow_empty:true))
         ~doc:
           ("PRIM,... primitives to test: "
            ^ String.concat ~sep:", " (List.map Prim.all ~f:Prim.name))
     and verbose = flag "-verbose" no_arg ~doc:" print every test" in
     fun () ->
       let module T =
         Test_bits.Make (struct
           let require _here eq ~error_message =
             if (not eq) || verbose
             then fprintf log "%s\n" (Sexp.to_string_hum @@ Lazy.force error_message)
           ;;
         end)
       in
       T.test
         ~stop_on_first_primitive_error:first_error_only
         [%here]
         { bits1 = bits_ref
         ; bits2 = bits
         ; prims
         ; iterations
         ; min_bit_width
         ; max_bit_width
         })
;;

let () = Command.run ~version:"1.0" ~build_info:"?" command
