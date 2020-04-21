include Base
include Expect_test_helpers_core
include Hardcaml
module Command = Core.Command
module Waves = Hardcaml_waveterm

let error_s = Or_error.error_s
let force = Lazy.force
let incr = Int.incr
let print_endline = Stdio.print_endline
let print_string = Caml.print_string
let printf = Caml.Printf.printf
let try_with = Or_error.try_with
let require_does_not_raise = require_does_not_raise ~hide_positions:true
let require_does_raise = require_does_raise ~hide_positions:true
let print_s = print_s ~hide_positions:true
let show_raise ?show_backtrace f = show_raise ~hide_positions:true ?show_backtrace f
let clock = Signal.input "clock" 1
let reset = Signal.input "reset" 1
let clear = Signal.input "clear" 1
let enable = Signal.input "enable" 1
