open! Core0

let verbose_exe_coverage =
  lazy (Sys.getenv "HARDCAML_CYCLESIM_COVERAGE_VERBOSE" |> Option.is_some)
;;

let exe_coverage =
  lazy
    (Sys.getenv "HARDCAML_CYCLESIM_COVERAGE" |> Option.is_some
     || force verbose_exe_coverage)
;;

module Mode = struct
  type t =
    | Disabled
    | Exe
    | Expect_test
  [@@deriving equal ~localize]
end

type t =
  { mutable mode : Mode.t
  ; mutable verbose : bool
  }

let global =
  let verbose = force verbose_exe_coverage in
  let mode = if verbose || force exe_coverage then Mode.Exe else Disabled in
  { mode; verbose }
;;

let coverage_enabled () =
  match global.mode with
  | Disabled -> false
  | _ -> true
;;

module For_cyclesim_coverage = struct
  let exe_coverage_enabled () =
    match global.mode with
    | Exe -> true
    | _ -> false
  ;;

  let verbose () = global.verbose

  let set_expect_test_mode ?(verbose = false) () =
    global.mode <- Expect_test;
    global.verbose <- verbose
  ;;
end
