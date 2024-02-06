open Base
module Printexc = Stdlib.Printexc

module Mode = struct
  type t =
    | Disabled
    | Top_of_stack
    | Full_trace
end

(* Default caller_id mode is [Disabled]. It can be enabled by setting the environment var
   [HARDCAML_DEBUG]. If it's value is [top] then [Top_of_stack] is used - otherwise
   [Full_trace]. *)
let mode =
  let default =
    match Sys.getenv "HARDCAML_DEBUG" with
    | Some value ->
      if String.(uppercase value = "TOP") then Mode.Top_of_stack else Mode.Full_trace
    | None -> Disabled
  in
  ref default
;;

let set_mode m = mode := m

(* Small helper to find out who is the caller of a function *)
type t =
  | Top_of_stack of Printexc.location
  | Full_trace of Printexc.location option list

let sexp_of_location (t : Printexc.location) =
  let loc = Printf.sprintf "%s:%i:%i" t.filename t.line_number t.start_char in
  [%sexp (loc : string)]
;;

let sexp_of_t (t : t) =
  match t with
  | Top_of_stack s -> [%sexp (s : location)]
  | Full_trace s -> [%sexp (s : location option list)]
;;

let basic_skipped_modules =
  [ "list.ml"
  ; "list0.ml"
  ; "array.ml"
  ; "comb.ml"
  ; "interface.ml"
  ; "signal.ml"
  ; "bits.ml"
  ; "with_valid.ml"
  ; "scope.ml"
  ; "parameter.ml"
  ; "hierarchy.ml"
  ; Stdlib.__FILE__
  ]
;;

let get_skipped_modules skip =
  match skip with
  | [] -> basic_skipped_modules
  | _ -> basic_skipped_modules @ skip
;;

let get_backtrace () =
  let stack = Printexc.get_callstack 16 in
  let len = Printexc.raw_backtrace_length stack in
  stack, len
;;

let top skip =
  let skip = get_skipped_modules skip in
  let stack, len = get_backtrace () in
  let rec top pos =
    if pos = len
    then None
    else (
      match
        Printexc.get_raw_backtrace_slot stack pos
        |> Printexc.convert_raw_backtrace_slot
        |> Printexc.Slot.location
      with
      | None -> None
      | Some loc ->
        if List.mem ~equal:String.equal skip loc.filename then top (pos + 1) else Some loc)
  in
  top 0 |> Option.map ~f:(fun s -> Top_of_stack s)
;;

let full () =
  let stack, len = get_backtrace () in
  let rec full pos =
    if pos = len
    then []
    else
      (Printexc.get_raw_backtrace_slot stack pos
       |> Printexc.convert_raw_backtrace_slot
       |> Printexc.Slot.location)
      :: full (pos + 1)
  in
  Some (Full_trace (full 0))
;;

let get ?(skip = []) () =
  match !mode with
  | Disabled -> None
  | Top_of_stack -> top skip
  | Full_trace -> full ()
;;
