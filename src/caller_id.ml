open Base
module Printexc = Stdlib.Printexc

module Mode = struct
  type t =
    | Disabled
    | Top_of_stack
    | Coverage_filtered_trace
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

type t =
  | Top_of_stack of Stack_slot.t
  | Coverage_filtered_trace of Stack_slot.t list
  | Full_trace of Stack_slot.t list

let sexp_of_t (t : t) =
  match t with
  | Top_of_stack s -> [%sexp (s : Stack_slot.t)]
  | Coverage_filtered_trace s -> [%sexp (s : Stack_slot.t list)]
  | Full_trace s -> [%sexp (s : Stack_slot.t list)]
;;

let call_stack t =
  match t with
  | Top_of_stack slot -> [ slot ]
  | Coverage_filtered_trace slots -> slots
  | Full_trace slots -> slots
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
      let slot =
        Printexc.get_raw_backtrace_slot stack pos |> Printexc.convert_raw_backtrace_slot
      in
      match Printexc.Slot.location slot with
      | None -> None
      | Some loc ->
        if List.mem ~equal:String.equal skip loc.filename
        then top (pos + 1)
        else Some slot)
  in
  top 0 |> Option.map ~f:(fun s -> Top_of_stack s)
;;

let coverage_filtered_stack () =
  let stack, len = get_backtrace () in
  (* Exclude slots without location information *)
  let rec filtered pos =
    if pos = len
    then []
    else (
      let slot =
        Printexc.get_raw_backtrace_slot stack pos |> Printexc.convert_raw_backtrace_slot
      in
      match Printexc.Slot.location slot with
      | None -> filtered (pos + 1)
      | Some _ -> slot :: filtered (pos + 1))
  in
  (* Exclude slots pertaining to the current file. *)
  let filtered =
    filtered 0
    |> List.drop_while ~f:(fun slot ->
      match Printexc.Slot.location slot with
      | None -> false
      | Some loc -> String.equal Stdlib.__FILE__ loc.filename)
  in
  Some (Coverage_filtered_trace filtered)
;;

let full () =
  let stack, len = get_backtrace () in
  let rec full pos =
    if pos = len
    then []
    else
      (Printexc.get_raw_backtrace_slot stack pos |> Printexc.convert_raw_backtrace_slot)
      :: full (pos + 1)
  in
  Some (Full_trace (full 0))
;;

let get ?(skip = []) () =
  match !mode with
  | Disabled -> None
  | Top_of_stack -> top skip
  | Coverage_filtered_trace -> coverage_filtered_stack ()
  | Full_trace -> full ()
;;
