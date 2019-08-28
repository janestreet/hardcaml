open! Import
module Printexc = Caml.Printexc

module Mode = struct
  type t =
    | Disabled
    | Top_of_stack
    | Full_trace
end

let mode =
  ref
    (if Base.Exported_for_specific_uses.am_testing then Mode.Disabled else Top_of_stack)
;;

let set_mode m = mode := m

(* Small helper to find out who is the caller of a function *)
type t =
  | Top_of_stack of Printexc.location
  | Full_trace of Printexc.location option list

let sexp_of_location (t : Printexc.location) =
  let loc = sprintf "%s:%i:%i" t.filename t.line_number t.start_char in
  [%sexp (loc : string)]
;;

let sexp_of_t (t : t) =
  match t with
  | Top_of_stack s -> [%sexp (s : location)]
  | Full_trace s -> [%sexp (s : location option list)]
;;

let get ?(skip = []) () =
  let skip =
    "list.ml"
    :: "list0.ml"
    :: "array.ml"
    :: "comb.ml"
    :: "interface.ml"
    :: "signal.ml"
    :: "bits.ml"
    :: "with_valid.ml"
    :: "scope.ml"
    :: "parameter.ml"
    :: "hierarchy.ml"
    :: Caml.__FILE__
    :: skip
  in
  let stack = Printexc.get_callstack 16 in
  let len = Printexc.raw_backtrace_length stack in
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
        if List.mem ~equal:String.equal skip loc.filename
        then top (pos + 1)
        else Some loc)
  in
  let rec full pos =
    if pos = len
    then []
    else
      (Printexc.get_raw_backtrace_slot stack pos
       |> Printexc.convert_raw_backtrace_slot
       |> Printexc.Slot.location)
      :: full (pos + 1)
  in
  match !mode with
  | Disabled -> None
  | Top_of_stack -> top 0 |> Option.map ~f:(fun s -> Top_of_stack s)
  | Full_trace -> Some (Full_trace (full 0))
;;
