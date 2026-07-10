open Core
module Printexc = Stdlib.Printexc

module Slot = struct
  type t' =
    { filename : string
    ; line_number : int
    ; start_char : int
    ; defname : string
    }
  [@@deriving bin_io, compare, sexp_of, equal, hash]

  type t = t' option [@@deriving bin_io, compare, sexp_of, equal, hash]

  let create (s : Printexc.Slot.t) =
    let l = Printexc.Slot.location s in
    Option.map l ~f:(fun l ->
      { filename = l.filename
      ; line_number = l.line_number
      ; start_char = l.start_char
      ; defname = Option.value ~default:"?" (Printexc.Slot.name s)
      })
  ;;

  let format_location (l : t') =
    [%string "%{l.filename}:%{l.line_number#Int}:%{l.start_char#Int}"]
  ;;

  let format t =
    Option.value_map ~default:"?" t ~f:(fun t ->
      [%string "%{t.defname}$%{format_location t}"])
  ;;

  let sexp_of_t =
    Option.value_map
      ~default:[%sexp ("" : string)]
      ~f:(fun t -> [%sexp (format_location t : string)])
  ;;
end

type t = Slot.t list [@@deriving bin_io, sexp_of, compare, equal, hash]
