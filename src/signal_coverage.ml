open Base

module Mux = struct
  module T = struct
    type t =
      { case_count : int
      ; covered_cases : int Hash_set.t
      ; select_names : Name_and_loc.t list
      }
    [@@deriving sexp_of, fields ~getters]

    let create ~select ~case_count =
      { select_names = Signal.names_and_locs select
      ; case_count
      ; covered_cases = Hash_set.create (module Int)
      }
    ;;

    let mark_choice t choice =
      let choice =
        (* If the index overflows, repeat the last case *)
        min choice (max 0 (t.case_count - 1))
      in
      Hash_set.add t.covered_cases choice
    ;;

    let total_cases t = t.case_count
    let covered_cases t = Hash_set.length t.covered_cases
    let case_covered t case = Hash_set.mem t.covered_cases case
  end

  include Coverage.Make (T)
  include T
end

module Cases = struct
  module Case = struct
    type t =
      | Specified of int
      | Default
    [@@deriving sexp_of]

    let to_string = function
      | Specified i -> Int.to_string i
      | Default -> "default"
    ;;
  end

  module Covered_cases = struct
    type t =
      { specified_cases : int Hash_set.t
      ; mutable default : bool
      }
    [@@deriving sexp_of]

    let create () = { specified_cases = Hash_set.create (module Int); default = false }

    let add (t : t) (case : Case.t) =
      match case with
      | Specified i -> Hash_set.add t.specified_cases i
      | Default -> t.default <- true
    ;;

    let length (t : t) = Hash_set.length t.specified_cases + Bool.select t.default 1 0

    let mem (t : t) (case : Case.t) =
      match case with
      | Specified i -> Hash_set.mem t.specified_cases i
      | Default -> t.default
    ;;
  end

  module T = struct
    type t =
      { cases : Case.t list
      ; covered_cases : Covered_cases.t
      ; select_names : Name_and_loc.t list
      }
    [@@deriving sexp_of, fields ~getters]

    let create ~select ~case_count =
      { cases = Case.Default :: List.init case_count ~f:(fun i -> Case.Specified i)
      ; covered_cases = Covered_cases.create ()
      ; select_names = Signal.names_and_locs select
      }
    ;;

    let mark_choice t (choice : Case.t) = Covered_cases.add t.covered_cases choice
    let total_cases t = List.length t.cases
    let covered_cases t = Covered_cases.length t.covered_cases
    let case_covered t (case : Case.t) = Covered_cases.mem t.covered_cases case
  end

  include Coverage.Make (T)
  include T
end

module Reg = struct
  module T = struct
    type t =
      { bits : int
      ; toggled_on : int Hash_set.t
      ; toggled_off : int Hash_set.t
      }
    [@@deriving sexp_of, fields ~getters]

    let create ~bits =
      { bits
      ; toggled_on = Hash_set.create (module Int)
      ; toggled_off = Hash_set.create (module Int)
      }
    ;;

    let toggled_set t ~on = if on then t.toggled_on else t.toggled_off
    let mark_toggled_bit t ~bit ~on = Hash_set.add (toggled_set t ~on) bit
    let total_cases t = t.bits * 2
    let covered_cases t = Hash_set.length t.toggled_on + Hash_set.length t.toggled_off
    let bit_toggled t ~bit ~on = Hash_set.mem (toggled_set t ~on) bit
  end

  include Coverage.Make (T)
  include T
end

type t' =
  | Mux of Mux.t
  | Reg of Reg.t
  | Cases of Cases.t
[@@deriving sexp_of]

module Debug_info = struct
  type t =
    { names : Name_and_loc.t list
    ; call_stack : Stack_slot.t list
    ; comment : string option
    }
  [@@deriving hash, compare, equal, sexp_of]

  let skipped_bottom_modules = [ "cyclesim.ml"; "circuit.ml" ]

  let is_test_file filename =
    String.substr_index filename ~pattern:"test" |> Option.is_some
  ;;

  let filter_call_stack signal =
    let call_stack =
      Option.value_map ~default:[] (Signal.Type.caller_id signal) ~f:Caller_id.call_stack
    in
    List.take_while call_stack ~f:(fun slot ->
      match Stdlib.Printexc.Slot.location slot with
      | None -> false
      | Some loc ->
        (not (List.mem ~equal:String.equal skipped_bottom_modules loc.filename))
        && not (is_test_file loc.filename))
  ;;

  let create signal =
    { names = Signal.names_and_locs signal
    ; call_stack = filter_call_stack signal
    ; comment = Signal.comment signal
    }
  ;;
end

type 'a with_metadata =
  { data : 'a
  ; id : Signal_graph.Normalized_signal_uid.t
  ; debug_info : Debug_info.t
  }
[@@deriving sexp_of]

module T = struct
  type t = t' with_metadata [@@deriving sexp_of]

  let total_cases { data; _ } =
    match data with
    | Mux mux -> Mux.total_cases mux
    | Cases cases -> Cases.total_cases cases
    | Reg reg -> Reg.total_cases reg
  ;;

  let covered_cases { data; _ } =
    match data with
    | Mux mux -> Mux.covered_cases mux
    | Cases cases -> Cases.covered_cases cases
    | Reg reg -> Reg.covered_cases reg
  ;;
end

include Coverage.Make (T)
include T

let maybe_create id (signal : Signal.t) =
  let%map.Option data =
    match signal with
    | Mux { select; cases; _ } ->
      Some (Mux (Mux.create ~select ~case_count:(List.length cases)))
    | Cases { select; cases; _ } ->
      Some (Cases (Cases.create ~select ~case_count:(List.length cases)))
    | Reg _ -> Some (Reg (Reg.create ~bits:(Signal.width signal)))
    | _ -> None
  in
  { data; id; debug_info = Debug_info.create signal }
;;

module Grouped = struct
  type flat = t list

  type t =
    { muxes : Mux.t with_metadata list
    ; cases : Cases.t with_metadata list
    ; regs : Reg.t with_metadata list
    }
  [@@deriving sexp_of]

  let of_flat (flat : flat) =
    let t = { muxes = []; cases = []; regs = [] } in
    List.fold ~init:t flat ~f:(fun t { data; id; debug_info } ->
      match data with
      | Mux mux -> { t with muxes = { data = mux; id; debug_info } :: t.muxes }
      | Cases cases -> { t with cases = { data = cases; id; debug_info } :: t.cases }
      | Reg reg -> { t with regs = { data = reg; id; debug_info } :: t.regs })
  ;;
end
