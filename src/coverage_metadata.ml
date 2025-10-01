open! Core0
open Coverage_prim

module Kind = struct
  type t =
    | Variable of Always_metadata.Variable.t
    | If of Always_metadata.If.t
    | Switch_mux of Always_metadata.Switch_mux.t
    | Switch_cases of Always_metadata.Switch_cases.t
  [@@deriving bin_io, sexp_of]
end

module Waiver = struct
  type t =
    | Mux of int Waiver.t
    | Cases of Case.t Waiver.t
    | Reg of Toggle.t Waiver.t
    | Always_state of
        { state : string Waiver.t
        ; transition : string Transition.t Waiver.t
        }
  [@@deriving bin_io, sexp_of]

  let to_string t =
    let maybe_to_string w ~f = if Waiver.is_none w then "" else Waiver.to_string w ~f in
    match t with
    | Mux mux -> maybe_to_string mux ~f:Int.to_string
    | Cases cases -> maybe_to_string cases ~f:Case.to_string
    | Reg reg -> maybe_to_string reg ~f:Toggle.to_string
    | Always_state { state; transition } ->
      List.to_string
        [ maybe_to_string state ~f:Fn.id
        ; maybe_to_string transition ~f:(Transition.to_string ~f:Fn.id)
        ]
        ~f:Fn.id
  ;;

  let is_none = function
    | Mux mux -> Waiver.is_none mux
    | Cases cases -> Waiver.is_none cases
    | Reg reg -> Waiver.is_none reg
    | Always_state { state; transition } ->
      Waiver.is_none state && Waiver.is_none transition
  ;;

  let join_opt_exn t_opt t =
    match t_opt, t with
    | None, t -> t
    | Some (Mux m1), Mux m2 -> Mux (Waiver.join m1 m2 ~equal:Int.equal)
    | Some (Cases c1), Cases c2 -> Cases (Waiver.join c1 c2 ~equal:Case.equal)
    | Some (Reg r1), Reg r2 -> Reg (Waiver.join r1 r2 ~equal:Toggle.equal)
    | ( Some (Always_state { state = s1; transition = t1 })
      , Always_state { state = s2; transition = t2 } ) ->
      Always_state
        { state = Waiver.join s1 s2 ~equal:String.equal
        ; transition = Waiver.join t1 t2 ~equal:(Transition.equal String.equal)
        }
    | Some t, adding ->
      raise_s [%message "Trying to add a waiver of the wrong type" (t : t) (adding : t)]
  ;;

  let add_mux_exn t_opt waiver = join_opt_exn t_opt (Mux waiver)
  let add_case_exn t_opt waiver = join_opt_exn t_opt (Cases waiver)
  let add_toggle_exn t_opt waiver = join_opt_exn t_opt (Reg waiver)

  let add_always_state_exn t_opt waiver =
    join_opt_exn t_opt (Always_state { state = waiver; transition = Waiver.none () })
  ;;

  let add_always_state_transition_exn t_opt waiver =
    join_opt_exn t_opt (Always_state { state = Waiver.none (); transition = waiver })
  ;;
end

type t =
  { waiver : Waiver.t option
  ; kind : Kind.t option
  }
[@@deriving bin_io, sexp_of]

let or_default t_opt = Option.value t_opt ~default:{ waiver = None; kind = None }

let set_kind t_opt kind =
  let t = or_default t_opt in
  { t with kind = Some kind }
;;

let add_mux_waiver_exn t_opt waiver =
  let t = or_default t_opt in
  { t with waiver = Some (Waiver.add_mux_exn t.waiver waiver) }
;;

let add_cases_waiver_exn t_opt waiver =
  let t = or_default t_opt in
  { t with waiver = Some (Waiver.add_case_exn t.waiver waiver) }
;;

let add_register_waiver_exn t_opt waiver =
  let t = or_default t_opt in
  { t with waiver = Some (Waiver.add_toggle_exn t.waiver waiver) }
;;

let add_always_state_waiver_exn t_opt waiver =
  let t = or_default t_opt in
  { t with waiver = Some (Waiver.add_always_state_exn t.waiver waiver) }
;;

let add_always_state_transition_waiver_exn t_opt waiver =
  let t = or_default t_opt in
  { t with waiver = Some (Waiver.add_always_state_transition_exn t.waiver waiver) }
;;
