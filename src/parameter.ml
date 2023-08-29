open Base

module Value = struct
  type t =
    | Bit of bool
    | Bit_vector of Logic.Bit_vector.t
    | Bool of bool
    | Int of int
    | Real of float
    | Std_logic of Logic.Std_logic.t
    | Std_logic_vector of Logic.Std_logic_vector.t
    | Std_ulogic of Logic.Std_logic.t
    | Std_ulogic_vector of Logic.Std_logic_vector.t
    | String of string
  [@@deriving equal, sexp, variants]
end

type t =
  { name : Parameter_name.t
  ; value : Value.t
  }
[@@deriving equal, sexp_of]

let sexp_of_t { name; value } =
  [%message "" ~_:(name : Parameter_name.t) ~_:(value : Value.t)]
;;

let create ~name ~value = { name = name |> Parameter_name.of_string; value }

let find_name ts name =
  List.find_map ts ~f:(fun t ->
    if Parameter_name.equal t.name name then Some t.value else None)
;;

let find_name_exn ts name =
  match find_name ts name with
  | Some x -> x
  | None ->
    raise_s
      [%message
        "couldn't find parameter" (name : Parameter_name.t) ~parameters:(ts : t list)]
;;

let is_subset ts1 ts2 =
  List.for_all ts1 ~f:(fun t1 ->
    match find_name ts2 t1.name with
    | Some v2 -> Value.equal t1.value v2
    | None -> false)
;;

let sort_by_name ts =
  List.sort ts ~compare:(fun t1 t2 -> Parameter_name.compare t1.name t2.name)
;;
