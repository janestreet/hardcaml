open! Import

type t = { by_name : (string, Combinational_op.t) Hashtbl.t } [@@deriving sexp_of]

let create () = { by_name = Hashtbl.create (module String) }
let find t ~(name : string) = Hashtbl.find t.by_name name

let insert t (op : Combinational_op.t) =
  let name = Combinational_op.name op in
  if Hashtbl.mem t.by_name name
  then
    raise_s
      [%message
        "A [Combinational_op] of the same name already exists in the database"
          (name : string)];
  Hashtbl.add_exn t.by_name ~key:name ~data:op
;;

let fold t ~init ~f = Hashtbl.fold t.by_name ~init ~f:(fun ~key:_ ~data a -> f a data)
let iter t ~f = Hashtbl.iter t.by_name ~f

let concat ts =
  let result = create () in
  List.iter ts ~f:(fun t -> iter t ~f:(fun op -> insert result op));
  result
;;
