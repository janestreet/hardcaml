open! Core0

module Name = struct
  module T = struct
    type t = string [@@deriving bin_io, compare ~localize, sexp, hash]
  end

  include T
  include Comparable.Make_plain (T)

  let of_string n = n
  let to_string n = n
end

type t =
  { name : Name.t
  ; period : int
  }
[@@deriving sexp_of, compare]

let create ~name ~period = { name; period }
let create_list ls = List.map ls ~f:(fun (name, period) -> create ~name ~period)
let aligned { period; _ } ~cycle = cycle mod period = 0

type domain = t [@@deriving sexp_of]

type indexed =
  { domain : domain
  ; index : int
  }
[@@deriving sexp_of, fields ~getters]

module Group = struct
  type t =
    { keys : indexed iarray
    ; map : indexed Name.Map.t
    ; size : int
    }
  [@@deriving sexp_of, fields ~getters]

  let create elements =
    let size = List.length elements in
    let keys = List.mapi elements ~f:(fun index domain -> { domain; index }) in
    match
      List.map keys ~f:(fun indexed -> indexed.domain.name, indexed)
      |> Map.of_alist (module Name)
    with
    | `Ok map -> `Ok { keys = Iarray.of_list keys; map; size }
    | `Duplicate_key key -> `Duplicate_key key
  ;;

  let create_exn elements =
    match create elements with
    | `Ok t -> t
    | `Duplicate_key key -> raise_s [%message "Duplicate key" (key : Name.t)]
  ;;

  let get t name = Map.find t.map name
  let elements t = t.keys
end

module Table = struct
  type 'a t = 'a array [@@deriving sexp_of]

  let create (group : Group.t) default = Array.create ~len:group.size default

  let init (group : Group.t) ~f =
    Array.init group.size ~f:(fun i -> f (Iarray.get group.keys i))
  ;;

  let set t ~key ~data = Array.set t key.index data
  let get t key = Array.get t key.index
  let map t ~f = Array.map t ~f
  let fold_map t ~init ~f = Array.fold_map t ~init ~f
  let fold t ~init ~f = Array.fold t ~init ~f
  let map_inplace t ~f = Array.map_inplace t ~f
end

module Set = struct
  type t =
    { keys : indexed iarray
    ; data : bool Table.t
    }
  [@@deriving sexp_of]

  let create group ~default =
    { keys = Group.keys group; data = Table.create group default }
  ;;

  let to_string t =
    Iarray.filter t.keys ~f:(fun key -> Table.get t.data key)
    |> Iarray.map ~f:(fun key -> key.domain.name)
    |> Iarray.to_list
    |> List.to_string ~f:Fn.id
  ;;

  let clear t = Array.map_inplace t.data ~f:(Fn.const false)
  let add t key = Table.set t.data ~key ~data:true
  let remove t key = Table.set t.data ~key ~data:false
  let mem t key = Table.get t.data key
  let iter (t : t) ~f = Iarray.iter t.keys ~f:(fun key -> if mem t key then f key)
end
