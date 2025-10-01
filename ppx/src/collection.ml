open Ppxlib

type t =
  | List
  | Array
  | Iarray

let to_string = function
  | List -> "list"
  | Array -> "array"
  | Iarray -> "iarray"
;;

let of_string_opt = function
  | "list" -> Some List
  | "array" -> Some Array
  | "iarray" -> Some Iarray
  | _ -> None
;;

let is_supported s = of_string_opt s |> Option.is_some

let of_string s =
  of_string_opt s |> Base.Option.value_exn ~message:("Unsupported collection type " ^ s)
;;

let to_module_expr t loc =
  match t with
  | List -> [%expr List]
  | Array -> [%expr Array]
  | Iarray -> [%expr Iarray]
;;

let map t ~(iter_or_map : Iter_or_map.t) loc =
  match t, iter_or_map with
  | List, Iter -> [%expr Ppx_hardcaml_runtime0.List.iter]
  | List, Map -> [%expr Ppx_hardcaml_runtime0.List.map]
  | Array, Iter -> [%expr Ppx_hardcaml_runtime0.Array.iter]
  | Array, Map -> [%expr Ppx_hardcaml_runtime0.Array.map]
  | Iarray, Iter -> [%expr Ppx_hardcaml_runtime0.Iarray.iter]
  | Iarray, Map -> [%expr Ppx_hardcaml_runtime0.Iarray.map]
;;

let mapi t ~(iter_or_map : Iter_or_map.t) loc =
  match t, iter_or_map with
  | List, Iter -> [%expr Ppx_hardcaml_runtime0.List.iteri]
  | List, Map -> [%expr Ppx_hardcaml_runtime0.List.mapi]
  | Array, Iter -> [%expr Ppx_hardcaml_runtime0.Array.iteri]
  | Array, Map -> [%expr Ppx_hardcaml_runtime0.Array.mapi]
  | Iarray, Iter -> [%expr Ppx_hardcaml_runtime0.Iarray.iteri]
  | Iarray, Map -> [%expr Ppx_hardcaml_runtime0.Iarray.mapi]
;;

let map2 t ~(iter_or_map : Iter_or_map.t) loc =
  match t, iter_or_map with
  | List, Iter -> [%expr Ppx_hardcaml_runtime0.List.iter2_exn]
  | List, Map -> [%expr Ppx_hardcaml_runtime0.List.map2_exn]
  | Array, Iter -> [%expr Ppx_hardcaml_runtime0.Array.iter2_exn]
  | Array, Map -> [%expr Ppx_hardcaml_runtime0.Array.map2_exn]
  | Iarray, Iter -> [%expr Ppx_hardcaml_runtime0.Iarray.iter2_exn]
  | Iarray, Map -> [%expr Ppx_hardcaml_runtime0.Iarray.map2_exn]
;;

let init t loc =
  match t with
  | List -> [%expr Ppx_hardcaml_runtime0.List.init]
  | Array -> [%expr Ppx_hardcaml_runtime0.Array.init]
  | Iarray -> [%expr Ppx_hardcaml_runtime0.Iarray.init]
;;

let to_list t loc =
  match t with
  | List -> [%expr Ppx_hardcaml_runtime0.List.to_list]
  | Array -> [%expr Ppx_hardcaml_runtime0.Array.to_list]
  | Iarray -> [%expr Ppx_hardcaml_runtime0.Iarray.to_list]
;;
