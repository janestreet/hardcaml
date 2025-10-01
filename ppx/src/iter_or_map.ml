open Ppxlib

type t =
  | Iter
  | Map

let name = function
  | Iter -> "iter"
  | Map -> "map"
;;

let name2 = function
  | Iter -> "iter2"
  | Map -> "map2"
;;

let option_map t loc =
  match t with
  | Iter -> [%expr Base.Option.iter]
  | Map -> [%expr Base.Option.map]
;;

let option_map2_exn t loc =
  match t with
  | Iter -> [%expr Ppx_hardcaml_runtime0.option_iter2_exn]
  | Map -> [%expr Ppx_hardcaml_runtime0.option_map2_exn]
;;
