open! Import
module Uid = Signal.Uid

let `New new_id, _ = Uid.generator ()

let%bench_fun "hash" =
  let uid = new_id () in
  fun () -> Uid.hash uid
;;
