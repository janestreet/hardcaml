open! Core0
module I = Int

module type S = Uid_builder_intf.S

module Make () = struct
  module T = struct
    type t = I.t [@@deriving bin_io, compare ~localize, sexp]

    (* We need a hash function compatible with native code and javascript. Currently the
       only type which allows this is [Int64]. So we perform a conversion to int64 here,
       and reach out directly to the (unboxed) hash_fold function in base to implement it.
       This allows zero alloc operation even in fast-build mode. *)

    external fold_int64
      :  Hash.state
      -> (int64[@unboxed])
      -> Hash.state
      = "Base_internalhash_fold_int64" "Base_internalhash_fold_int64_unboxed"
    [@@noalloc]

    let hash_fold_t state t = fold_int64 state (Int64.of_int_exn t)
    let hash t = Hash.get_hash_value (hash_fold_t (Hash.alloc ()) t)
  end

  include T
  include Comparator.Make (T)
  include Hashable.Make_binable (T)

  include%template Comparable.Make [@mode local] (T)

  let zero = I.of_int 0
  let one = I.of_int 1
  let to_int t = I.to_int_exn t
  let to_string t = I.to_string t

  let generator () =
    let id = ref one in
    let new_id () =
      let x = !id in
      (id := I.(!id + one));
      x
    in
    let reset_id () = id := one in
    `New new_id, `Reset reset_id
  ;;

  module Expert = struct
    let of_int a = a
  end
end
