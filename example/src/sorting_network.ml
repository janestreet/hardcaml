open! Import

module Min_or_max = struct
  type t = Min | Max [@@deriving sexp_of]
end

module Min_max = struct
  type 'a t = { min : 'a; max : 'a } [@@deriving sexp_of]

  let get t (min_or_max : Min_or_max.t) =
    match min_or_max with
    | Min -> t.min
    | Max -> t.max
end

open Min_max

(* split list into 2 halves *)
let halve l = List.split_n l (List.length l / 2)

type 'a compare_and_swap = ('a -> 'a -> 'a Min_max.t)

module Make
    (S : sig
       type t
       val compare_and_swap : t compare_and_swap
     end) = struct

  open S

  (* Bitonic sort.

     A bitonic sequence is one which changes at most twice.  The following are all
     bitonic:

     0001110
     1110011
     0001111
     1111111

     The bitonic merge operation takes a bitonic sequence and produces 2 new sequences
     of half the size each.  Both are bitonic, and all elements in the 1st sequence
     are less than or equal to all elements in the 2nd sequence.  This is performed
     recursively until the base case of 1 element which is trivially sorted.  Coming back
     up the recursion the halves are concatenated and the result is thus fully sorted.

     The bitonic sort procedure takes an unsorted list in, divides it into 2 and recurses
     to the base case.  The lower half is sorted in ascending order, while the upper half
     is sorted in descending order.  Concatentation of these lists lead to a bitonic
     sequence that can be fully sorted with the bitonic merge operation. *)

  type dirn = Up | Down

  let sort_bitonic dirn bitonic =
    let rec loop bitonic =
      match bitonic with
      | [ _ ] -> bitonic
      | _ ->
        let l0, l1 = halve bitonic in
        let pairs =
          List.map2_exn l0 l1 ~f:(fun a b ->
            let { Min_max. min; max } = compare_and_swap a b in
            match dirn with
            | Up   -> min, max
            | Down -> max, min) in
        (* If [dirn = Up], then every element in [bitonic0] is [<=] every element in
           [bitonic1].  If [dirn = Down], then similarly but with [>=]. *)
        let bitonic0 = List.map pairs ~f:fst in
        let bitonic1 = List.map pairs ~f:snd in
        let sorted0 = loop bitonic0 in
        let sorted1 = loop bitonic1 in
        sorted0 @ sorted1 in
    loop bitonic

  let rec sort' dirn l =
    match l with
    | [ _ ] -> l
    | _ ->
      let l0, l1 = halve l in
      let up = sort' Up l0 in (* [up] looks like one of: 0, 1, 01 *)
      let down = sort' Down l1 in (* [down looks like one of: 0, 1, 10 *)
      let bitonic = up @ down in (* [bitonic] looks like one of: 0, 1, 01, 10, 010 *)
      sort_bitonic dirn bitonic

  let bitonic_sort = sort' Up

  (* Odd-even merge sort

     Consider a sequence whose first half and second half are sorted.  Now take the even
     and odd indices to make two new sequences of half the size.  Both of these new
     sequences will also be made up of two sorted halves.

     The base case of 2 elements is trivially sorted with a compare.  As we come back up
     the recursion the even and odd elements are now sorted (by induction).  The merge
     operation will take these two sequences and apply compares between indices [i, i+1]
     for all [1,3,5,...,n-3].  The trick to note is that the odd sequence can only have
     2 more 1's than the even elements (that is, at this step, no more than 1 swap will
     actually be performed). *)

  let odd_even l =
    let rec loop l e o =
      match l with
      | [] -> e, o
      | e' :: o' :: t -> loop t (e' :: e) (o' :: o)
      | _ -> failwith "expecting even length list"
    in
    let e, o = loop l [] [] in
    List.rev e, List.rev o

  let rec compare_list o e =
    match o, e with
    | [ o1 ], [] -> [ o1 ]
    | o1 :: ot, e2 :: et ->
      let { min; max } = compare_and_swap o1 e2 in
      min :: max :: compare_list ot et
    | _ -> failwith "compare list failed"

  (* [merge] takes a list that is the concatenation of two equal-length sorted lists and
     returns a list that is completely sorted. *)
  let rec merge l =
    match l with
    | [ a0; a1 ] ->
      let { min; max } = compare_and_swap a0 a1 in
      [ min; max ]
    | _ ->
      let e, o = odd_even l in
      let e, o = merge e, merge o in
      (* For an explanation of why this step produces sorted output, see Figure 1 of:

         http://www.iti.fh-flensburg.de/lang/algorithmen/sortieren/networks/oemen.htm

         The operation in (d) is clearly a correct sort.  The shape of (b) and the comment
         "the right column can have at most two more 1's that the left" is the key thing
         to why this works. *)
      List.hd_exn e :: compare_list o (List.tl_exn e)

  let rec odd_even_merge_sort l =
    if List.length l <= 1
    then l
    else (
      let l, h = halve l in
      merge (odd_even_merge_sort l @ odd_even_merge_sort h))
end

module Config = struct
  type t =
    | Bitonic_sort
    | Odd_even_merge_sort
  [@@deriving enumerate, sexp_of]
end

let create (type a) (config : Config.t) compare_and_swap (l : a list) =
  let module S = Make (struct type t = a let compare_and_swap = compare_and_swap end) in
  if not (Int.is_pow2 (List.length l))
  then raise_s [%message
         "Sorting networks require their input length to be a power of 2"
           (config : Config.t)
           ~length:(List.length l : int)];
  let create =
    match config with
    | Bitonic_sort -> S.bitonic_sort
    | Odd_even_merge_sort -> S.odd_even_merge_sort in
  create l
