open! Import

include Interface_intf

module Create_fn (I : S) (O : S) = struct
  type 'a t = 'a I.t -> 'a O.t

  let sexp_of_t _ _ =
    [%message
      ""
        ~inputs: (I.t : (string * int) I.t)
        ~outputs:(O.t : (string * int) O.t)]
end

module Make (X : Pre) : S with type 'a t := 'a X.t = struct

  include X

  let port_names  = map t ~f:fst
  let port_widths = map t ~f:snd

  let to_list_rev x = to_list x |> List.rev

  let to_alist x = to_list (map2 port_names x ~f:(fun name x -> name, x))

  let of_alist x =
    map port_names ~f:(fun name ->
      match List.Assoc.find x name ~equal:String.equal with
      | Some x -> x
      | None ->
        raise_s [%message
          "[Interface_extended.of_alist] Field not found in interface"
            ~missing_field_name:(name : string)
            ~input:(x : (string * _) list)
            ~interface:(port_widths : int X.t)])

  let zip a b =
    map2 a b ~f:(fun a b -> a, b)
  let zip3 a b c =
    map2 (zip a b) c ~f:(fun (a, b) c -> a, b, c)
  let zip4 a b c d =
    map2 (zip a b) (zip c d) ~f:(fun (a, b) (c, d) -> a, b, c, d)
  let zip5 a b c d e =
    map2 (zip3 a b c) (zip d e) ~f:(fun (a, b, c) (d, e) -> a, b, c, d, e)

  let map3 a b c     ~f = map ~f:(fun (a, b, c)       -> f a b c)     (zip3 a b c)
  let map4 a b c d   ~f = map ~f:(fun (a, b, c, d)    -> f a b c d)   (zip4 a b c d)
  let map5 a b c d e ~f = map ~f:(fun (a, b, c, d, e) -> f a b c d e) (zip5 a b c d e)

  let iter3 a b c     ~f = ignore @@ map3 ~f a b c
  let iter4 a b c d   ~f = ignore @@ map4 ~f a b c d
  let iter5 a b c d e ~f = ignore @@ map5 ~f a b c d e

  let equal equal_a t1 t2 =
    With_return.with_return (fun r ->
      iter2 t1 t2 ~f:(fun a1 a2 -> if not (equal_a a1 a2) then r.return false);
      true)

  let fold a ~init ~f =
    let init = ref init in
    iter a ~f:(fun a -> init := f !init a);
    !init

  let fold2 a b ~init ~f = fold (zip a b) ~init ~f:(fun c (a, b) -> f c a b)

  let offsets ?(rev = false) () =
    let rec loop fields ~offset =
      match fields with
      | [] -> []
      | (name, width) :: fields ->
        (name, offset) :: loop fields ~offset:(offset + width) in
    loop (if rev then to_list_rev t else to_list t) ~offset:0
    |> of_alist

  let of_interface_list ts =
    List.fold (List.rev ts)
      ~init:(map t ~f:(fun _ -> []))
      ~f:(fun ac t -> map2 t ac ~f:(fun h t -> h :: t))

  let to_interface_list t =
    let lengths = map t ~f:List.length in
    let distinct_lengths = fold lengths ~init:(Set.empty (module Int)) ~f:Set.add in
    match Set.to_list distinct_lengths with
    | [] -> []
    | [ length ] ->
      let rec loop length t =
        if length = 0
        then []
        else map t ~f:List.hd_exn :: loop (length - 1) (map t ~f:List.tl_exn) in
      loop length t
    | _ ->
      raise_s [%message
        "[Interface_extended.to_interface_list] field list lengths must be the same"
          (lengths : int t)]

  module Make_comb (Comb : Comb.S) = struct

    type comb = Comb.t [@@deriving sexp_of]

    type t = Comb.t X.t [@@deriving sexp_of]

    let widths t = map t ~f:Comb.width

    let const i = map port_widths ~f:(fun b -> Comb.consti b i)

    let consts i = map2 port_widths i ~f:Comb.consti

    let pack ?(rev=false) t =
      if rev
      then to_list     t |> Comb.concat
      else to_list_rev t |> Comb.concat

    let unpack ?(rev=false) comb =
      let rec loop fields ~offset =
        match fields with
        | [] -> []
        | (name, width) :: fields ->
          (name, Comb.select comb (offset + width - 1) offset)
          :: loop fields ~offset:(offset + width)
      in
      loop (if rev then to_list_rev t else to_list t) ~offset:0
      |> of_alist

    let mux  s l   = map ~f:(Comb.mux s) (of_interface_list l)
    let mux2 s h l = mux s [ l; h ]

    let concat l = map ~f:Comb.concat (of_interface_list l)
  end

  module type Comb = Comb with type 'a interface := 'a t

  module Of_bits = Make_comb (Bits)

  module Of_signal = struct
    include Make_comb (Signal)

    let assign t1 t2 = iter2 t1 t2 ~f:Signal.assign
    let ( <== ) = assign

    let wires ?(named = false) ?from () =
      let wires =
        match from with
        | None -> map port_widths ~f:Signal.wire
        | Some x -> map x ~f:Signal.wireof
      in
      if named
      then map2 wires port_names ~f:Signal.(--)
      else wires

    let inputs () = wires () ~named:true

    let outputs t = wires () ~from:t ~named:true
  end
end

module Empty = struct
  type 'a t = None [@@deriving sexp_of]
  include Make (struct
      type nonrec 'a t = 'a t [@@deriving sexp_of]
      let t = None
      let iter ~f:_ _ = ()
      let iter2 ~f:_ _ _ = ()
      let map ~f:_ _ = None
      let map2 ~f:_ _ _ = None
      let to_list _ = []
    end)
end
