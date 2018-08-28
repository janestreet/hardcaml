open! Import
open Signal
open! Recipe_intf

let clock = input "clock" 1
let enable = input "enable" 1

type var = int
type inp = Signal.t * Signal.t (* enable * value *)
module VMap = Map.Make (Int)
type env =
  { freshId    : var
  ; writerInps : inp list VMap.t
  ; outs       : Signal.t VMap.t }
type 'a recipe = Recipe of (Signal.t -> env -> (Signal.t * env * 'a))

let delay clear_to d =
  reg (Reg_spec.override (Reg_spec.create () ~clock:clock) ~clear_to) ~enable d

let delayEn clear_to enable d =
  reg (Reg_spec.override (Reg_spec.create () ~clock:clock) ~clear_to) ~enable d

let delayFb clear_to f =
  reg_fb
    (Reg_spec.override (Reg_spec.create () ~clock:clock) ~clear_to)
    ~enable
    ~w:(width clear_to)
    f

let setReset s r = delayFb gnd (fun q -> (s |: q) &: (~: r))

module Monad = struct

  let return a =
    Recipe (fun start env -> (start, env, a))

  let bind (Recipe m) f = Recipe (fun start env ->
    let (fin0, env0, a) = m start env in
    let Recipe f = f a in
    let (fin1, env1, b) = f fin0 env0 in
    (fin1, env1, b))

  let (>>=) = bind

  let (>>) m f = bind m (fun _ -> f)
end

open Monad

let skip = Recipe (fun start env -> (delay gnd start) -- "skip_fin", env, ())

let rec wait = function
  | 0 -> return ()
  | n -> skip >> wait (n-1)

let gen_par_fin comb_fin fin' fin =
  let fin = setReset fin' fin in
  if comb_fin then fin' |: fin else fin

let par2 ?(comb_fin=true) (Recipe p) (Recipe q) = Recipe (fun start env ->
  let (fin0, env0, a) = p start env in
  let (fin1, env1, b) = q start env0 in
  let fin = wire 1 in
  fin <== (gen_par_fin comb_fin fin0 fin &: gen_par_fin comb_fin fin1 fin);
  (fin, env1, (a, b)))

let (|||) p q = par2 ~comb_fin:true p q

let par ?(comb_fin=true) r = Recipe (fun start env ->
  let finl, env, al =
    List.fold r ~init:([], env, []) ~f:(fun (finl, env, al) (Recipe r) ->
      let fin, env, a = r start env in
      (fin :: finl, env, a :: al))
  in
  let fin = wire 1 -- "par_fin" in
  fin <== reduce ~f:(&:) (List.map finl ~f:(fun fin' -> gen_par_fin comb_fin fin' fin));
  (fin, env, List.rev al))

let cond c (Recipe p) (Recipe q) = Recipe (fun start env ->
  let (fin0, _, _) = p (start &: c) env in
  let (fin1, env1, _) = q (start &: (~: c)) env in
  ((fin0 |: fin1) -- "cond_fin", env1, ()))

let iter c (Recipe p) = Recipe (fun start env ->
  let ready = wire 1 -- "iter_ready" in
  let (fin, env', b) = p ((c &: ready) -- "iter_start") env in
  ready <== (start |: fin);
  (((~: c) &: ready) -- "iter_fin", env', b))

let forever p = iter vdd p
let waitWhile a = iter a skip
let waitUntil a = iter (~: a) skip

let follow start (Recipe r) =
  let initialEnv =
    { freshId    = 0
    ; writerInps = VMap.empty
    ; outs       = VMap.empty }
  in
  let fin, env, a = r start initialEnv in
  (* connect writerInps to outs *)
  Map.iteri env.outs ~f:(fun ~key:v ~data:o ->
    try
      let inps = Map.find_exn env.writerInps v in
      let enable = reduce ~f:(|:) (List.map inps ~f:fst) in
      let value =
        reduce ~f:(|:) (List.map inps ~f:(fun (e, v) -> mux2 e v (zero (width v)))) in
      o <== (delayEn (zero (width o)) enable value)
    with _ ->
      (* this can lead to combinatorial loops, so perhaps an exception would be better
      *)
      printf "unassigned var; defaulting to zero\n";
      o <== (zero (width o)) (* unassigned variable *));
  fin, a

let createVar env a =
  let v = env.freshId in
  v, { env with freshId = v+1; outs = Map.set env.outs ~key:v ~data:a }

let ofList al =
  List.fold al ~init:VMap.empty ~f:(fun m (k, v) -> Map.set m ~key:k ~data:v)

let addInps env al =
  let merge ~key:_ = function
    | `Left a | `Right a -> Some a
    | `Both (a, b) -> Some (a @ b)
  in
  { env with writerInps = Map.merge (ofList al) env.writerInps ~f:merge }

let newVar ?name n = Recipe (fun start env ->
  let out = match name with None -> wire n | Some x -> (wire n) -- x in
  let v, env' = createVar env out in
  (start, env', v))

let readVar v = Recipe (fun start env -> (start, env, Map.find_exn env.outs v))

let assign al = Recipe (fun start env ->
  let al' = List.map al ~f:(fun (a, b) -> a, [ start, b ]) in
  (delay gnd start, addInps env al', ()))

let writeVar v a = assign [ v, a ]

let modifyVar f v = readVar v >>= fun a -> writeVar v (f a)

let rewriteVar f v w = readVar v >>= fun a -> writeVar w (f a)

module type Same = Same
  with type var := var
  with type 'a recipe := 'a recipe

module Same (X : Interface.Pre) = struct
  type 'a same = 'a X.t
  let smap ~f t = X.map ~f t
  let szip x y = X.(to_list (map2 ~f:(fun a b -> a, b) x y))
  let read a =
    Recipe (fun start env -> (start, env, smap ~f:(fun a -> Map.find_exn env.outs a) a))
  let rewrite f a b = read a >>= fun x -> assign (szip b (f x))
  let apply f a  = rewrite f a a
  let set a b = rewrite (fun _ -> b) a a
  let ifte f a p q = read a >>= fun b -> cond (f b) p q
  let while_ f a p = read a >>= fun b -> iter (f b) p

  let newVar () =
    let mkvar n b l = newVar ~name:("newVar_" ^ n) b >>= fun v -> return ((n, v) :: l) in
    let rec f m l =
      match m, l with
      | None, [] -> failwith "Same.newVar: no elements"
      | None, (n, b) :: t ->
        f (Some (mkvar n b [])) t
      | Some m, (n, b) :: t ->
        f (Some (m >>= mkvar n b)) t
      | Some m, [] -> m
    in
    let m = f None X.(to_list t) in
    m >>= fun l -> return (X.map ~f:(fun (n, _) ->
      try List.Assoc.find_exn l n ~equal:String.equal
      with Not_found_s _ | Caml.Not_found -> failwith ("Not_found " ^ n)) X.t)
end

module SVar = Same (struct
    type 'a t = 'a [@@deriving sexp_of]
    let t = "var", 0
    let iter ~f a = f a
    let iter2 ~f a b = f a b
    let map ~f a = f a
    let map2 ~f a b = f a b
    let to_list a = [ a ]
  end)

(* not so sure these are particularly useful; interfaces can do the job better *)
module SList = Same (struct
    type 'a t = 'a list [@@deriving sexp_of]
    let t = []
    let iter ~f l = List.iter l ~f
    let iter2 ~f l0 l1 = List.iter2_exn l0 l1 ~f
    let map ~f l = List.map l ~f
    let map2 ~f l0 l1 = List.map2_exn l0 l1 ~f
    let to_list a = a
  end)

module SArray = Same (struct
    type 'a t = 'a array [@@deriving sexp_of]
    let t = [||]
    let iter ~f t = Array.iter t ~f
    let iter2 ~f t1 t2 = Array.iter2_exn t1 t2 ~f
    let map ~f a = Array.map a ~f
    let map2 ~f a b = Array.init (Array.length a) ~f:(fun i -> f a.(i) b.(i))
    let to_list = Array.to_list
  end)

module STuple2 = Same (struct
    type 'a t = 'a * 'a [@@deriving sexp_of]
    let t = ("a", 0), ("b", 0)
    let iter ~f (a, b) = f a; f b
    let iter2 ~f (a, b) (c, d) = f a c; f b d
    let map ~f (a, b) = (f a, f b)
    let map2 ~f (a, b) (c, d) = (f a c, f b d)
    let to_list (a, b) = [ a; b ]
  end)

module STuple3 = Same (struct
    type 'a t = 'a * 'a * 'a [@@deriving sexp_of]
    let t = ("a", 0), ("b", 0), ("c", 0)
    let iter ~f (a, b, c) = f a; f b; f c
    let iter2 ~f:f' (a, b, c) (d, e, f) = f' a d; f' b e; f' c f
    let map ~f (a, b, c) = (f a, f b, f c)
    let map2 ~f:f' (a, b, c) (d, e, f) = (f' a d, f' b e, f' c f)
    let to_list (a, b, c) = [ a; b; c ]
  end)
