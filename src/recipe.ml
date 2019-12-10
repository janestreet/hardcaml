open! Import
open Signal
open! Recipe_intf

type var = int
type inp = Signal.t * Signal.t (* enable * value *)

module VMap = Map.Make (Int)

type env =
  { freshId : var
  ; writerInps : inp list VMap.t
  ; outs : Signal.t VMap.t
  ; clock : Signal.t
  ; enable : Signal.t
  }

type 'a t = Recipe of (Signal.t -> env -> Signal.t * env * 'a)

include Monad.Make (struct
    type nonrec 'a t = 'a t

    let return a = Recipe (fun start env -> start, env, a)

    let bind (Recipe m) ~f =
      Recipe
        (fun start env ->
           let fin0, env0, a = m start env in
           let (Recipe f) = f a in
           let fin1, env1, b = f fin0 env0 in
           fin1, env1, b)
    ;;

    let map = `Define_using_bind
  end)

let delay ~env ~clear_to d =
  let clock = env.clock in
  let enable = env.enable in
  reg (Reg_spec.override (Reg_spec.create () ~clock) ~clear_to) ~enable d
;;

let delay_with_enable ~env ~clear_to ~enable d =
  let clock = env.clock in
  reg (Reg_spec.override (Reg_spec.create () ~clock) ~clear_to) ~enable d
;;

let delayFb ~clock ~enable ~clear_to f =
  reg_fb
    (Reg_spec.override (Reg_spec.create () ~clock) ~clear_to)
    ~enable
    ~w:(width clear_to)
    f
;;

let setReset ~clock ~enable s r =
  delayFb ~clock ~enable ~clear_to:gnd (fun q -> s |: q &: ~:r)
;;

let skip =
  Recipe (fun start env -> delay ~env ~clear_to:gnd start -- "skip_fin", env, ())
;;

let rec wait = function
  | 0 -> return ()
  | n -> skip >>= fun _ -> wait (n - 1)
;;

let gen_par_fin ~clock ~enable ~comb_fin fin' fin =
  let fin = setReset ~clock ~enable fin' fin in
  if comb_fin then fin' |: fin else fin
;;

let par2 ?(comb_fin = true) (Recipe p) (Recipe q) =
  Recipe
    (fun start env ->
       let fin0, env0, a = p start env in
       let fin1, env1, b = q start env0 in
       let fin = wire 1 in
       fin
       <== (gen_par_fin ~clock:env.clock ~enable:env.enable ~comb_fin fin0 fin
            &: gen_par_fin ~clock:env.clock ~enable:env.enable ~comb_fin fin1 fin);
       fin, env1, (a, b))
;;

let ( ||| ) p q = par2 ~comb_fin:true p q

let par ?(comb_fin = true) r =
  Recipe
    (fun start env ->
       let finl, env, al =
         List.fold r ~init:([], env, []) ~f:(fun (finl, env, al) (Recipe r) ->
           let fin, env, a = r start env in
           fin :: finl, env, a :: al)
       in
       let par_fin = wire 1 -- "par_fin" in
       let par_fin =
         reduce
           ~f:( &: )
           (List.map finl ~f:(fun fin' ->
              gen_par_fin ~clock:env.clock ~enable:env.enable ~comb_fin fin' par_fin))
       in
       par_fin, env, List.rev al)
;;

let cond c (Recipe p) (Recipe q) =
  Recipe
    (fun start env ->
       let fin0, _, _ = p (start &: c) env in
       let fin1, env1, _ = q (start &: ~:c) env in
       (fin0 |: fin1) -- "cond_fin", env1, ())
;;

let iter c (Recipe p) =
  Recipe
    (fun start env ->
       let ready = wire 1 -- "iter_ready" in
       let fin, env', b = p ((c &: ready) -- "iter_start") env in
       ready <== (start |: fin);
       (~:c &: ready) -- "iter_fin", env', b)
;;

let forever p = iter vdd p
let wait_while a = iter a skip
let wait_until a = iter ~:a skip

let follow ~clock ~enable start (Recipe r) =
  let initialEnv =
    { freshId = 0; writerInps = VMap.empty; outs = VMap.empty; enable; clock }
  in
  let fin, env, a = r start initialEnv in
  (* connect writerInps to outs *)
  Map.iteri env.outs ~f:(fun ~key:v ~data:o ->
    try
      let inps = Map.find_exn env.writerInps v in
      let enable = reduce ~f:( |: ) (List.map inps ~f:fst) in
      let value =
        reduce ~f:( |: ) (List.map inps ~f:(fun (e, v) -> mux2 e v (zero (width v))))
      in
      o <== delay_with_enable ~env ~clear_to:(zero (width o)) ~enable value
    with
    | _ ->
      (* this can lead to combinatorial loops, so perhaps an exception would be better
      *)
      printf "unassigned var; defaulting to zero\n";
      o <== zero (width o)
      (* unassigned variable *));
  fin, a
;;

let createVar env a =
  let v = env.freshId in
  v, { env with freshId = v + 1; outs = Map.set env.outs ~key:v ~data:a }
;;

let ofList al =
  List.fold al ~init:VMap.empty ~f:(fun m (k, v) -> Map.set m ~key:k ~data:v)
;;

let addInps env al =
  let merge ~key:_ = function
    | `Left a | `Right a -> Some a
    | `Both (a, b) -> Some (a @ b)
  in
  { env with writerInps = Map.merge (ofList al) env.writerInps ~f:merge }
;;

let new_var ?name n =
  Recipe
    (fun start env ->
       let out =
         match name with
         | None -> wire n
         | Some x -> wire n -- x
       in
       let v, env' = createVar env out in
       start, env', v)
;;

let read_var v = Recipe (fun start env -> start, env, Map.find_exn env.outs v)

let assign al =
  Recipe
    (fun start env ->
       let al' = List.map al ~f:(fun (a, b) -> a, [ start, b ]) in
       delay ~env ~clear_to:gnd start, addInps env al', ())
;;

let write_var v a = assign [ v, a ]
let modify_var f v = read_var v >>= fun a -> write_var v (f a)
let rewrite_var f v w = read_var v >>= fun a -> write_var w (f a)

module type Same = Same with type var := var with type 'a recipe := 'a t

module Same (X : Interface.Pre) = struct
  type 'a same = 'a X.t

  let smap ~f t = X.map ~f t
  let szip x y = X.(to_list (map2 ~f:(fun a b -> a, b) x y))

  let read a =
    Recipe (fun start env -> start, env, smap ~f:(fun a -> Map.find_exn env.outs a) a)
  ;;

  let rewrite f a b = read a >>= fun x -> assign (szip b (f x))
  let apply ~f a = rewrite f a a
  let set a b = rewrite (fun _ -> b) a a
  let if_ f a ~then_ ~else_ = read a >>= fun b -> cond (f b) then_ else_
  let while_ f a ~do_ = read a >>= fun b -> iter (f b) do_

  let new_var () =
    let mkvar n b l =
      new_var ~name:("new_var_" ^ n) b >>= fun v -> return ((n, v) :: l)
    in
    let rec f m l =
      match m, l with
      | None, [] -> failwith "Same.new_var: no elements"
      | None, (n, b) :: t -> f (Some (mkvar n b [])) t
      | Some m, (n, b) :: t -> f (Some (m >>= mkvar n b)) t
      | Some m, [] -> m
    in
    let m = f None X.(to_list t) in
    m
    >>= fun l ->
    return
      (X.map
         ~f:(fun (n, _) ->
           try
             List.Assoc.find_exn l n ~equal:String.equal
           with
           | Not_found_s _ | Caml.Not_found -> failwith ("Not_found " ^ n))
         X.t)
  ;;
end

module SVar = Same (struct
    type 'a t = 'a [@@deriving sexp_of]

    let t = "var", 0
    let iter a ~f = f a
    let iter2 a b ~f = f a b
    let map a ~f = f a
    let map2 a b ~f = f a b
    let to_list a = [ a ]
  end)

(* not so sure these are particularly useful; interfaces can do the job better *)
module SList = Same (struct
    type 'a t = 'a list [@@deriving sexp_of]

    let t = []
    let iter l ~f = List.iter l ~f
    let iter2 l0 l1 ~f = List.iter2_exn l0 l1 ~f
    let map l ~f = List.map l ~f
    let map2 l0 l1 ~f = List.map2_exn l0 l1 ~f
    let to_list a = a
  end)

module SArray = Same (struct
    type 'a t = 'a array [@@deriving sexp_of]

    let t = [||]
    let iter t ~f = Array.iter t ~f
    let iter2 t1 t2 ~f = Array.iter2_exn t1 t2 ~f
    let map a ~f = Array.map a ~f
    let map2 a b ~f = Array.init (Array.length a) ~f:(fun i -> f a.(i) b.(i))
    let to_list = Array.to_list
  end)

module STuple2 = Same (struct
    type 'a t = 'a * 'a [@@deriving sexp_of]

    let t = ("a", 0), ("b", 0)

    let iter (a, b) ~f =
      f a;
      f b
    ;;

    let iter2 (a, b) (c, d) ~f =
      f a c;
      f b d
    ;;

    let map (a, b) ~f = f a, f b
    let map2 (a, b) (c, d) ~f = f a c, f b d
    let to_list (a, b) = [ a; b ]
  end)

module STuple3 = Same (struct
    type 'a t = 'a * 'a * 'a [@@deriving sexp_of]

    let t = ("a", 0), ("b", 0), ("c", 0)

    let iter (a, b, c) ~f =
      f a;
      f b;
      f c
    ;;

    let iter2 (a, b, c) (d, e, f) ~f:fn =
      fn a d;
      fn b e;
      fn c f
    ;;

    let map (a, b, c) ~f = f a, f b, f c
    let map2 (a, b, c) (d, e, f) ~f:fn = fn a d, fn b e, fn c f
    let to_list (a, b, c) = [ a; b; c ]
  end)
