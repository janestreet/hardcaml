open! Import

(** Determines the cordic update equations *)
module System = struct
  type t =
    | Circular
    | Linear
    | Hyperbolic
  [@@deriving variants]
end

(** iteration mode *)
module Mode = struct
  type t =
    | Rotation
    | Vectoring
    | Inverse
end

let iter ~iterations ~init ~f =
  let rec loop i ih k acc =
    if i=iterations
    then acc
    else
      let acc = f ~i ~ih acc in
      if ih=k
      then loop (i+1) ih     ((k*3)+1) acc
      else loop (i+1) (ih+1) k         acc
  in
  loop 0 1 4 init

let gain ~iterations =
  iter ~iterations ~init:1.
    ~f:(fun ~i ~ih:_ a -> a *. Float.(sqrt (1. +. (2. ** (2. *. (- of_int i))))))

let gainh ~iterations =
  iter ~iterations ~init:1.
    ~f:(fun ~i:_ ~ih a -> a *. Float.(sqrt (1. -. (2. ** (2. *. (- of_int ih))))))

(* (1/2) * log2 ((1+t) / (1-t)) *)
let atanh t =
  0.5 *. (Float.log ((1.0 +. t) /. (1.0 -. t)))

let cordic ?(c = 0.) () ~(system : System.t) ~(mode : Mode.t) ~iterations ~x ~y ~z =
  iter ~iterations ~init:(x, y, z)
    ~f:(fun ~i ~ih (x,y,z) ->
      let t i = Float.(2. ** - of_int i) in
      let t, m, e =
        match system with
        | Circular   -> let t = t i  in t ,  1. , Float.atan t
        | Linear     -> let t = t i  in t ,  0. , t
        | Hyperbolic -> let t = t ih in t , -1. , atanh t
      in
      let d =
        match mode with
        | Rotation  -> if Float.(z < 0.) then -1. else  1.
        | Vectoring -> if Float.(y < 0.) then  1. else -1.
        | Inverse   -> if Float.(y < c ) then  1. else -1.
      in
      let x' = x -. (m *. y *. d *. t) in
      let y' = y +. (x *. d *. t) in
      let z' = z -. (d *. e) in
      x', y', z')

(* range |pi/2| *)
let cos_sin ~iterations angle =
  let gain = gain ~iterations in
  let x, y, _ = cordic () ~system:Circular ~mode:Rotation ~iterations
                  ~x:(1. /. gain) ~y:0. ~z:angle
  in
  x, y

let polar_to_rect ~iterations mag phase =
  let gain = gain ~iterations in
  let x, y, _ = cordic () ~system:Circular ~mode:Rotation ~iterations
                  ~x:(mag /. gain) ~y:0. ~z:phase
  in
  x, y

let rotate_vector ~iterations x y angle =
  let gain = gain ~iterations in
  let x, y, _ = cordic () ~system:Circular ~mode:Rotation ~iterations
                  ~x ~y ~z:angle
  in
  x /. gain, y /. gain

(* no particular range restriction *)
let atan ~iterations a =
  let _, _, z = cordic () ~system:Circular ~mode:Vectoring ~iterations
                  ~x:1.0 ~y:a ~z:0.
  in
  z

let atan2 ~iterations y x =
  let _, _, z = cordic () ~system:Circular ~mode:Vectoring ~iterations
                  ~x ~y ~z:0.
  in
  z

let rect_to_polar ~iterations x y =
  let gain = gain ~iterations in
  let x, _, z = cordic () ~system:Circular ~mode:Vectoring ~iterations
                  ~x ~y ~z:0.
  in
  x /. gain, z

(* range ~|0.98| *)
let asin ~iterations a =
  let gain = gain ~iterations in
  let _, _, z = cordic ~system:Circular ~mode:Inverse ~iterations
                  ~x:(1. /. gain) ~y:0. ~z:0. ~c:(Float.abs a) ()
  in
  if Float.(a < 0.) then z else -. z

let mul ~iterations a b =
  let _, y, _ = cordic () ~system:Linear ~mode:Rotation ~iterations
                  ~x:a ~y:0. ~z:b
  in
  y

let div ~iterations a b =
  let _, _, z = cordic () ~system:Linear ~mode:Vectoring ~iterations
                  ~x:b ~y:a ~z:0.
  in
  z

let cosh_sinh ~iterations a =
  let gainh = gainh ~iterations in
  let x, y, _ = cordic () ~system:Hyperbolic ~mode:Rotation ~iterations
                  ~x:(1. /. gainh) ~y:0. ~z:a
  in
  x, y

(* range ~|0.8| *)
let atanh ~iterations a =
  let _, _, z = cordic () ~system:Hyperbolic ~mode:Vectoring ~iterations
                  ~x:1. ~y:a ~z:0.
  in
  z
