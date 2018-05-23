open! Import

exception Failure of string
let failwith str = raise (Failure str)

module Signedness = struct
  type t = Signed | Unsigned [@@deriving sexp_of]
end

(* detect if we are running on a 32 or 64 bit platform *)
let platform_bits =
  let min = Nativeint.min_value in
  let tst = Nativeint.shift_left 1n 31 in
  if Nativeint.equal min tst then 32 else 64

(* some simple composition/pipelining operators *)
let (>>) f g x = g (f x)
let (<<) g f x = g (f x)

(* Conversions *)

let list_of_string s =
  let len = String.length s in
  let rec str i =
    if i = len
    then []
    else s.[i] :: str (i+1)
  in
  str 0

let int_of_hchar c =
  match c with
  | '0' -> 0
  | '1' -> 1
  | '2' -> 2
  | '3' -> 3
  | '4' -> 4
  | '5' -> 5
  | '6' -> 6
  | '7' -> 7
  | '8' -> 8
  | '9' -> 9
  | 'a' | 'A' -> 10
  | 'b' | 'B' -> 11
  | 'c' | 'C' -> 12
  | 'd' | 'D' -> 13
  | 'e' | 'E' -> 14
  | 'f' | 'F' -> 15
  | _ -> failwith "Invalid hex char"

let int_of_bchar = function
  | '0' -> 0
  | '1' -> 1
  | _ -> failwith ("int_of_bin_char: Invalid binary character encountered")

let t_of_bstr (lsl) (lor) zero one b =
  List.fold (list_of_string b) ~init:zero
    ~f:(fun acc v -> (acc lsl 1) lor (if Char.equal v '1' then one else zero))

let int_of_bstr = t_of_bstr (lsl) (lor) 0 1
let int32_of_bstr = t_of_bstr Int32.shift_left Int32.logor 0l 1l
let int64_of_bstr = t_of_bstr Int64.shift_left Int64.logor 0L 1L
let nativeint_of_bstr = t_of_bstr Nativeint.shift_left Nativeint.logor 0n 1n

let bstr_of_int w d =
  let rec b i d =
    if i = w
    then ""
    else b (i+1) (d asr 1) ^ (if d land 1 = 1 then "1" else "0")
  in
  b 0 d

let bstr_of_int32 w d =
  let module I = Int32 in
  let rec b i d =
    if i = w
    then ""
    else b (i+1) (I.shift_right d 1) ^
         (if I.equal (I.logand d 1l) 1l then "1" else "0")
  in
  b 0 d

let bstr_of_int64 w d =
  let module I = Int64 in
  let rec b i d =
    if i = w
    then ""
    else b (i+1) (I.shift_right d 1) ^
         (if I.equal (I.logand d 1L) 1L then "1" else "0")
  in
  b 0 d

let bstr_of_nint w d =
  let module I = Nativeint in
  let rec b i d =
    if i = w
    then ""
    else b (i+1) (I.shift_right d 1) ^
         (if I.equal (I.logand d 1n) 1n then "1" else "0")
  in
  b 0 d

let rec bstr_of_intbitslist = function
  | [] -> ""
  | h :: t -> (if h = 1 then "1" else "0") ^ (bstr_of_intbitslist t)

let intbitslist_of_bstr s =
  let len = String.length s in
  let rec make i =
    if i = len
    then []
    else
      (if Char.equal s.[i] '1' then 1 else 0) :: make (i+1)
  in
  make 0

let int_of_hstr s =
  let len = String.length s in
  let v = ref 0 in
  for i = 0 to (len-1) do
    v := (!v lsl 4) lor (int_of_hchar s.[i])
  done;
  !v

let bstr_of_hstr (signedness : Signedness.t) width hex =
  let len = String.length hex in
  let len4 = len * 4 in
  let rec make_string i =
    if i = 0
    then ""
    else (make_string (i-1)) ^ (bstr_of_int 4 (int_of_hchar hex.[i-1])) in
  let result = make_string len in
  if width < len4
  then String.sub result ~pos:(len4-width) ~len:width
  else
    (String.init (width - len4) ~f:(fun _ ->
       match signedness with
       | Signed -> result.[0]
       | Unsigned -> '0'))
    ^ result

let rec hstr_of_bstr (sign : Signedness.t) s =
  let hex_of_bin s =
    match s with
    | "0000" -> "0"
    | "0001" -> "1"
    | "0010" -> "2"
    | "0011" -> "3"
    | "0100" -> "4"
    | "0101" -> "5"
    | "0110" -> "6"
    | "0111" -> "7"
    | "1000" -> "8"
    | "1001" -> "9"
    | "1010" -> "a"
    | "1011" -> "b"
    | "1100" -> "c"
    | "1101" -> "d"
    | "1110" -> "e"
    | "1111" -> "f"
    | _ -> failwith "Invalid string"
  in
  let len = String.length s in
  match len with
  | 0 -> failwith "Invalid string"
  | 1 | 2 | 3 ->
    hex_of_bin
      ((match sign with
         | Signed   -> String.init (4-len) ~f:(fun _ -> s.[0])
         | Unsigned -> String.init (4-len) ~f:(fun _ -> '0'))
       ^ s)
  | 4 -> hex_of_bin s
  | _ ->
    hstr_of_bstr sign (String.sub s ~pos:0 ~len:(len-4))
    ^ hex_of_bin (String.sub s ~pos:(len-4) ~len:4)

(* arraybits to/from string conversion *)

module type Convert_arg = sig
  type t [@@deriving compare]
  include Equal.S with type t := t
  val platform_bits : int
  val logor : t -> t -> t
  val logand : t -> t -> t
  val shift_left : t -> int -> t
  val zero : t
  val of_int_exn : int -> t
end

let convert_abits_and_string (type t) (module X : Convert_arg with type t = t) =
  let open X in
  let (|.) = logor in
  let (&.) = logand in
  let (<<.) = shift_left in
  let one = of_int_exn 1 in
  let of_bstr b =
    let width = String.length b in
    let words = (width + platform_bits - 1) / platform_bits in
    let a = Array.create ~len:words zero in
    let rec build n =
      let word = n / platform_bits in
      let bit = n mod platform_bits in
      if Char.equal b.[width - n - 1] '1'
      then a.( word ) <- a.( word ) |. (one <<. bit);
      if n <> 0
      then build (n-1)
    in
    build (width-1);
    a in
  let to_bstr width a =
    if width = 0
    then ""
    else
      let b = Bytes.make width '0' in
      let rec build n =
        let word = n / platform_bits in
        let bit = n mod platform_bits in
        if not (X.equal (a.( word ) &. (one <<. bit)) zero)
        then Bytes.set b (width - n - 1) '1';
        if n <> 0
        then build (n-1)
      in
      build (width-1);
      Bytes.to_string b in
  of_bstr, to_bstr

let abits_int32_of_bstr, bstr_of_abits_int32 =
  convert_abits_and_string
    (module struct
      let platform_bits = 32
      include Int32
    end)

let abits_int64_of_bstr, bstr_of_abits_int64 =
  convert_abits_and_string
    (module struct
      let platform_bits = 64
      include Int64
    end)

let abits_nint_of_bstr, bstr_of_abits_nint =
  convert_abits_and_string
    (module struct
      let platform_bits = platform_bits
      include Nativeint
    end)

let abits_int_of_bstr, bstr_of_abits_int =
  convert_abits_and_string
    (module struct
      let platform_bits = platform_bits-2
      include Int
    end)

(* Converts a big int to a binary string *)
let bstr_of_big_int width b =

  let b = ref b in
  let one = Big_int.unit_big_int in
  let two = Big_int.big_int_of_int 2 in

  let str = String.init width ~f:(fun _ -> '0') in
  let len = String.length str in

  let rec make_string i =
    if i = 0
    then ""
    else (
      let q, m = Big_int.quomod_big_int !b two in
      b := q;
      (make_string (i-1)) ^ (if Big_int.eq_big_int m one then "1" else "0"))
  in
  make_string len

let big_int_of_bstr b =
  let zero = Big_int.zero_big_int in
  let len = String.length b in
  let rec make v n =
    if n=len
    then v
    else
      let s =
        if Char.equal b.[n] '1'
        then 1
        else 0
      in
      let s = Big_int.add_int_big_int s (Big_int.mult_int_big_int 2 v) in
      make s (n+1)
  in
  make zero 0

let abits_of_big_int' nbits (&.) (|.) (~.) (<<.) mone zero _ get set create of_bi w b =
  let div = Big_int.big_int_of_int (1 lsl (nbits/2)) in
  let a = create ~len:((w+nbits-1)/nbits) zero in
  let rec make v i bits =
    if bits <= 0
    then a
    else
      let q, m = Big_int.quomod_big_int v div in
      let m = of_bi m in
      if i mod 2 = 0
      then set a (i/2) m
      else set a (i/2) ((get a (i/2)) |. (m <<. (nbits/2)));
      make q (i+1) (bits-(nbits/2))
  in
  let mask s =
    let top_word = (w-1) / nbits in
    let bits = w mod nbits in
    let mask = if bits=0 then mone else ~. (mone <<. bits) in
    set s top_word ((get s top_word) &. mask);
    s
  in
  mask (make b 0 w)

let big_int_of_abits' nbits (&.) (<<.) (>>.) mone one get length to_bi b =
  let mask = mone >>. (nbits/2) in
  let bit = one <<. (nbits/2) in
  let multf = to_bi bit in
  let words = length b in
  let rec make n v =
    if n < 0
    then v
    else
      let vi = to_bi (((get b n) >>. (nbits/2)) &. mask) in
      let v = Big_int.add_big_int vi (Big_int.mult_big_int v multf) in
      let vi = to_bi ((get b n) &. mask) in
      let v = Big_int.add_big_int vi (Big_int.mult_big_int v multf) in
      make (n-1) v
  in
  make (words-1) Big_int.zero_big_int

(* ... *)

let abits_int32_of_big_int =
  abits_of_big_int'
    32 Int32.logand Int32.logor Int32.lognot Int32.shift_left
    (-1l) 0l 1l
    Array.get Array.set Array.create Big_int.int32_of_big_int

let abits_int64_of_big_int =
  abits_of_big_int'
    64 Int64.logand Int64.logor Int64.lognot Int64.shift_left
    (-1L) 0L 1L
    Array.get Array.set Array.create Big_int.int64_of_big_int

let abits_nint_of_big_int =
  abits_of_big_int'
    platform_bits Nativeint.logand Nativeint.logor Nativeint.lognot Nativeint.shift_left
    (-1n) 0n 1n
    Array.get Array.set Array.create Big_int.nativeint_of_big_int

let big_int_of_abits_int32 =
  big_int_of_abits'
    32 Int32.logand Int32.shift_left Int32.shift_right_logical
    (-1l) 1l
    Array.get Array.length Big_int.big_int_of_int32

let big_int_of_abits_int64 =
  big_int_of_abits'
    64 Int64.logand Int64.shift_left Int64.shift_right_logical
    (-1L) 1L
    Array.get Array.length Big_int.big_int_of_int64

let big_int_of_abits_nint =
  big_int_of_abits'
    platform_bits Nativeint.logand Nativeint.shift_left Nativeint.shift_right_logical
    (-1n) 1n
    Array.get Array.length Big_int.big_int_of_nativeint

(* number + list handling *)

let rec nbits x =
  if x < 0 then failwith "arg to [nbits] must be >= 0";
  match x with 0 | 1 -> 1 | x -> 1 + (nbits (x/2))

let range n =
  let rec r0 n i = if n = i then [] else i :: (r0 n (i+1)) in
  r0 n 0

let lselect l lo hi =
  let rec ls l idx lo hi =
    if idx > hi
    then []
    else if idx < lo
    then ls (List.tl_exn l) (idx+1) lo hi
    else (List.hd_exn l) :: (ls (List.tl_exn l) (idx+1) lo hi) in
  ls l 0 lo hi

(* Selects the even elements from a list *)
let leven l =
  let rec r l n =
    match l with
    | [] -> []
    | hd :: tl ->
      if (n land 1) = 0
      then hd :: (r tl (n+1))
      else (r tl (n+1)) in
  r l 0

(* Selects the odd elements from a list *)
let lodd l =
  let rec r l n =
    match l with
    | [] -> []
    | hd :: tl ->
      if (n land 1) = 1
      then hd :: (r tl (n+1))
      else (r tl (n+1)) in
  r l 0

let rec zip a b =
  match a, b with
  | _, [] -> []
  | [], _ -> []
  | a :: b, c :: d -> (a, c) :: zip b d

let pairs l = zip (leven l) (lodd l)

let split_pow2 l =
  let len = List.length l in
  let ll = Int.ceil_log2 len in
  match len with
  | 0 -> [], []
  | 1 -> l, []
  | 2 -> [ List.hd_exn l ], List.tl_exn l
  | _ ->
    let ll = 1 lsl (ll-1) in
    lselect l 0 (ll-1), lselect l ll (len-1)
