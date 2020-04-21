open! Import

(** Various functions that build a tree-structured circuit take an optional
    [branching_factor] argument that controls the number of branches at each level of the
    circuit.  With [N] inputs and [branching_factor = 1] the depth is [N].  With
    [branching_factor = 2] the the depth is [ceil_log2 N].  Similarly for
    [branching_factor = X], the depth is by [ceil_log_{X} N]. *)
type 'a optional_branching_factor = ?branching_factor:int (** default is 2 *) -> 'a

module type TypedMath = sig
  type t
  type v

  val of_signal : t -> v
  val to_signal : v -> t
  val ( +: ) : v -> v -> v
  val ( -: ) : v -> v -> v
  val ( *: ) : v -> v -> v
  val ( <: ) : v -> v -> v
  val ( >: ) : v -> v -> v
  val ( <=: ) : v -> v -> v
  val ( >=: ) : v -> v -> v
  val ( ==: ) : v -> v -> v
  val ( <>: ) : v -> v -> v
  val resize : v -> int -> v
end

module type Gates = sig
  type t [@@deriving sexp_of]

  include Equal.S with type t := t

  (** the empty signal *)
  val empty : t

  val is_empty : t -> bool

  (** returns the width of a signal *)
  val width : t -> int

  (** creates a constant *)
  val of_constant : Constant.t -> t

  val to_constant : t -> Constant.t

  (** concatenates a list of signals *)
  val concat_msb : t list -> t

  (** select a range of bits *)
  val select : t -> int -> int -> t

  (** names a signal *)
  val ( -- ) : t -> string -> t

  (** bitwise and *)
  val ( &: ) : t -> t -> t

  (** bitwise or *)
  val ( |: ) : t -> t -> t

  (** bitwise xor *)
  val ( ^: ) : t -> t -> t

  (** bitwise not *)
  val ( ~: ) : t -> t

  (** create string from signal *)
  val to_string : t -> string
end

(** Type required to generate the full combinational API *)
module type Primitives = sig
  include Gates

  (** multiplexer *)
  val mux : t -> t list -> t

  (** addition *)
  val ( +: ) : t -> t -> t

  (** subtraction *)
  val ( -: ) : t -> t -> t

  (** unsigned multiplication *)
  val ( *: ) : t -> t -> t

  (** signed multiplication *)
  val ( *+ ) : t -> t -> t

  (** equality *)
  val ( ==: ) : t -> t -> t

  (** less than *)
  val ( <: ) : t -> t -> t
end

(** Full combinational API *)
module type S = sig
  type t [@@deriving sexp_of]

  include Equal.S with type t := t

  (** the empty signal *)
  val empty : t

  val is_empty : t -> bool

  (** names a signal

      [let a = a -- "a" in ...]

      signals may have multiple names. *)
  val ( -- ) : t -> string -> t

  (** returns the width (number of bits) of a signal.

      [let w = width s in ...] *)
  val width : t -> int

  (** [addess_bits_for num_elements] returns the address width required to index
      [num_elements].

      It is the same as [Int.ceil_log2], except it wll return a minimum value of 1 (since
      you cannot have 0 width vectors). Raises if [num_elements] is [< 0]. *)
  val address_bits_for : int -> int

  (** [num_bits_to_represent x] returns the number of bits required to represent the number
      [x], which should be [>= 0]. *)
  val num_bits_to_represent : int -> int

  val of_constant : Constant.t -> t
  val to_constant : t -> Constant.t


  (** convert binary string to constant *)
  val constb : string -> t

  val of_bit_string : string -> t

  (** convert integer to constant *)
  val of_int : width:int -> int -> t

  val of_int32 : width:int -> int32 -> t
  val of_int64 : width:int -> int64 -> t

  (** convert hex string to a constant. If the target width is greater than the hex length
      and [signedness] is [Signed] then the result is sign extended. Otherwise the result
      is zero padded. *)
  val of_hex
    :  ?signedness:Constant.Signedness.t (** default is [Unsigned] *)
    -> width:int
    -> string
    -> t

  (** convert verilog style or binary string to constant *)
  val of_string : string -> t

  (** convert IntbitsList to constant *)
  val of_bit_list : int list -> t

  val of_decimal_string : width:int -> string -> t

  (** convert a [char] to an 8 bit constant *)
  val of_char : char -> t

  val constv : string -> t [@@deprecated "[since 2019-11] constv]"]
  val consti : width:int -> int -> t [@@deprecated "[since 2019-11] consti]"]
  val consti32 : width:int -> int32 -> t [@@deprecated "[since 2019-11] consti32]"]
  val consti64 : width:int -> int64 -> t [@@deprecated "[since 2019-11] consti64]"]
  val constibl : int list -> t [@@deprecated "[since 2019-11] constibl]"]
  val consthu : width:int -> string -> t [@@deprecated "[since 2019-11] consthu]"]
  val consths : width:int -> string -> t [@@deprecated "[since 2019-11] consths]"]
  val const : string -> t [@@deprecated "[since 2019-11] const"]
  val constd : width:int -> string -> t [@@deprecated "[since 2019-11] constd]"]


  (** [concat ts] concatenates a list of signals - the msb of the head of the list will
      become the msb of the result.

      [let c = concat \[ a; b; c \] in ...]

      [concat] raises if [ts] is empty or if any [t] in [ts] is empty. *)
  val concat_msb : t list -> t

  (** Similar to [concat_msb] except the lsb of the head of the list will become the lsb
      of the result. *)
  val concat_lsb : t list -> t


  (** same as [concat_msb] except empty signals are first filtered out *)
  val concat_msb_e : t list -> t

  (** same as [concat_lsb] except empty signals are first filtered out *)
  val concat_lsb_e : t list -> t

  (** concatenate two signals.

      [let c = a @: b in ...]

      equivalent to [concat \[ a; b \]] *)
  val ( @: ) : t -> t -> t

  (** logic 1 *)
  val vdd : t

  val is_vdd : t -> bool

  (** logic 0 *)
  val gnd : t

  val is_gnd : t -> bool

  (** [zero w] makes a the zero valued constant of width [w] *)
  val zero : int -> t

  (** [ones w] makes a constant of all ones of width [w] *)
  val ones : int -> t

  (** [one w] makes a one valued constant of width [w] *)
  val one : int -> t

  (** [select t hi lo] selects from [t] bits in the range [hi]...[lo], inclusive.
      [select] raises unless [hi] and [lo] fall within [0 .. width t - 1] and [hi >=
      lo]. *)
  val select : t -> int -> int -> t

  (** same as [select] except invalid indices return [empty] *)
  val select_e : t -> int -> int -> t

  (** select a single bit *)
  val bit : t -> int -> t

  (** get most significant bit *)
  val msb : t -> t

  (** get least significant bits *)
  val lsbs : t -> t

  (** get least significant bit *)
  val lsb : t -> t

  (** get most significant bits *)
  val msbs : t -> t

  (** [drop_bottom s n] drop bottom [n] bits of [s] *)
  val drop_bottom : t -> int -> t

  (** [drop_top s n] drop top [n] bits of [s] *)
  val drop_top : t -> int -> t

  (** [sel_bottom s n] select bottom [n] bits of [s] *)
  val sel_bottom : t -> int -> t

  (** [sel_top s n] select top [n] bits of [s] *)
  val sel_top : t -> int -> t

  (* Verilog-like addressing operators. These operators are less verbose than
     the function counterparts. See IEEE 1800 Syntax 11-5. *)

  (** [x.:[hi, lo]] == [select x hi lo] *)
  val ( .:[] ) : t -> int * int -> t

  (** [x.:+[lo, width]] == [select x (lo + width - 1) lo]. If [width] is [None] it selects
      all remaining msbs of the vector ie [x.:+[lo,None]] == [drop_bottom x lo] *)
  val ( .:+[] ) : t -> int * int option -> t

  (** [x.:-[hi, width]] == [select x hi (hi - width + 1)]. If [hi] is [None] it defaults
      to the msb of the vector ie [x.:-[None, width]] == [sel_top x width] *)
  val ( .:-[] ) : t -> int option * int -> t

  (** [x.(i)] == [bit x i] *)
  val ( .:() ) : t -> int -> t

  (** [insert ~into:t x ~at_offset] insert [x] into [t] at given offet *)
  val insert : into:t -> t -> at_offset:int -> t

  (** multiplexer.

      [let m = mux sel inputs in ...]

      Given [l] = [List.length inputs] and [w] = [width sel] the following conditions must
      hold.

      [l] <= 2**[w], [l] >= 2

      If [l] < 2**[w], the last input is repeated.

      All inputs provided must have the same width, which will in turn be equal to the
      width of [m]. *)
  val mux : t -> t list -> t

  (** [mux2 c t f] 2 input multiplexer.  Selects [t] if [c] is high otherwise [f].

      [t] and [f] must have same width and [c] must be 1 bit.

      Equivalent to [mux c \[f; t\]] *)
  val mux2 : t -> t -> t -> t

  val mux_init : t -> int -> f:(int -> t) -> t

  (** logical and *)
  val ( &: ) : t -> t -> t

  val ( &:. ) : t -> int -> t

  (** a <>:. 0 &: b <>:. 0 *)
  val ( &&: ) : t -> t -> t

  (** logical or *)
  val ( |: ) : t -> t -> t

  val ( |:. ) : t -> int -> t

  (** a <>:. 0 |: b <>:. 0 *)
  val ( ||: ) : t -> t -> t

  (** logic xor *)
  val ( ^: ) : t -> t -> t

  val ( ^:. ) : t -> int -> t

  (** logical not *)
  val ( ~: ) : t -> t

  (** addition *)
  val ( +: ) : t -> t -> t

  val ( +:. ) : t -> int -> t

  (** subtraction *)
  val ( -: ) : t -> t -> t

  val ( -:. ) : t -> int -> t

  (** negation *)
  val negate : t -> t

  (** unsigned multiplication *)
  val ( *: ) : t -> t -> t

  (** signed multiplication *)
  val ( *+ ) : t -> t -> t

  (** equality *)
  val ( ==: ) : t -> t -> t

  val ( ==:. ) : t -> int -> t

  (** inequality *)
  val ( <>: ) : t -> t -> t

  val ( <>:. ) : t -> int -> t

  (** less than *)
  val ( <: ) : t -> t -> t

  val ( <:. ) : t -> int -> t

  (* added due to clash with camlp5 *)
  val lt : t -> t -> t

  (** greater than *)
  val ( >: ) : t -> t -> t

  val ( >:. ) : t -> int -> t

  (** less than or equal to *)
  val ( <=: ) : t -> t -> t

  val ( <=:. ) : t -> int -> t

  (** greater than or equal to *)
  val ( >=: ) : t -> t -> t

  val ( >=:. ) : t -> int -> t

  (** signed less than *)
  val ( <+ ) : t -> t -> t

  val ( <+. ) : t -> int -> t

  (** signed greater than *)
  val ( >+ ) : t -> t -> t

  val ( >+. ) : t -> int -> t

  (** signed less than or equal to *)
  val ( <=+ ) : t -> t -> t

  val ( <=+. ) : t -> int -> t

  (** signed greated than or equal to *)
  val ( >=+ ) : t -> t -> t

  val ( >=+. ) : t -> int -> t

  (** create string from signal *)
  val to_string : t -> string


  (** [to_int t] treats [t] as unsigned and resizes it to fit exactly within an OCaml
      [Int.t].

      - If [width t > Int.num_bits] then the upper bits are truncated.
      - If [width t >= Int.num_bits] and [bit t (Int.num_bits-1) = vdd] (i.e. the msb of
        the resulting [Int.t] is set), then the result is negative.
      - If [t] is [Signal.t] and not a constant value, an exception is raised. *)
  val to_int : t -> int


  (** [to_sint t] treats [t] as signed and resizes it to fit exactly within an OCaml
      [Int.t].

      - If [width t > Int.num_bits] then the upper bits are truncated.
      - If [t] is [Signal.t] and not a constant value, an exception is raised. *)
  val to_sint : t -> int

  val to_int32 : t -> int32
  val to_sint32 : t -> int32
  val to_int64 : t -> int64
  val to_sint64 : t -> int64

  (** Convert signal to a [char].  The signal must be 8 bits wide. *)
  val to_char : t -> char

  (** create binary string from signal *)
  val to_bstr : t -> string

  (** convert signal to a list of bits with msb at head of list *)
  val bits_msb : t -> t list

  (** convert signal to a list of bits with lsb at head of list *)
  val bits_lsb : t -> t list

  (** [to_array s] convert signal [s] to array of bits with lsb at index 0 *)
  val to_array : t -> t array

  (** [of_array a] convert array [a] of bits to signal with lsb at index 0 *)
  val of_array : t array -> t

  (** repeat signal n times *)
  val repeat : t -> int -> t

  (** split signal in half. The most significant bits will be in the left half of the
      returned tuple. *)
  val split_in_half_msb : t -> t * t

  (** Split signal into a list of signals with width equal to [part_width]. The least
      significant bits are at the head of the returned list. If [exact] is [true] the
      input signal width must be exactly divisable by [part_width]. When [exact] is
      [false] and the input signal width is not exactly divisible by [part_width], the
      last element will contains residual bits.

      eg:

      {v
        split_lsb ~part_width:4 16b0001_0010_0011_0100 =
          [ 4b0100; 4b0011; 4b0010; 4b0001 ]

        split_lsb ~exact:false ~part_width:4 17b11_0001_0010_0011_0100 =
          [ 4b0100; 4b0011; 4b0010; 4b0001; 2b11 ]
      v}
  *)
  val split_lsb : ?exact:bool (** default is [true] **) -> part_width:int -> t -> t list

  (** Like [split_lsb] except the most significant bits are at the head of the returned
      list. Residual bits when [exact] is [false] goes to the last element of the list,
      so in the general case [split_lsb] is not necessarily equivalent to
      [split_msb |> List.rev]. *)
  val split_msb : ?exact:bool (** default is [true] **) -> part_width:int -> t -> t list

  val bswap : t -> t

  (** shift left logical *)
  val sll : t -> int -> t

  (** shift right logical *)
  val srl : t -> int -> t

  (** shift right arithmetic *)
  val sra : t -> int -> t

  (** rotate left *)
  val rotl : t -> int -> t

  (** rotate right *)
  val rotr : t -> int -> t

  (** shift by variable amount *)
  val log_shift : (t -> int -> t) -> t -> t -> t

  (** [uresize t w] returns the unsigned resize of [t] to width [w].  If [w = width t],
      this is a no-op.  If [w < width t], this [select]s the [w] low bits of [t].  If [w >
      width t], this extends [t] with [zero (width t - w)]. *)
  val uresize : t -> int -> t

  (** [sresize t w] returns the signed resize of [t] to width [w].  If [w = width t], this
      is a no-op.  If [w < width t], this [select]s the [w] low bits of [t].  If [w >
      width t], this extends [t] with [width t - w] copies of [msb t]. *)
  val sresize : t -> int -> t

  (** unsigned resize by +1 bit *)
  val ue : t -> t

  (** signed resize by +1 bit *)
  val se : t -> t

  (** [resize_list ?resize l] finds the maximum width in [l] and applies [resize el max]
      to each element. *)
  val resize_list : resize:(t -> int -> t) -> t list -> t list

  (** [resize_op2 ~resize f a b] applies [resize x w] to [a] and [b] where [w] is the
      maximum of their widths.  It then returns [f a b] *)
  val resize_op2 : resize:(t -> int -> t) -> (t -> t -> t) -> t -> t -> t

  (** fold 'op' though list *)
  val reduce : f:('a -> 'a -> 'a) -> 'a list -> 'a

  (** reverse bits *)
  val reverse : t -> t

  (** [mod_counter max t] is [if t = max then 0 else (t + 1)], and can be used to count
      from 0 to (max-1) then from zero again.  If max == 1<<n, then a comparator is not
      generated and overflow arithmetic used instead.  If *)
  val mod_counter : max:int -> t -> t

  (** [tree ~arity ~f input] creates a tree of operations.  The arity of the operator is
      configurable.  [tree] raises if [input = []]. *)
  val tree : arity:int -> f:('a list -> 'a) -> 'a list -> 'a

  (** [priority_select cases] returns the value associated with the first case whose
      [valid] signal is high.  [valid] will be set low in the returned [With_valid.t] if
      no case is selected. *)
  val priority_select : (t With_valid.t list -> t With_valid.t) optional_branching_factor

  (** Same as [priority_select] except returns [default] if no case matches. *)
  val priority_select_with_default
    : (t With_valid.t list -> default:t -> t) optional_branching_factor

  (** Select a case where one and only one [valid] signal is enabled.  If more than one
      case is [valid] then the return value is undefined.  If no cases are valid, [0] is
      returned by the current implementation, though this should not be relied upon. *)
  val onehot_select : (t With_valid.t list -> t) optional_branching_factor

  (** [popcount t] returns the number of bits set in [t]. *)
  val popcount : (t -> t) optional_branching_factor

  (** [is_pow2 t] returns a bit to indicate if [t] is a power of 2. *)
  val is_pow2 : (t -> t) optional_branching_factor

  (** [leading_ones t] returns the number of consecutive [1]s from the most significant
      bit of [t] down. *)
  val leading_ones : (t -> t) optional_branching_factor

  (** [trailing_ones t] returns the number of consecutive [1]s from the least significant
      bit of [t] up. *)
  val trailing_ones : (t -> t) optional_branching_factor

  (** [leading_zeros t] returns the number of consecutive [0]s from the most significant
      bit of [t] down. *)
  val leading_zeros : (t -> t) optional_branching_factor

  (** [trailing_zeros t] returns the number of consecutive [0]s from the least significant
      bit of [t] up. *)
  val trailing_zeros : (t -> t) optional_branching_factor

  (** [floor_log2 x] returns the floor of log-base-2 of [x].  [x] is treated as unsigned
      and an error is indicated by [valid = gnd] in the return value if [x = 0]. *)
  val floor_log2 : (t -> t With_valid.t) optional_branching_factor

  (** [ceil_log2 x] returns the ceiling of log-base-2 of [x].  [x] is treated as unsigned
      and an error is indicated by [valid = gnd] in the return value if [x = 0]. *)
  val ceil_log2 : (t -> t With_valid.t) optional_branching_factor

  (** convert binary to onehot *)
  val binary_to_onehot : t -> t

  (** convert onehot to binary *)
  val onehot_to_binary : t -> t

  (** convert binary to gray code *)
  val binary_to_gray : t -> t

  (** convert gray code to binary *)
  val gray_to_binary : t -> t

  (** create random constant vector of given width *)
  val random : width:int -> t

  module type TypedMath = TypedMath with type t := t

  (* General arithmetic on unsigned signals.  Operands and results are resized
     to fit as appropriate. *)
  module Unsigned : TypedMath

  (* General arithmetic on signed signals.  Operands and results are resized to fit as
     appropriate. *)
  module Signed : TypedMath

  (** Unsigned operations compatible with type t *)
  module Uop : TypedMath with type v := t

  (** Signed operations compatible with type t *)
  module Sop : TypedMath with type v := t
end

module type Comb = sig
  module type Gates = Gates
  module type Primitives = Primitives
  module type S = S

  type nonrec 'a optional_branching_factor = 'a optional_branching_factor

  module Make_primitives (Gates : Gates) : Primitives with type t = Gates.t

  (** Generates the full combinational API *)
  module Make (Primitives : Primitives) : S with type t = Primitives.t
end
