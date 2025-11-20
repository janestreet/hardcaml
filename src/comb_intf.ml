open! Core0

(** Various functions that build a tree-structured circuit take an optional
    [branching_factor] argument that controls the number of branches at each level of the
    circuit. With [N] inputs and [branching_factor = 1] the depth is [N]. With
    [branching_factor = 2] the the depth is [ceil_log2 N]. Similarly for
    [branching_factor = X], the depth is given by [ceil_log_{X} N]. *)
type 'a optional_branching_factor = ?branching_factor:int (** default is 2 *) -> 'a

type 'a with_valid = ('a, 'a) with_valid2

and ('a, 'b) with_valid2 =
  { valid : 'a
  ; value : 'b
  }

(** Types wrappers for vectors which differentiate their signedness.

    Operator argument widths are more flexible as we know how to resize them. Results are
    sized to avoid truncation. *)
module type Typed_math = sig
  (** Base signal or bits type *)
  type t

  (** Addition. Arguments are extended appropriately and result is 1 bit wider to avoid
      truncation. *)
  val ( +: ) : t -> t -> t

  (** Subtraction. Arguments are extended appropriately and result is 1 bit wider to avoid
      truncation. *)
  val ( -: ) : t -> t -> t

  (** Multiplication. *)
  val ( *: ) : t -> t -> t

  (** {2 Comparison operations}

      Arguments need not be the same width. *)

  val ( <: ) : t -> t -> t
  val ( >: ) : t -> t -> t
  val ( <=: ) : t -> t -> t
  val ( >=: ) : t -> t -> t
  val ( ==: ) : t -> t -> t
  val ( <>: ) : t -> t -> t

  (** Resize argument to given width. Appropriate extension is performed. *)
  val resize : t -> int -> t

  (** Reduce the width of [t] to [width] bits. The result is [valid] if the [value] fits
      within [width] bits. *)
  val truncate : t -> width:int -> t with_valid
end

module type Gates = sig
  type t [@@deriving sexp_of]

  include%template Equal.S [@mode local] with type t := t

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

  val gnd : t
  val vdd : t

  (** select a range of bits *)
  val select : t -> high:int -> low:int -> t

  (** names a signal *)
  val ( -- ) : loc:[%call_pos] -> t -> string -> t

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

  (** match against explicit case values. *)
  val cases : default:t -> t -> (t * t) list -> t
end

(** Constructors for combinational types. *)
module type Constructors = sig
  (** Type for implementing the combinational interface. *)
  type t

  (** logic 1 *)
  val vdd : t

  (** logic 0 *)
  val gnd : t

  (** create signal from a constant *)
  val of_constant : Constant.t -> t

  (** convert binary string to constant *)
  val of_bit_string : string -> t

  (** Convert integer to constant. Negative values are sign extended up to [width]. Input
      values which are larger or smaller than those representable in [width] bits are
      truncated. *)
  val of_int_trunc : width:int -> int -> t

  val of_int32_trunc : width:int -> int32 -> t
  val of_int64_trunc : width:int -> int64 -> t

  (** Same as [of_int_trunc] *)
  val of_int : width:int -> int -> t
  [@@deprecated "[since 2024-11] use [of_int_trunc]"]

  val of_int32 : width:int -> int32 -> t
  [@@deprecated "[since 2024-11] use [of_int32_trunc]"]

  val of_int64 : width:int -> int64 -> t
  [@@deprecated "[since 2024-11] use [of_int64_trunc]"]

  (** Convert non-negative values to constant. Input values which are not representable in
      [width] bits will raise. *)
  val of_unsigned_int : width:int -> int -> t

  val of_unsigned_int32 : width:int -> int32 -> t
  val of_unsigned_int64 : width:int -> int64 -> t

  (** Convert signed values to constant. Input values which are not representable in
      [width] bits will raise. *)
  val of_signed_int : width:int -> int -> t

  val of_signed_int32 : width:int -> int32 -> t
  val of_signed_int64 : width:int -> int64 -> t

  (** convert hex string to a constant. If the target width is greater than the hex length
      and [signedness] is [Signed] then the result is sign extended. Otherwise the result
      is zero padded. *)
  val of_hex
    :  ?signedness:Signedness.t (** default is [Unsigned] *)
    -> width:int
    -> string
    -> t

  (** convert octal string to a constant. If the target width is greater than the octal
      length and [signedness] is [Signed] then the result is sign extended. Otherwise the
      result is zero padded. *)
  val of_octal
    :  ?signedness:Signedness.t (** default is [Unsigned] *)
    -> width:int
    -> string
    -> t

  (** Convert an arbitrarily wide integer value to a constant. *)
  val of_bigint : width:int -> Bigint.t -> t

  (** convert verilog style or binary string to constant *)
  val of_string : string -> t

  (** convert IntbitsList to constant *)
  val of_bit_list : int list -> t

  val of_decimal_string : width:int -> string -> t

  (** convert a [char] to an 8 bit constant *)
  val of_char : char -> t

  (** convert a [bool] to [vdd] or [gnd] *)
  val of_bool : bool -> t

  (** convert a string to a series of 8-bit characters and concatenate them into a signal,
      with the first character of the string being in the least-significant bits *)
  val of_ascii_string_lsb : string -> t

  (** convert a string to a series of 8-bit characters and concatenate them into a signal,
      with the first character of the string being in the most-significant bits *)
  val of_ascii_string_msb : string -> t

  (** create random constant vector of given width *)
  val random : width:int -> t

  (** [zero w] makes a the zero valued constant of width [w] *)
  val zero : int -> t

  (** [ones w] makes a constant of all ones of width [w] *)
  val ones : int -> t

  (** [one w] makes a one valued constant of width [w] *)
  val one : int -> t
end

(** Full combinational API *)
module type S = sig
  type t [@@deriving sexp_of]

  include%template Equal.S [@mode local] with type t := t

  include Constructors with type t := t

  (** the empty signal *)
  val empty : t

  (** returns true if the signal is empty *)
  val is_empty : t -> bool

  (** names a signal

      [let a = a -- "a" in ...]

      signals may have multiple names. *)
  val ( -- ) : loc:[%call_pos] -> t -> string -> t

  (** returns the width (number of bits) of a signal.

      [let w = width s in ...] *)
  val width : t -> int

  (** [addess_bits_for num_elements] returns the address width required to index
      [num_elements].

      It is the same as [Int.ceil_log2], except it wll return a minimum value of 1 (since
      you cannot have 0 width vectors). Raises if [num_elements] is [< 0]. *)
  val address_bits_for : int -> int

  (** [num_bits_to_represent x] returns the number of bits required to represent the
      number [x], which should be [>= 0]. *)
  val num_bits_to_represent : int -> int

  (** convert the signal to a constant *)
  val to_constant : t -> Constant.t

  (** [concat ts] concatenates a list of signals - the msb of the head of the list will
      become the msb of the result.

      [let c = concat [ a; b; c ] in ...]

      [concat] raises if [ts] is empty or if any [t] in [ts] is empty. *)
  val concat_msb : t list -> t

  (** Similar to [concat_msb] except the lsb of the head of the list will become the lsb
      of the result. *)
  val concat_lsb : t list -> t

  (** concatenate two signals.

      [let c = a @: b in ...]

      equivalent to [concat [ a; b ]] *)
  val ( @: ) : t -> t -> t

  val is_vdd : t -> bool [@@deprecated "[since 2025-03] use [to_bool]"]
  val is_gnd : t -> bool [@@deprecated "[since 2025-03] use [not to_bool]"]

  (** [select t ~high ~low] selects from [t] bits in the range [high]...[low], inclusive.
      [select] raises unless [high] and [low] fall within [0 .. width t - 1] and
      [high >= lo]. *)
  val select : t -> high:int -> low:int -> t

  (** select a single bit *)
  val bit : t -> pos:int -> t

  (** get most significant bit *)
  val msb : t -> t

  (** get least significant bits *)
  val lsbs : t -> t

  (** get least significant bit *)
  val lsb : t -> t

  (** get most significant bits *)
  val msbs : t -> t

  (** [drop_bottom s ~width] drop bottom [width] bits of [s] *)
  val drop_bottom : t -> width:int -> t

  (** [drop_top s ~width] drop top [width] bits of [s] *)
  val drop_top : t -> width:int -> t

  (** [sel_bottom s ~width] select bottom [width] bits of [s] *)
  val sel_bottom : t -> width:int -> t

  (** [sel_top s ~width] select top [width] bits of [s] *)
  val sel_top : t -> width:int -> t

  (* Verilog-like addressing operators. These operators are less verbose than the function
     counterparts. See IEEE 1800 Syntax 11-5. *)

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
      hold:

      [l] <= [Int.pow 2 w], [l] >= 1

      If [l] < [Int.pow 2 w], the last input is repeated.

      All inputs provided must have the same width, which will in turn be equal to the
      width of [m]. *)
  val mux : t -> t list -> t

  (** Same as [mux] except requires:

      [l] = [Int.pow 2 w] *)
  val mux_strict : t -> t list -> t

  (** [mux2 c t f] 2 input multiplexer. Selects [t] if [c] is high otherwise [f].

      [t] and [f] must have same width and [c] must be 1 bit.

      Equivalent to [mux c [f; t]] *)
  val mux2 : t -> t -> t -> t

  val mux_init : t -> int -> f:(int -> t) -> t

  (** [cases ~default select [(match0, value0); (match0, value0)...]] compares [select]
      with [matchN] and outputs [valueN]. If nothing matches [default] is output. This
      construct maps to a case statement.

      The [matchN] values must be constants. *)
  val cases : default:t -> t -> (t * t) list -> t

  (** logical and *)
  val ( &: ) : t -> t -> t

  val ( &:. ) : t -> int -> t
  val ( &+. ) : t -> int -> t

  (** a <>:. 0 &: b <>:. 0 *)
  val ( &&: ) : t -> t -> t

  (** logical or *)
  val ( |: ) : t -> t -> t

  val ( |:. ) : t -> int -> t
  val ( |+. ) : t -> int -> t

  (** {v  a <>:. 0 |: b <>:. 0 v} *)
  val ( ||: ) : t -> t -> t

  (** logic xor *)
  val ( ^: ) : t -> t -> t

  val ( ^:. ) : t -> int -> t
  val ( ^+. ) : t -> int -> t

  (** logical not *)
  val ( ~: ) : t -> t

  (** addition *)
  val ( +: ) : t -> t -> t

  val ( +:. ) : t -> int -> t
  val ( ++. ) : t -> int -> t

  (** subtraction *)
  val ( -: ) : t -> t -> t

  val ( -:. ) : t -> int -> t
  val ( -+. ) : t -> int -> t

  (** negation *)
  val negate : t -> t

  (** unsigned multiplication *)
  val ( *: ) : t -> t -> t

  (** signed multiplication *)
  val ( *+ ) : t -> t -> t

  (** equality *)
  val ( ==: ) : t -> t -> t

  val ( ==:. ) : t -> int -> t
  val ( ==+. ) : t -> int -> t

  (** inequality *)
  val ( <>: ) : t -> t -> t

  val ( <>:. ) : t -> int -> t
  val ( <>+. ) : t -> int -> t

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

  (** Propositional logic implication operator *)
  val ( -->: ) : t -> t -> t

  (** create string from signal *)
  val to_string : t -> string

  (** [to_int t] treats [t] as unsigned and resizes it to fit exactly within an OCaml
      [Int.t].

      - If [width t > Int.num_bits] then the upper bits are truncated.
      - If [width t >= Int.num_bits] and [bit t (Int.num_bits-1) = vdd] (i.e. the msb of
        the resulting [Int.t] is set), then the result is negative.
      - If [t] is [Signal.t] and not a constant value, an exception is raised. *)
  val to_int_trunc : t -> int

  val to_int32_trunc : t -> int32
  val to_int64_trunc : t -> int64

  (** Same as [to_int_trunc]. *)
  val to_int : t -> int
  [@@deprecated "[since 2024-11] use [to_int_trunc]"]

  val to_int32 : t -> int32 [@@deprecated "[since 2024-11] use [to_int32_trunc]"]
  val to_int64 : t -> int64 [@@deprecated "[since 2024-11] use [to_int64_trunc]"]

  (** [to_signed_int t] treats [t] as signed and resizes it to fit exactly within an OCaml
      [Int.t].

      - An exception is raised if the value cannot fit in the resulting int.
      - If [t] is [Signal.t] and not a constant value, an exception is raised. *)
  val to_signed_int : t -> int

  val to_signed_int32 : t -> int32
  val to_signed_int64 : t -> int64

  (** [to_unsigned_int t] treats [t] as unsigned and resizes it to fit exactly within an
      OCaml [Int.t].

      - An exception is raised if the value cannot fit in the resulting int.
      - The value [int] value must be unsigned (so the max width is [Int.num_bits - 1]).
      - If [t] is [Signal.t] and not a constant value, an exception is raised. *)
  val to_unsigned_int : t -> int

  val to_unsigned_int32 : t -> int32
  val to_unsigned_int64 : t -> int64

  (** Convert signal to a [bool]. The signal must be 1 bit wide. *)
  val to_bool : t -> bool

  (** Convert signal to a [char]. The signal must be 8 bits wide. *)
  val to_char : t -> char

  (** Convert into a string, with the first character being from the least-significant
      bits of the signal. The signal width must be a multiple of 8 bits. *)
  val to_ascii_string_lsb : t -> string

  (** Convert into a string, with the first character being from the most-significant bits
      of the signal. The signal width must be a multiple of 8 bits. *)
  val to_ascii_string_msb : t -> string

  (** create binary string from signal *)
  val to_bstr : t -> string

  (** Convert bits to a Zarith.t *)
  val to_bigint : t -> signedness:Signedness.t -> Bigint.t

  (** convert signal to a list of bits with msb at head of list *)
  val bits_msb : t -> t list

  (** convert signal to a list of bits with lsb at head of list *)
  val bits_lsb : t -> t list

  (** [to_array s] convert signal [s] to array of bits with lsb at index 0 *)
  val to_array : t -> t array

  (** [of_array a] convert array [a] of bits to signal with lsb at index 0 *)
  val of_array : t array -> t

  (** repeat signal [count] times *)
  val repeat : t -> count:int -> t

  (** Split signal in half. The most significant bits will be in the left half of the
      returned tuple.

      - If [msbs] is not provided, the signal will be split in half with the MSB part
        possibly containing one more bit.
      - If [msbs] is provided, [msbs] most significant bits will be split off. *)
  val split_in_half_msb : ?msbs:int -> t -> t * t

  (** Same as [split_in_half_msb], but

      - If [lsbs] is not provided, the LSB part might have one more bit.
      - If [lsbs] is provided, [lsbs] least significant bits will be split off.

      The most significant bits will still be in the left half of the tuple. *)
  val split_in_half_lsb : ?lsbs:int -> t -> t * t

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
      v} *)
  val split_lsb : ?exact:bool (** default is [true] *) -> part_width:int -> t -> t list

  (** Like [split_lsb] except the most significant bits are at the head of the returned
      list. Residual bits when [exact] is [false] goes to the last element of the list, so
      in the general case [split_lsb] is not necessarily equivalent to
      [split_msb |> List.rev]. *)
  val split_msb : ?exact:bool (** default is [true] *) -> part_width:int -> t -> t list

  val bswap : t -> t

  (** shift left logical *)
  val sll : t -> by:int -> t

  (** shift right logical *)
  val srl : t -> by:int -> t

  (** shift right arithmetic *)
  val sra : t -> by:int -> t

  (** rotate left *)
  val rotl : t -> by:int -> t

  (** rotate right *)
  val rotr : t -> by:int -> t

  (** shift by variable amount *)
  val log_shift : f:(t -> by:int -> t) -> t -> by:t -> t

  (** [uresize t ~width:w] returns the unsigned resize of [t] to width [w]. If
      [w = width t], this is a no-op. If [w < width t], this [select]s the [w] low bits of
      [t]. If [w > width t], this extends [t] with [zero (w - width t)]. *)
  val uresize : t -> width:int -> t

  (** [sresize t ~width:w] returns the signed resize of [t] to width [w]. If
      [w = width t], this is a no-op. If [w < width t], this [select]s the [w] low bits of
      [t]. If [w > width t], this extends [t] with [w - width t] copies of [msb t]. *)
  val sresize : t -> width:int -> t

  (** [uextend t ~width:w] behaves similarly to [uresize t ~width:w] but asserts that
      [w >= width] to prevent inadvertently chopping the signal. *)
  val uextend : t -> width:int -> t

  (** [sextend t ~width:w] behaves similarly to [sresize t ~width:w] but asserts that
      [w >= width] to prevent inadvertently chopping the signal. *)
  val sextend : t -> width:int -> t

  (** unsigned resize by +1 bit *)
  val ue : t -> t

  (** signed resize by +1 bit *)
  val se : t -> t

  (** [resize_list ~resize l] finds the maximum width in [l] and applies [resize el max]
      to each element. *)
  val resize_list : resize:(t -> int -> t) -> t list -> t list

  (** [resize_op2 ~resize f a b] applies [resize x w] to [a] and [b] where [w] is the
      maximum of their widths. It then returns [f a b] *)
  val resize_op2 : resize:(t -> int -> t) -> (t -> t -> t) -> t -> t -> t

  (** fold 'op' though list *)
  val reduce : f:('a -> 'a -> 'a) -> 'a list -> 'a

  (** reverse bits *)
  val reverse : t -> t

  (** [mod_counter max t] is [if t = max then 0 else (t + 1)], and can be used to count
      from 0 to [max] then from zero again. If [max == (1<<n - 1)], then a comparator is
      not generated and overflow arithmetic is used instead. *)
  val mod_counter : max:int -> t -> t

  (** [compute_arity ~steps num_values] computes the tree arity required to reduce
      [num_values] in [steps]. [steps<=0] raises. *)
  val compute_arity : steps:int -> int -> int

  (** [compute_tree_branches ~steps num_values] returns a list of length [steps] of
      branching factors required to reduce [num_values]. This tends to produce a slightly
      more balanced sequence than just applying [compute_arity] at every step. *)
  val compute_tree_branches : steps:int -> int -> int list

  (** [tree ~arity ~f input] creates a tree of operations. The arity of the operator is
      configurable. [tree] raises if [input = []]. *)
  val tree : arity:int -> f:('a list -> 'a) -> 'a list -> 'a

  (** [priority_select cases] returns the value associated with the first case whose
      [valid] signal is high. [valid] will be set low in the returned [with_valid] if no
      case is selected. *)
  val priority_select : (t with_valid list -> t with_valid) optional_branching_factor

  (** Same as [priority_select] except returns [default] if no case matches. *)
  val priority_select_with_default
    : (t with_valid list -> default:t -> t) optional_branching_factor

  (** Select a case where one and only one [valid] signal is enabled. If more than one
      case is [valid] then the return value is undefined. If no cases are valid, [0] is
      returned by the current implementation, though this should not be relied upon. *)
  val onehot_select : (t with_valid list -> t) optional_branching_factor

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

  (** [floor_log2 x] returns the floor of log-base-2 of [x]. [x] is treated as unsigned
      and an error is indicated by [valid = gnd] in the return value if [x = 0]. *)
  val floor_log2 : (t -> t with_valid) optional_branching_factor

  (** [ceil_log2 x] returns the ceiling of log-base-2 of [x]. [x] is treated as unsigned
      and an error is indicated by [valid = gnd] in the return value if [x = 0]. *)
  val ceil_log2 : (t -> t with_valid) optional_branching_factor

  (** convert binary to onehot *)
  val binary_to_onehot : t -> t

  (** convert onehot to binary *)
  val onehot_to_binary : t -> t

  (** convert binary to gray code *)
  val binary_to_gray : t -> t

  (** convert gray code to binary *)
  val gray_to_binary : t -> t

  (** Increment a gray code value. The implementation converts to binary, increments the
      binary value, then converts back to gray code. *)
  val gray_increment : t -> by:int -> t

  (** [any_bit_set t] returns [vdd] if at least one bit if set in [t] and [gnd] otherwise. *)
  val any_bit_set : t -> t

  (** [all_bits_set t] returns [vdd] if all bits in [t] are set and [gnd] otherwise. *)
  val all_bits_set : t -> t

  (** [no_bits_set t] returns [vdd] if no bits in [t] are set and [gnd] otherwise. *)
  val no_bits_set : t -> t

  (** Increment (defaults to 1). Wrap on overflow. *)
  val incr : ?by:int -> t -> t

  (** Decrement (defaults to 1). Wrap on overflow. *)
  val decr : ?by:int -> t -> t

  (** Concatention, selection and resizing functions for signals encoded as an option
      where [None] means zero width. *)
  module With_zero_width : sig
    type non_zero_width = t [@@deriving sexp_of]
    type t = non_zero_width option [@@deriving sexp_of]

    val of_non_zero_width : non_zero_width -> t
    val to_non_zero_width : ?default:non_zero_width -> t -> non_zero_width
    val zero_width : t
    val zero : int -> t
    val one : int -> t
    val ones : int -> t
    val concat_msb : t list -> t
    val concat_lsb : t list -> t
    val select : t -> high:int -> low:int -> t
    val lsbs : t -> t
    val msbs : t -> t
    val drop_bottom : t -> width:int -> t
    val drop_top : t -> width:int -> t
    val sel_bottom : t -> width:int -> t
    val sel_top : t -> width:int -> t
    val repeat : t -> count:int -> t
    val mux : t -> non_zero_width list -> non_zero_width
  end

  module type Typed_math = Typed_math with type t := t

  (** Unsigned vector operations (ie may operate on [Bits.t] or [Signal.t] directly). *)
  module Unsigned : Typed_math

  (** Signed vector operations (ie may operate on [Bits.t] or [Signal.t] directly). *)
  module Signed : Typed_math
end

module type Gen_cases_from_mux = sig
  type t

  val mux : t -> t list -> t
  val ( ==: ) : t -> t -> t
end

module type Comb = sig
  module type Gates = Gates
  module type Primitives = Primitives
  module type S = S

  type nonrec 'a optional_branching_factor = 'a optional_branching_factor

  type nonrec ('a, 'b) with_valid2 = ('a, 'b) with_valid2 =
    { valid : 'a
    ; value : 'b
    }

  type nonrec 'a with_valid = ('a, 'a) with_valid2

  module Make_primitives (Gates : Gates) : Primitives with type t = Gates.t

  (** Generates the full combinational API *)
  module Make (Primitives : Primitives) : S with type t = Primitives.t

  module Expert : sig
    (** Generate the implementation of [cases] based on equality and muxs. *)
    module Gen_cases_from_mux (Comb : Gen_cases_from_mux) : sig
      open Comb

      val cases : default:t -> t -> (t * t) list -> t
    end
  end
end
