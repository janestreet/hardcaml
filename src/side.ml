type t =
  | Before
  | After
[@@deriving compare ~localize, sexp_of]
