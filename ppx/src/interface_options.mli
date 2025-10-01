open Ppxlib

type t =
  { rtlprefix : expression option
  ; rtlsuffix : expression option
  ; rtlmangle : expression option
  ; ast : bool
  ; derive_from_map2 : bool
  ; pre : bool
  }
