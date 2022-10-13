open Base

module type Cases = Flags_vector_intf.Cases
module type S = Flags_vector_intf.S

module Make (Cases : Cases) = struct
  type cases = Cases.t

  let number_of = List.length Cases.all

  include Scalar.Make (struct
      let port_name = Cases.port_name
      let port_width = number_of
    end)

  let to_int = Cases.Variants.to_rank

  let of_int =
    let arr = Array.of_list Cases.all in
    Array.get arr
  ;;

  module Flags = struct
    let flags =
      List.map Cases.all ~f:(fun v -> Flags.create ~bit:(Cases.Variants.to_rank v))
    ;;

    include Flags.Make (struct
        let allow_intersecting = false
        let should_print_error = true
        let remove_zero_flags = false

        let known =
          List.map2_exn Cases.all flags ~f:(fun v flag ->
            flag, Cases.sexp_of_t v |> Sexplib.Sexp.to_string)
        ;;
      end)

    let of_cases =
      let arr = Array.of_list flags in
      fun t -> Array.get arr (to_int t)
    ;;

    let to_string t = Sexp.to_string (sexp_of_t t)
  end

  let init (type a) (module Comb : Comb.S with type t = a) f =
    List.map Cases.all ~f:(fun v -> f v) |> Comb.concat_lsb
  ;;

  let of_bits (type a) (module Comb : Comb.S with type t = a) b : a =
    create (module Comb) b
  ;;

  let to_bits b = b

  let of_flags (type a) (module Comb : Comb.S with type t = a) flags =
    Comb.of_int ~width:number_of (Flags.to_int_exn flags)
  ;;

  let of_cases_list (type a) (module Comb : Comb.S with type t = a) ts =
    List.fold ts ~init:Flags.empty ~f:(fun flags v ->
      Flags.( + ) flags (Flags.of_cases v))
    |> of_flags (module Comb)
  ;;

  let to_cases_list (t : Bits.t t) =
    let bits = Bits.bits_lsb t in
    List.filter_mapi bits ~f:(fun index bit ->
      if Bits.is_vdd bit then Some (of_int index) else None)
  ;;

  let to_flags (t : Bits.t t) =
    let ts = to_cases_list t in
    List.fold ts ~init:Flags.empty ~f:(fun flags v ->
      Flags.( + ) flags (Flags.of_cases v))
  ;;

  let add (type a) (module Comb : Comb.S with type t = a) (s : a t) (t : a t) =
    Comb.( |: ) s t
  ;;

  let invert (type a) (module Comb : Comb.S with type t = a) (s : a t) = Comb.( ~: ) s

  let remove (type a) (module Comb : Comb.S with type t = a) (s : a t) (t : a t) =
    Comb.( &: ) s (invert (module Comb) t)
  ;;

  let is_set (type a) (module Comb : Comb.S with type t = a) (s : a t) v =
    Comb.(s.:(Cases.Variants.to_rank v))
  ;;

  let mux2 (type a) (module Comb : Comb.S with type t = a) (sel : a) (s : a t) (t : a t) =
    Comb.mux2 sel s t
  ;;

  let deref (a : 'a ref t) : 'a t = !a

  let iter_flags t ~f =
    List.iter Cases.all ~f:(fun v -> if Flags.(do_intersect t (of_cases v)) then f v)
  ;;
end
