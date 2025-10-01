include Coverage_intf

module Make (M : sig
    type t

    val total_cases : t -> int
    val covered_cases : t -> int
    val unexpectedly_observed_cases : t -> int
  end) =
struct
  include M

  let fully_covered t = total_cases t = covered_cases t
end
