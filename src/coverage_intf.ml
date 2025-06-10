module type S = sig
  type t

  val fully_covered : t -> bool
end

module type Coverage = sig
  module type S = S

  module Make (M : sig
      type t

      val total_cases : t -> int
      val covered_cases : t -> int
    end) : S with type t = M.t
end
