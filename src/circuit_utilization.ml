open! Import

module Total_bits = struct
  type t =
    { count : int
    ; total_bits : int
    }
  [@@deriving sexp_of]

  let none = { count = 0; total_bits = 0 }

  let add ?(u = none) s =
    Some { count = u.count + 1; total_bits = u.total_bits + Signal.width s }
  ;;
end

module Total_and_max_bits = struct
  type t =
    { count : int
    ; total_bits : int
    ; max_instance_bits : int
    }
  [@@deriving sexp_of]

  let none = { count = 0; total_bits = 0; max_instance_bits = 0 }

  let add ?(u = none) s =
    let w = Signal.width s in
    Some
      { count = u.count + 1
      ; total_bits = u.total_bits + w
      ; max_instance_bits = max u.max_instance_bits w
      }
  ;;
end

module Multiplexer = struct
  type t =
    { max_instance_bits : int
    ; total_bits : int
    ; count : int
    }
  [@@deriving sexp_of]
end

module Multiplexers = struct
  (* Index mutliplexers by their depth (ie numberof_data_elements) as this most directly
     effects timing and means we tend to print fewer instances. *)
  module Mux_map = struct
    type t = Multiplexer.t Map.M(Int).t

    let sexp_of_elt (number_of_data_elements, (m : Multiplexer.t)) =
      let max_instance_bits = m.max_instance_bits in
      let total_bits = m.total_bits in
      let count = m.count in
      [%message
        (number_of_data_elements : int)
          (max_instance_bits : int)
          (total_bits : int)
          (count : int)]
    ;;

    let sexp_of_t t = [%sexp (Map.to_alist t : elt list)]
  end

  type t =
    { count : int
    ; total_bits : int
    ; multiplexers : Mux_map.t
    }
  [@@deriving sexp_of]

  let none = { count = 0; total_bits = 0; multiplexers = Map.empty (module Int) }

  let add ?(u = none) s =
    let number_of_data_elements = List.length (Signal.deps s) - 1 in
    let data_width = Signal.width s in
    let key = number_of_data_elements in
    let total_bits = number_of_data_elements * data_width in
    Some
      { count = u.count + 1
      ; total_bits = u.total_bits + total_bits
      ; multiplexers =
          (match Map.find u.multiplexers key with
           | None ->
             Map.add_exn
               u.multiplexers
               ~key
               ~data:
                 { max_instance_bits = number_of_data_elements; total_bits; count = 1 }
           | Some data ->
             Map.set
               u.multiplexers
               ~key
               ~data:
                 { max_instance_bits = max data.max_instance_bits total_bits
                 ; total_bits = data.total_bits + total_bits
                 ; count = data.count + 1
                 })
      }
  ;;
end

module Memory = struct
  module T = struct
    type t =
      {
        data_width : int
      ; depth : int
      ; total_bits : int
      }
    [@@deriving sexp_of, compare]
  end

  include T
  include Comparable.Make (T)
end

module Memories = struct
  module Mem_map = struct
    type t = int Map.M(Memory).t

    let sexp_of_elt ({ Memory.data_width; depth; total_bits }, count) =
      [%message (data_width : int) (depth : int) (total_bits : int) (count : int)]
    ;;

    let sexp_of_t t = [%sexp (Map.to_alist t : elt list)]
  end

  type t =
    { count : int
    ; total_bits : int
    ; memories : Mem_map.t
    }
  [@@deriving sexp_of]

  let none = { count = 0; total_bits = 0; memories = Map.empty (module Memory) }

  let add ?(u = none) s =
    let data_width = Signal.width s in
    let depth =
      match s with
      | Mem { memory = m; _ } -> m.mem_size
      | Multiport_mem { size; _ } -> size
      | _ -> 0
    in
    let total_bits = depth * data_width in
    let key = { Memory.data_width; depth; total_bits } in
    Some
      { count = u.count + 1
      ; total_bits = u.total_bits + key.total_bits
      ; memories =
          (match Map.find u.memories key with
           | None -> Map.add_exn u.memories ~key ~data:1
           | Some count -> Map.set u.memories ~key ~data:(count + 1))
      }
  ;;
end

module rec Instantiation : sig
  type t =
    | Instantiation of string
    | Submodule of T.t
  [@@deriving sexp_of]
end = struct
  type t =
    | Instantiation of string
    | Submodule of T.t
  [@@deriving sexp_of]
end

and Instantiations : sig
  type t = Instantiation.t list [@@deriving sexp_of]

  val add : ?u:t -> Signal.t -> t option
end = struct
  type t = Instantiation.t list [@@deriving sexp_of]

  let none = []

  let add ?(u = none) s =
    let name =
      match (s : Signal.t) with
      | Inst { instantiation; _ } -> instantiation.inst_name
      | _ -> "<not an instantiation>"
    in
    Some (Instantiation.Instantiation name :: u)
  ;;
end

and T : sig
  type t =
    { name : string
    ; adders : Total_and_max_bits.t option
    ; subtractors : Total_and_max_bits.t option
    ; unsigned_multipliers : Total_and_max_bits.t option
    ; signed_multipliers : Total_and_max_bits.t option
    ; and_gates : Total_bits.t option
    ; or_gates : Total_bits.t option
    ; xor_gates : Total_bits.t option
    ; not_gates : Total_bits.t option
    ; equals : Total_and_max_bits.t option
    ; comparators : Total_and_max_bits.t option
    ; multiplexers : Multiplexers.t option
    ; registers : Total_bits.t option
    ; memories : Memories.t option (* the following do not generate gates. *)
    ; constants : Total_bits.t option
    ; wires : Total_bits.t option
    ; concatenation : Total_bits.t option
    ; part_selects : Total_bits.t option (* (recursive) sub modules. *)
    ; instantiations : Instantiations.t option
    }
  [@@deriving sexp_of]
end = struct
  type t =
    { name : string
    ; adders : Total_and_max_bits.t option [@sexp.option]
    ; subtractors : Total_and_max_bits.t option [@sexp.option]
    ; unsigned_multipliers : Total_and_max_bits.t option [@sexp.option]
    ; signed_multipliers : Total_and_max_bits.t option [@sexp.option]
    ; and_gates : Total_bits.t option [@sexp.option]
    ; or_gates : Total_bits.t option [@sexp.option]
    ; xor_gates : Total_bits.t option [@sexp.option]
    ; not_gates : Total_bits.t option [@sexp.option]
    ; equals : Total_and_max_bits.t option [@sexp.option]
    ; comparators : Total_and_max_bits.t option [@sexp.option]
    ; multiplexers : Multiplexers.t option [@sexp.option]
    ; registers : Total_bits.t option [@sexp.option]
    ; memories : Memories.t option [@sexp.option]
    ; constants : Total_bits.t option [@sexp.option]
    ; wires : Total_bits.t option [@sexp.option]
    ; concatenation : Total_bits.t option [@sexp.option]
    ; part_selects : Total_bits.t option [@sexp.option]
    ; instantiations : Instantiations.t option [@sexp.option]
    }
  [@@deriving sexp_of]
end

type t = T.t [@@deriving sexp_of]

let rec create ?database circuit =
  let expand_submodules t =
    match database with
    | None -> t
    | Some database ->
      { t with
        T.instantiations =
          Option.map t.T.instantiations ~f:(fun instantiations ->
            List.map instantiations ~f:(function
              | Instantiation.Instantiation name as inst ->
                (match Circuit_database.find database ~mangled_name:name with
                 | None -> inst
                 | Some circuit -> Instantiation.Submodule (create ~database circuit))
              | Instantiation.Submodule _ as x -> x))
      }
  in
  Signal_graph.fold
    (Circuit.signal_graph circuit)
    ~init:
      { T.name = Circuit.name circuit
      ; constants = None
      ; adders = None
      ; subtractors = None
      ; unsigned_multipliers = None
      ; signed_multipliers = None
      ; and_gates = None
      ; or_gates = None
      ; xor_gates = None
      ; not_gates = None
      ; equals = None
      ; comparators = None
      ; concatenation = None
      ; multiplexers = None
      ; part_selects = None
      ; wires = None
      ; registers = None
      ; memories = None
      ; instantiations = None
      }
    ~f:(fun utilization signal ->
      match (signal : Signal.t) with
      | Empty -> utilization
      | Const _ ->
        { utilization with constants = Total_bits.add ?u:utilization.constants signal }
      | Wire _ -> { utilization with wires = Total_bits.add ?u:utilization.wires signal }
      | Select _ ->
        { utilization with
          part_selects = Total_bits.add ?u:utilization.part_selects signal
        }
      | Reg _ ->
        { utilization with registers = Total_bits.add ?u:utilization.registers signal }
      | Mem _ | Multiport_mem _ ->
        { utilization with memories = Memories.add ?u:utilization.memories signal }
      | Mem_read_port _ -> utilization
      | Inst _ ->
        { utilization with
          instantiations = Instantiations.add ?u:utilization.instantiations signal
        }
      | Not _ ->
        { utilization with not_gates = Total_bits.add ?u:utilization.not_gates signal }
      | Mux _ ->
        { utilization with
          multiplexers = Multiplexers.add ?u:utilization.multiplexers signal
        }
      | Cat _ ->
        { utilization with
          concatenation = Total_bits.add ?u:utilization.concatenation signal
        }
      | Op2 { op; _ } ->
        (match (op : Signal.signal_op) with
         | Signal_add ->
           { utilization with
             adders = Total_and_max_bits.add ?u:utilization.adders signal
           }
         | Signal_sub ->
           { utilization with
             subtractors = Total_and_max_bits.add ?u:utilization.subtractors signal
           }
         | Signal_mulu ->
           { utilization with
             unsigned_multipliers =
               Total_and_max_bits.add ?u:utilization.unsigned_multipliers signal
           }
         | Signal_muls ->
           { utilization with
             signed_multipliers =
               Total_and_max_bits.add ?u:utilization.signed_multipliers signal
           }
         | Signal_and ->
           { utilization with and_gates = Total_bits.add ?u:utilization.and_gates signal }
         | Signal_or ->
           { utilization with or_gates = Total_bits.add ?u:utilization.or_gates signal }
         | Signal_xor ->
           { utilization with xor_gates = Total_bits.add ?u:utilization.xor_gates signal }
         | Signal_eq ->
           { utilization with
             equals = Total_and_max_bits.add ?u:utilization.equals signal
           }
         | Signal_lt ->
           { utilization with
             comparators = Total_and_max_bits.add ?u:utilization.comparators signal
           }))
  |> expand_submodules
;;
