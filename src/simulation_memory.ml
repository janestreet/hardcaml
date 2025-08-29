open! Core0

type 'a t =
  | Array of 'a Array.t
  | Cyclesim_memory of
      { memory : Cyclesim.Memory.t
      ; of_bits : Bits.t -> 'a
      }

let of_evsim_memory array = Array array
let of_cyclesim_memory memory ~of_bits = Cyclesim_memory { memory; of_bits }

let length = function
  | Array array -> Array.length array
  | Cyclesim_memory { memory; of_bits = _ } -> Cyclesim.Memory.memory_size memory
;;

let get t idx =
  match t with
  | Array array -> array.(idx)
  | Cyclesim_memory { memory; of_bits } ->
    of_bits (Cyclesim.Memory.to_bits memory ~address:idx)
;;
