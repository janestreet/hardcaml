open Base

module Collision_mode = struct
  module T = struct
    type t =
      | Read_before_write
      | Write_before_read
    [@@deriving sexp_of, compare]
  end

  include T
  include Comparable.Make (T)
end

let if_write_before_read_mode ~collision_mode (r : _ Read_port.t array) =
  match (collision_mode : Collision_mode.t) with
  | Write_before_read ->
    Array.map r ~f:(fun r ->
      Signal.reg
        (Reg_spec.create () ~clock:r.read_clock)
        ~enable:r.read_enable
        r.read_address)
  | Read_before_write -> Array.map r ~f:(fun r -> r.read_address)
;;

let if_read_before_write_mode
  ~collision_mode
  (r : _ Read_port.t array)
  (q : Signal.t array)
  =
  match (collision_mode : Collision_mode.t) with
  | Write_before_read -> q
  | Read_before_write ->
    Array.map2_exn r q ~f:(fun r q ->
      Signal.reg (Reg_spec.create () ~clock:r.read_clock) ~enable:r.read_enable q)
;;

let create ?attributes ?name ~collision_mode ~size ~write_ports ~read_ports () =
  Signal.multiport_memory
    ?attributes
    ?name
    size
    ~write_ports
    ~read_addresses:(if_write_before_read_mode ~collision_mode read_ports)
  |> if_read_before_write_mode ~collision_mode read_ports
;;
