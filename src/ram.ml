open! Import

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

module Write_port = struct
  type t = Signal.write_port =
    { write_clock : Signal.t
    ; write_address : Signal.t
    ; write_enable : Signal.t
    ; write_data : Signal.t
    }

  let sexp_of_t t =
    let open Signal in
    [%message
      ""
        (t.write_clock : Signal.t)
        (t.write_address : Signal.t)
        (t.write_enable : Signal.t)
        (t.write_data : Signal.t)]
  ;;
end

module Read_port = struct
  type t = Signal.read_port =
    { read_clock : Signal.t
    ; read_address : Signal.t
    ; read_enable : Signal.t
    }
  [@@deriving sexp_of]

  let sexp_of_t t =
    let open Signal in
    [%message
      "" (t.read_clock : Signal.t) (t.read_address : Signal.t) (t.read_enable : Signal.t)]
  ;;
end

let if_write_before_read_mode ~collision_mode (r : Read_port.t array) =
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
      (r : Read_port.t array)
      (q : Signal.t array)
  =
  match (collision_mode : Collision_mode.t) with
  | Write_before_read -> q
  | Read_before_write ->
    Array.map2_exn r q ~f:(fun r q ->
      Signal.reg (Reg_spec.create () ~clock:r.read_clock) ~enable:r.read_enable q)
;;

let create ~collision_mode ~size ~write_ports ~read_ports =
  Signal.multiport_memory
    size
    ~write_ports
    ~read_addresses:(if_write_before_read_mode ~collision_mode read_ports)
  |> if_read_before_write_mode ~collision_mode read_ports
;;
