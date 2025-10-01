open! Core0
open Signal

module Collision_mode = struct
  module T = struct
    type t =
      | Read_before_write
      | Write_before_read
    [@@deriving sexp_of, compare ~localize]
  end

  include T

  include%template Comparable.Make_plain [@mode local] (T)
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

module Dual_port = struct
  module type Config = sig
    val address_bits : int
    val data_bits : int
  end

  module Make (Config : Config) = struct
    module Port = struct
      type 'a t =
        { address : 'a [@bits Config.address_bits]
        ; data : 'a [@bits Config.data_bits]
        ; enable : 'a
        ; write : 'a
        }
      [@@deriving hardcaml]
    end

    module I = struct
      type 'a t =
        { clock : 'a
        ; port_a : 'a Port.t
        ; port_b : 'a Port.t
        }
      [@@deriving hardcaml ~rtlmangle:"_"]
    end

    module O = struct
      type 'a t =
        { qa : 'a [@bits Config.data_bits]
        ; qb : 'a [@bits Config.data_bits]
        }
      [@@deriving hardcaml]
    end

    let create ?attributes ?name ?size _scope (i : _ I.t) =
      let size = Option.value ~default:(1 lsl Config.address_bits) size in
      let q =
        (* Implements [No_change] mode - the read output is not updated when writing. This
           is the "best" mode for Xilinx rams. *)
        let read_a = i.port_a.enable &: ~:(i.port_a.write) in
        let write_a = i.port_a.enable &: i.port_a.write in
        let read_b = i.port_b.enable &: ~:(i.port_b.write) in
        let write_b = i.port_b.enable &: i.port_b.write in
        create
          ?attributes
          ?name
          ~collision_mode:Read_before_write
          ~size
          ~read_ports:
            [| { read_clock = i.clock
               ; read_address = i.port_a.address
               ; read_enable = read_a
               }
             ; { read_clock = i.clock
               ; read_address = i.port_b.address
               ; read_enable = read_b
               }
            |]
          ~write_ports:
            [| { write_clock = i.clock
               ; write_enable = write_a
               ; write_address = i.port_a.address
               ; write_data = i.port_a.data
               }
             ; { write_clock = i.clock
               ; write_enable = write_b
               ; write_address = i.port_b.address
               ; write_data = i.port_b.data
               }
            |]
          ()
      in
      { O.qa = q.(0); qb = q.(1) }
    ;;

    let hierarchical
      ?attributes
      ?(name = "dual_port_inferred")
      ?instance
      ?memory_attributes
      ?memory_name
      ?size
      scope
      =
      let module Hier = Hierarchy.In_scope (I) (O) in
      Hier.hierarchical
        ?attributes
        ?instance
        ~name
        ~scope
        (create ?attributes:memory_attributes ?size ?name:memory_name)
    ;;
  end
end
