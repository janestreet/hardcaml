(** Icarus Verilog Cosimulation interface (DEPRECATED) *)

open! Import

(** run [sets], then [gets] then schedule next callback at cur_time+[delta_time]*)
type delta_message =
  { sets : (string * string) list
  ; gets : string list
  ; delta_time : int64
  }

(** expected inputs and outputs *)
type init_message = string list

(** control message *)
type control_message =
  | Finish
  | Run of delta_message

(** response message *)
type response_message = (string * string) list

(* XXX to parameterise *)
val net_addr : string
val net_port : int

(** basic TCP communications between client (simulation) and server (hardcaml) *)
module Comms : sig
  open Unix

  val create_client : string -> int -> file_descr
  val create_server : string -> int -> file_descr
  val accept_client : file_descr -> file_descr
  val send : file_descr -> string -> int
  val recv : file_descr -> string
  val send_string : file_descr -> string -> int
  val recv_string : file_descr -> string
  val recv_string_is : file_descr -> string -> unit
end

(** send a control message to the simulation *)
val control : Unix.file_descr -> control_message -> response_message

(** write test harness *)
val write_testbench
  :  ?dump_file:string
  -> name:string
  -> inputs:(string * int) list
  -> outputs:(string * int) list
  -> (string -> unit)
  -> unit

(** write test hardness derivied from a hardcaml circuit *)
val write_testbench_from_circuit
  :  ?dump_file:string
  -> (string -> unit)
  -> Circuit.t
  -> unit

(** compile verilog files to a vvp simulation object *)
val compile : string list -> string -> unit

(** find clocks and resets in a hardcaml circuit *)
val derive_clocks_and_resets : Circuit.t -> string list * string list

(** load vvp file into simulator along with vpi object *)
val load_sim : string -> unit

(** compile circuit and load simulation *)
val compile_and_load_sim : ?dump_file:string -> Circuit.t -> unit

val init_sim
  :  (unit -> unit)
  -> (string * int) list
  -> (string * int) list
  -> Unix.file_descr

val make_sim_obj
  :  server:Unix.file_descr
  -> clocks:(string * int) list
  -> resets:(string * int) list
  -> inputs:(string * int) list
  -> outputs:(string * int) list
  -> Cyclesim.t_port_list


(** create simulator from hardcaml circuit *)
val make : ?dump_file:string -> Circuit.t -> Cyclesim.t_port_list

(** load icarus vvp simulation *)
val load
  :  clocks:(string * int) list
  -> resets:(string * int) list
  -> inputs:(string * int) list
  -> outputs:(string * int) list
  -> string
  -> Cyclesim.t_port_list

module With_interface (I : Interface.S) (O : Interface.S) : sig
  (** Create a co-simulator using the provided [Create_fn].  The returned simulator ports
      are coerced to the input and output interface types. *)
  val create
    : (?vcd_file_name:string
       -> ?port_checks:Circuit.Port_checks.t
       -> ?add_phantom_inputs:bool
       -> Circuit.With_interface(I)(O).create
       -> Cyclesim.With_interface(I)(O).t)
        Circuit.with_create_options
end
