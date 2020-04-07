open! Import

module type Rtl = sig
  module Language : sig
    type t =
      | Verilog
      | Vhdl
    [@@deriving sexp_of]

    val file_extension : t -> string
    val legalize_identifier : t -> string -> string
  end

  (** RTL generation options. *)
  module Output_mode : sig
    type t =
      | In_directory of string
      (** Write each circuit into a file in the given directory.  The file name consists
          of the circuit name and the approriate file extension ([.v] for Verilog and
          [.vhd] for VHDL). *)
      | To_buffer of Buffer.t (** Write all circuits into one buffer. *)
      | To_channel of Out_channel.t (** Write all circuits to one out channel. *)
      | To_file of string (** Write all circuits into one file. *)
    [@@deriving sexp_of]
  end

  (** Control blackbox generation. [None] implies blackboxes are not used. [Top] means the
      circuit will be turned into a blackbox. [Instantiations] means that the top level
      circuit will be written as normal, but submodules will be written as blackboxes. *)
  module Blackbox : sig
    type t =
      | None
      | Top
      | Instantiations
    [@@deriving sexp_of]
  end

  (** Write circuit to [Verilog] or [Vhdl].  Instantiations are (recursively) looked up in
      [database] and if a circuit exists it is also written.  The [output_mode] specifies
      how the circuit should be written - either to a single file (or buffer, or channel)
      or to a directory with one file for each for the top level circuit and any
      instantiated circuits contained in the database. *)
  val output
    :  ?output_mode:Output_mode.t (** default is [To_file (Circuit.name circuit)]. *)
    -> ?database:Circuit_database.t (** default is an empty database  *)
    -> ?blackbox:Blackbox.t (** Default is [None] *)
    -> Language.t
    -> Circuit.t
    -> unit

  (** [print] is [output ~output_mode:(To_channel stdout)] *)
  val print
    :  ?database:Circuit_database.t
    -> ?blackbox:Blackbox.t (** Default is [None] *)
    -> Language.t
    -> Circuit.t
    -> unit
end
