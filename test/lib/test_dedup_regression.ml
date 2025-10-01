open Core
open! Hardcaml
open! Signal

let set_reg_uid (reg : t) uid : t =
  match reg with
  | Reg reg -> Reg { reg with info = { reg.info with uid } }
  | _ -> raise_s [%message "error"]
;;

(* These are carefully chosen uids which, when set for 2 registers, causes the 2 registers
   to hash to the same thing in [Dedup.signal_hash] *)
let uid1 = 6027779
let uid2 = 213251096

let%expect_test "2 registers with colliding uids doesn't cause the registers to alias" =
  let num_registers c =
    Circuit.signal_graph c |> Signal_graph.filter ~f:Signal.Type.is_reg |> List.length
  in
  let clock = wire 1 -- "clock" in
  let i1 = wire 1 -- "i1" in
  let r1 = reg (Reg_spec.create ~clock ()) i1 -- "r1" in
  let r1 = set_reg_uid r1 (Signal.Type.Uid.Expert.of_int uid1) in
  let i2 = wire 1 -- "i2" in
  let r2 = reg (Reg_spec.create ~clock ()) i2 -- "r2" in
  let r2 = set_reg_uid r2 (Signal.Type.Uid.Expert.of_int uid2) in
  let sum = r1 +: r2 -- "sum" in
  let circuit =
    Circuit.create_exn
      ~config:{ Circuit.Config.default with normalize_uids = false }
      ~name:"test"
      [ output "out" sum ]
  in
  let deduped_circuit = Dedup.deduplicate circuit in
  let num_registers_in_original_circuit = num_registers circuit in
  let num_registers_in_deduped_circuit = num_registers deduped_circuit in
  if num_registers_in_original_circuit <> num_registers_in_deduped_circuit
  then
    raise_s
      [%message
        "Number of registers changed in deduped circuit"
          (num_registers_in_original_circuit : int)
          (num_registers_in_deduped_circuit : int)]
;;

(* This test must pass for the above test to actually test the regression *)
let%test_unit "hash collision" =
  let create_reg_signal_with_uid ~uid =
    Signal.Type.map_info
      (Signal.reg (Signal.Reg_spec.create () ~clock:Signal.vdd) Signal.vdd)
      ~f:(fun signal_info -> { signal_info with uid = Signal.Type.Uid.Expert.of_int uid })
  in
  let hash1 =
    Dedup.For_testing.signal_hash
      (Hashtbl.create (module Signal.Type.Uid))
      (create_reg_signal_with_uid ~uid:uid1)
  in
  let hash2 =
    Dedup.For_testing.signal_hash
      (Hashtbl.create (module Signal.Type.Uid))
      (create_reg_signal_with_uid ~uid:uid2)
  in
  assert (hash1 = hash2)
;;
