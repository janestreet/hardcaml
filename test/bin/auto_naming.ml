let () =
  let waves = Hardcaml_test.Test_auto_names.sim () in
  Hardcaml_waveterm_interactive.run waves
;;
