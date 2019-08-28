include Base
include Hardcaml
module Command = Core.Command
module Out_channel = Stdio.Out_channel
module Test_bits = Hardcaml_test.Test_bits

let fprintf = Out_channel.fprintf
let stdout = Stdio.stdout
