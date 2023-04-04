# Waveterm Interactive Viewer

Normally we write expect_tests which can output waveforms in ASCII
when testing Hardcaml, but we can also use the interactive viewer tool
to view waveforms that run for many cycles or
have a lot of signals. This is a very useful debugging tool
when initially designing a hardcaml circuit.

## Buliding a simulation application

The [`hardcaml_waveterm_interactive`](https://github.com/janestreet/hardcaml_waveterm/tree/master/interactive)
library can be used to build an
application which includes a hardware circuit that can be simulated
and the results viewed with the interactive viewer.

A waveform is created in the normal way by wrapping a simulator with
`Hardcaml_waveterm.create` and then running a testbench.

The following function is used to display the waveform.

```ocaml skip
# Hardcaml_waveterm_interactive.run waves
```


## Using the waveform viewer

* `q` quits
* `left/right` or `ctrl+mouse-wheel` scroll waveform horizontally
* `up/down` or `mouse-wheel` scroll waveform vertically
* `-/=` decrease/increase waveform scale

Left clicking on the waveform will position a cursor and show the
value of all signals in the values window.

All windows also have scroll bars that can be clicked on.

## Interactive waveforms from expect tests

Waveform-based expect tests are very information limited. We often
either run very small simulations, show a small portion of a larger
simulation, or zoom far out to capture general behaviour. Often we can
see the behaviour of control signals but not the detailed values on
data buses.

To help with this, the `Hardcaml_waveterm` library can optionally
serialize a waveform to disk. It provides an `expect` function which is
very similar to `Hardcaml_waveterm.Waveform.print`

```ocaml skip
Hardcaml_waveterm.Waveform.expect ~serialize_to:"filename" waves
```

This provides an optional argument called `serialize_to` which
specifies the filename to which the waveform will be saved. The suffix
`.hardcamlwaveform` will be added.

By default, waveforms are not saved. To do so execute the tests with
the following environment variable set.

```
EXPECT_TEST_WAVEFORM=1 ./inline_tests_runner
```

Which will output a file in the same directory called
`filename.hardcamlwaveform`.

To view the waveform, a tool called `waveform_viewer` is provided with
`hardcaml_waveterm` in the `bin` directory.

```
waveform_viewer.exe show filename.hardcamlwaveform
```
