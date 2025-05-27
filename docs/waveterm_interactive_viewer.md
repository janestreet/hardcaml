# 3.3 Interactive Viewer

Writing waveforms in expect tests can be very useful, but does come with it's
limitations. In particular it is not really possible to print very long traces, and
designs with lots and lots of signals can become unwieldy.

As an alternative we can use the terminal based interactive waveform viewer.

# Building a Simulation Application

The interactive waveform viewer should be built as a top level application. It should be
linked with the
[`hardcaml_waveterm_interactive`](https://github.com/janestreet/hardcaml_waveterm/tree/master/interactive)
library.

Waveforms are created in the normal way by wrapping a simulator with
`Hardcaml_waveterm.Wavform.create` and then running a testbench.

The following function is used to run the viewer.

```ocaml skip
# Hardcaml_waveterm_interactive.run waves
```


# Using the Waveform Viewer

* `q/escape` quits
* `left/right` or `ctrl+mouse-wheel` scroll waveform horizontally
* `up/down` or `mouse-wheel` scroll waveform vertically
* `-/=` decrease/increase waveform scale
* `e/b` will find the next/prev transition on the highlighted signal
* `s/S` save or load the current waveform configuration
* `h/j/k/l` move the cursor
* `9/(` - increase/decrease the signals pane size 
* `0/)` - increase/decrease the values pane size
* `_/+` - increase/decrease the waves pane size

Left clicking on the waveform will position a cursor and show the value of signals at that
cycle in the values window.

All windows also have scroll bars that can be clicked on.

# Interactive Waveforms from Expect Tests

Waveform-based expect tests are very information limited. We often either run very small
simulations, show a small portion of a larger simulation, or zoom far out to capture
general behavior. Often we can see the behavior of control signals but not the detailed
values on data buses.

To help with this, the `Hardcaml_waveterm` library can optionally serialize a waveform to
disk. It provides an `expect` function which is very similar to
`Hardcaml_waveterm.Waveform.print`

```ocaml skip
Hardcaml_waveterm.Waveform.expect ~serialize_to:"filename" waves
```

This provides an optional argument called `serialize_to` which specifies the filename to
which the waveform will be saved. The suffix `.hardcamlwaveform` will be added.

By default, waveforms are not saved. To do so execute the tests with the following
environment variable set.

```
EXPECT_TEST_WAVEFORM=1
```

Which will output a file in the same directory called `filename.hardcamlwaveform`.

To view the waveform, a tool called `waveform_viewer` is provided with `hardcaml_waveterm`
in the `bin` directory.

```
waveform_viewer.exe show filename.hardcamlwaveform
```
