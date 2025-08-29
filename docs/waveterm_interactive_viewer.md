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

```ocaml
# Hardcaml_waveterm_interactive.print_key_help ();;
              Escape    quit
                  Up    scroll up
                Down    scroll down
             Ctrl Up    scroll up fast
           Ctrl Down    scroll down fast
                   ?    show help
                   =    cursor centered zoom in
                   -    cursor centered zoom out
               Alt =    cursor centered zoom in fast
               Alt -    cursor centered zoom out fast
                   9    expand signals window
                   (    shrink signals window
                   0    expand values window
                   )    shrink values window
                Home    scroll wave to beginning
                 End    scroll wave to end
           Ctrl Home    scroll to first signal
            Ctrl End    scroll to last signal
                Left    scroll left
               Right    scroll right
           Ctrl Left    scroll left fast
          Ctrl Right    scroll right fast
                   k    move cursor up
                   j    move cursor down
                   h    move cursor left
                   l    move cursor right
                   K    move cursor up fast
                   J    move cursor down fast
                   H    move cursor left fast
                   L    move cursor right fast
                   g    center on cursor
                   G    set cursor to first wave on screen
                   1    select cursor 1
                   2    select cursor 2
                   3    select cursor 3
                   4    select cursor 4
                   5    select cursor 5
               Alt 1    select cursor 1 and center
               Alt 2    select cursor 2 and center
               Alt 3    select cursor 3 and center
               Alt 4    select cursor 4 and center
               Alt 5    select cursor 5 and center
                   r    remove selected cursor
               Enter    maybe toggle module
                   s    save ui state
                   S    load ui state
                   e    search for transition forward
                   b    search for transition backward
                   f    cycle wave format
                   F    reset wave format
                   c    cycle colour
                   C    toggle bold
- : unit = ()
```

Left clicking on the waveform will position the active cursor and show the value of
signals at that cycle in the values window. Cycle deltas between different cursors is
shown in the wave view title.

All windows also have scroll bars that can be clicked on.

A `.hardcamlwavetermrc` can be present in the current directory, your home directory or
found via the `HARDCAMLWAVETERMRC` environment variable to configure key bindings.

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
