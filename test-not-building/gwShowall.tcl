# Script to automatically add all design signals to the wave window on startup;
# ie
# > gtkwave -S gwShowall.tcl test.vcd
# > testprog | gtkwave -v -S gwShowall.tcl 

set nsigs [ gtkwave::getNumFacs ]
set sigs [list]
for {set i 0} {$i < $nsigs} {incr i} {
    set name  [ gtkwave::getFacName $i ] 
    lappend sigs $name
}

set added [ gtkwave::addSignalsFromList $sigs ]

