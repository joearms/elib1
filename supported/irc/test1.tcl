proc mkListScroll { w } {
    frame $w
    listbox $w.list -yscrollcommand "$w.scr set"
    puts "creating listbox $w.list"
    scrollbar $w.scr -orient vertical -command "$w.list yview"
    pack $w.scr -side right -fill y
    pack $w.list -fill both -expand 1
    bind $w.list <Double-1> {listBoxClicked %W [selection get]}
    return $w
}

proc listBoxClicked { w  } {
    ## sendToErlang "event $w listbox [selection get]"
    puts "event $w listbox [selection get]"
}

mkListScroll .lb
pack .lb


.lb.list insert end {one}
.lb.list insert end {two}
.lb.list insert end {three}
.lb.list insert end {four}
