namespace eval chat {}

proc chat::mkList { w } {
    listbox $w -bd 5
    pack $w -fill both -expand 1
    bind $w <Double-1> "chat::listBoxClicked $w"
}

proc chat::listBoxClicked { w  } {
    ## sendToErlang "event $w listbox [selection get]"
    sendToErlang "event $w listbox {[selection get]}"
}

## makes a listbox with a scrollbar

proc chat::mkListScroll { w } {
    frame $w
    listbox $w.list -yscrollcommand "$w.scr set"
    puts "creating listbox $w.list"
    scrollbar $w.scr -orient vertical -command "$w.list yview"
    pack $w.scr -side right -fill y
    pack $w.list -fill both -expand 1
    bind $w.list <Double-1> {puts "event %W {[selection get]}"}
    return $w
}

proc chat::mkEditor { w } {
    frame $w.f -bd 5
    pack $w.f -fill both -expand 1

    text $w.f.txt 
    puts "text widget is $w.f.txt"
    scrollbar $w.f.scroll -orient vertical

    pack $w.f.scroll -side right -fill y
    pack $w.f.txt -fill both -expand 1
    
    $w.f.txt config -yscrollcommand "$w.f.scroll set" 
    $w.f.scroll config -command "$w.f.txt yview"
}

## image create photo rball -file "./redball.gif"
## image create photo wball -file "./whiteball.gif"
    
proc chat::readEntry { w f } {
    set val [$f get]
    sendToErlang "event $w entry $val"
}

proc chat::make_chat { w } {
    ## make a notebook
    ttk::notebook $w.n
    puts "notepad is $w.n"
    ## and an entry
    entry $w.entry -bd 5
    $w.entry insert 0 "hello joe"
    bind $w.entry <Return> "chat::readEntry $w $w.entry"
    chat::mkListScroll $w.lb

    grid $w.n  -column 0 -row 0 -columnspan 5 -sticky nsew
    grid $w.lb -column 5 -row 0 -sticky nsew
    grid $w.entry -column 0 -row 2 -columnspan 6 -sticky nsew

    grid columnconfigure $w 0 -weight 1
    grid rowconfigure $w 0 -weight 1
    bind $w <<NotebookTabChanged>> "chat::tabclicked $w %W"
}

proc chat::add_listbox { w t } {
    $w.lb.list insert end $t
}

proc chat::add_tab { w t txt} {
    ttk::frame $w.n.$t
    ## puts "add $w.n and $w.n.$t"
    $w.n add $w.n.$t -c right -text $txt -image wball
    chat::mkEditor $w.n.$t 
} 

proc chat::delete_tab { w t } {
    $w.n forget $w.n.$t
    destroy $w.n.$t
}

proc chat::tabclicked { w x} {
    set s [$x select]
    ## s is something like .w1.n.f2
    ## set ret = f2
    set i [expr [string last "." $s] + 1]
    set ret [string range $s $i end]
    sendToErlang "event $w clicked $ret"
}

# http://coding.derkeiler.com/Archive/Tcl/comp.lang.tcl/2005-08/msg00685.html
# Bryan Oakley

# add text follows end of text if the scollbar is at the
# bottom of the window

proc chat::add_content {w t txt} {
    set yview [$w.n.$t.f.txt yview]
    if {[lindex $yview 1] >= 1.0} {
      set scroll 1
    } else {
      set scroll 0
    }
    $w.n.$t.f.txt insert end $txt
    if {$scroll} { $w.n.$t.f.txt see end}
  }

# proc chat::add_content { w t txt } {
#     puts "text is $w.n.f1.$t.f.txt"
#     $w.n.$t.f.txt insert end $txt
# }

proc chat::toggle { w t } {
    set s [$w.n tab $w.n.$t -image]
    ## puts "image $s"
    if { $s == "rball"} {
	$w.n tab $w.n.$t -image wball
    } else {
	$w.n tab $w.n.$t -image rball
    }
}

proc chat::set_entry { w txt } {
    $w.entry delete 0 end
    $w.entry insert end $txt
}

proc chat::set_color { w t s } {
    if { $s == "white"} {
	$w.n tab $w.n.$t -image wball
    } elseif { $s == "red" } {
	$w.n tab $w.n.$t -image rball
    }
}

proc chat::set_title { w t } {
    wm title $w $t
}
