set s "This is a demonstration of a notepad widget.

This editor will resize when you change resize the outer window

This can be launched with the command 

   > wish -f chat_best.tcl

This is the basic unadulternated TCL - the adapted widget desiged
to be used with Erlang is in the file chat_erl.tcl

    namespace eval mypackage {\}

    Then create the procedures using their fully qualified name as such:

    proc mypackage::get {\} {\}
    proc mypackage::set {\} {\}

Lorem ipsum dolor sit amet, consectetur adipisicing elit, 
sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. 
Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris 
nisi ut aliquip ex ea commodo consequat. 
Duis aute irure dolor in reprehenderit in voluptate velit 
esse cillum dolore eu fugiat nulla pariatur. 
Excepteur sint occaecat cupidatat non proident, 
sunt in culpa qui officia deserunt mollit anim id est laborum.
"

proc mkList { w } {
    listbox $w -bd 5

    puts "creating listbox $w"
    pack $w -fill both -expand 1
    bind $w <Double-1> "listBoxClicked $w"
}

proc listBoxClicked { w  } {
    ## sendToErlang "event $w listbox [selection get]"
    puts "event $w listbox [selection get]"
}

## makes a listbox with a scrollbar

proc mkListScroll { w } {
    frame $w
    listbox $w.list -yscrollcommand "$w.scr set"
    puts "creating listbox $w.list"
    scrollbar $w.scr -orient vertical -command "$w.list yview"
    pack $w.scr -side right -fill y
    pack $w.list -fill both -expand 1
    bind $w.list <Double-1> {listBoxClicked $w [selection get]}
    return $w
}

proc mkEditor { w } {
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

image create photo rball -file "redball.gif"
image create photo wball -file "whiteball.gif"

toplevel .w1

ttk::notebook .w1.n

ttk::frame .w1.n.f1
.w1.n add .w1.n.f1 -c right -text "One" -image wball
mkEditor .w1.n.f1
.w1.n.f1.f.txt insert end $s
.w1.n.f1.f.txt insert end $s
.w1.n.f1.f.txt insert end $s
.w1.n.f1.f.txt insert end $s

ttk::frame .w1.n.f2
.w1.n add .w1.n.f2 -c right -text "Two" -image wball
mkEditor .w1.n.f2
.w1.n.f2.f.txt insert end "A much shorter text"

ttk::frame .w1.n.f3
.w1.n add .w1.n.f3 -c right -text "Commands" -image rball
mkEditor .w1.n.f3
.w1.n.f3.f.txt insert end {
To test the commands:
$ wish
source test4.tcl
.w1.n tab .w1.n.f3 -image rball
}

proc readEntry { w f } {
    set val [$f get]
    puts "$w entry $val"
}


## Now add an entry

entry .w1.entry -bd 5
.w1.entry insert 0 "hello joe"
bind .w1.entry <Return> "readEntry .w1 .w1.entry"

mkList .w1.lb

.w1.lb insert end {joe}
.w1.lb insert end {jim}
.w1.lb insert end {jane}

## Now we have three object all we have to do is pack them

#pack .w1.n .w1.lb .w1.entry

grid .w1.n  -column 0 -row 0 -columnspan 5 -sticky nsew
grid .w1.lb -column 5 -row 0 -sticky nsew
grid .w1.entry -column 0 -row 2 -columnspan 6 -sticky nsew

grid columnconfigure .w1 0 -weight 1
grid rowconfigure .w1 0 -weight 1


proc myprog { x } {
    set s [$x select]
    puts "Tab Selected $s"
}

bind .w1 <<NotebookTabChanged>> "myprog %W"

wm withdraw .


