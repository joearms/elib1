proc sendToErlang { x } {
    puts "$x"
}

source "chat_erl.tcl"

image create photo rball -file "./redball.gif"
image create photo wball -file "./whiteball.gif"

wm withdraw .

proc windowDestroyed { w } {
    puts "$w is destoyed"
}

toplevel .w1
bind .w1 <Destroy> "windowDestroyed %W"

chat::make_chat .w1
chat::add_tab .w1 f1 "One"
chat::add_content .w1 f1 {This is some content}

chat::add_tab .w1 f2 "Two"
chat::add_content .w1 f2 {This is some other content}

chat::add_tab .w1 f3 "Three"
chat::add_content .w1 f2 {This is tab 3
and some other content}

chat::add_listbox .w1 {joe}
chat::add_listbox .w1 {james the first}

chat::set_title .w1 {This is my window}

## make a test widget
toplevel .w2
button .w2.b1 -text "toggle one" -command "chat::toggle .w1 f1"
button .w2.b2 -text "add text to one" -command "chat::add_content .w1 f1 {\nhello}"
button .w2.b3 -text "set entry" -command "chat::set_entry .w1 {some stuff}"
button .w2.b4 -text "set one red" -command "chat::set_color .w1 f1 red"
button .w2.b5 -text "set one white" -command "chat::set_color .w1 f1 white"
button .w2.b6 -text "delete tab 3" -command "chat::delete_tab .w1 f3"
button .w2.b7 -text "add tab 3" -command "chat::add_tab .w1 f3 New"

pack .w2.b1 .w2.b2 .w2.b3 .w2.b4 .w2.b5 .w2.b6 .w2.b7 -fill both -expand 1


