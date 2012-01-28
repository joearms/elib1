wm withdraw "."

## debug code first so we can see what is happening

proc puti {str} {
    .debug.txt insert end "$str\n"
 }

proc mkdebug {} {
    toplevel .debug
    set w .debug
    text $w.txt -bd -0 -width 80
    scrollbar $w.scroll
    pack $w.scroll -side right -fill y
    $w.txt config -yscrollcommand "$w.scroll set"
    $w.scroll config -command "$w.txt yview"
    pack $w.txt -fill both -expand 1
}

proc read_sock {wsock} {
    # puti "read_sock"
    global  eventLoop
    binary scan [read $wsock 4] I len
    if {[eof $wsock]} {
	catch {close $wsock}
	exit
    }
    # puti "LEN $len"
    # FIXME need to read again if less then $len ?????
    set command [read $wsock $len]
    if {[eof $wsock]} {
	catch {close $wsock}
	exit
    }
    # report {"INMSG $command EOMSG"}
    # set l [gets $wsock]
    if {[eof $wsock]} {
	close $wsock             ;# close the socket client connection
	set eventLoop "done"     ;# terminate the vwait (eventloop)
    } else {
	## uncomment for debugging
	## puti "fromErl: $command"
	set ret [catch $command x]
	## puti "catch return ret=$ret x=$x"
    }
}


proc sendToErlang { msg } {
    global sock
    set len [string length $msg]
    set x  [binary format Ia* $len $msg]
    puts -nonewline $sock $x
}

proc ecall { n cmd } {
    global x
    global errorInfo
    ## puti "in  ecall cmd=$cmd"
    set ret [catch $cmd val]
    if { $ret == 0} {
	sendToErlang "eret  $n $val"
    } else {
	sendToErlang "error $n $errorInfo"
    }
}

proc ecast { n cmd } {
    global x
    global errorInfo
    set errorInfo "none"
    ## puti "in ecast cmd=$cmd"
    set ret [catch $cmd val]
    ## puti "ecast n=$n ret = $ret $errorInfo"
    if { $ret != 0} {
	sendToErlang "notify $n $cmd $errorInfo"
    }
  update
}

proc windowDestroyed { win } {
    sendToErlang "winDestroyed $win"
}

mkdebug

## puti "connect to port $argv"

## open the socket the indicated port on localhost

set sock [socket localhost $argv]

fconfigure $sock -buffering none -blocking true \
	-translation binary -encoding binary

fileevent $sock readable [list read_sock $sock]

## puti "sending sync to erlang"

sendToErlang "magic sync"


