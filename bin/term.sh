#!/bin/sh
#
# Open a new Mac OS X terminal window or tab in the current or another
# directory and optionally run a command in the new window or tab.
#
# - Without any arguments, the new terminal window opens in
#   the current directory, i.e. the executed command is "cd $PWD".
# - If the first argument is a directory, the new terminal will "cd" into
#   that directory before executing the remaining arguments as command.
# - The optional "-t" flag executes the command in a new tab 
#   instead of a new window.
# - The optional "-x" flag closes the new window or tab
#   after the executed command finishes.
# - The optional "-p" flag takes an argument of the form x,y (e.g. 40,50) and
#   positions the terminal window to the indicated location on the screen
# - The optional "-s" flag takes an argument of the form w,h (e.g. 800,400) and
#   resizes the terminal window to the indicated width and height in pixels.
#
# Written by Marc Liyanage <http://www.entropy.ch>
#
# Version 2.1
#

set -e

while getopts xtp:s: OPTION; do
[ $OPTION = "x" ] && { EXIT='; exit'; }
[ $OPTION = "t" ] && { TAB=1; }
[ $OPTION = "p" ] && { POSITION="set position of window 1 to {$OPTARG}"; }
[ $OPTION = "s" ] && { SIZE="set size of window 1 to {$OPTARG}"; }
done

for (( $OPTIND; $OPTIND-1; OPTIND=$OPTIND-1 )); do shift; done

if [[ -d "$1" ]]; then WD=$(cd "$1"; pwd); shift; else WD=$PWD; fi


COMMAND="cd '$WD' && echo -n \$'\\\\ec';"
for i in "$@"; do
	COMMAND="$COMMAND '$i'"
done

if [ $TAB ]; then

osascript 2>/dev/null <<EOF
	tell application "System Events"
		tell process "Terminal" to keystroke "t" using command down
	end
	tell application "Terminal"
		activate
		do script with command "$COMMAND $EXIT" in window 1
		$POSITION
		$SIZE
	end tell
EOF

else

osascript <<EOF
	tell application "Terminal"
		activate
		do script with command "$COMMAND $EXIT"
		$POSITION
		$SIZE
	end tell
EOF

fi
