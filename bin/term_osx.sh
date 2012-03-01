#!/bin/sh
#
# Open a new Mac OS X terminal window with the command given
# as argument.
#
# - If there are no arguments, the new terminal window will
#   be opened in the current directory, i.e. as if the command
#   would be "cd `pwd`".
# - If the first argument is a directory, the new terminal will
#   "cd" into that directory before executing the remaining
#   arguments as command.
# - If there are arguments and the first one is not a directory,
#   the new window will be opened in the current directory and
#   then the arguments will be executed as command.
# - The optional, leading "-x" flag will cause the new terminal
#   to be closed immediately after the executed command finishes.
#
# Written by Marc Liyanage <http://www.entropy.ch>
#
# Version 1.0
#

if [ "x-x" = x"$1" ]; then
    EXIT="; exit"; shift;
fi

if [[ -d "$1" ]]; then
    WD=`cd "$1"; pwd`; shift;
else
    WD="'`pwd`'";
fi

COMMAND="cd $WD; $@"
echo "$COMMAND $EXIT"

osascript 2>/dev/null <<EOF
    tell application "Terminal"
        activate
        do script with command "$COMMAND $EXIT"
    end tell
EOF
