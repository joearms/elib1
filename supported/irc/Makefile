.SUFFIXES: .erl .beam .yrl

MODS := $(wildcard *.erl)

%.beam: %.erl
	erlc  -W $<

all: server

server: beam
	erl -s ircd start_server

client: beam
	erl -noshell -s irc_text_client local

irc: beam
	erl -noshell -s irc start

test: beam
	erl -noshell -s irc test12

chat: beam
	erl -s chat_widget test1

widgets: beam
	erl -s widget_lib test

beam: ${MODS:%.erl=%.beam}

clean:
	rm -rf *.beam 
	rm -rf *.log *.tmp erl_crash.dump

veryclean:
	make clean
	rm -rf *~ 




