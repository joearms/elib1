.SUFFIXES: .erl .beam .yrl

MODS := $(wildcard *.erl)

%.beam: %.erl
	erlc  -W $<

all: beam
	erl -noshell -s make_html start -s init stop

beam: ${MODS:%.erl=%.beam}

clean:
	rm -rf *.beam 
	rm -rf html/*.html 
	rm -rf *.log *.tmp erl_crash.dump

veryclean:
	make clean
	rm -rf *~ 
 





