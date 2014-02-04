.SUFFIXES: .erl .beam .yrl

MODS := $(wildcard *.erl)

%.beam: %.erl
	erlc  -W $<

server: beam

beam: ${MODS:%.erl=%.beam}

clean:
	rm -rf *.beam
	rm -rf *.log *.tmp erl_crash.dump

veryclean:
	make clean
	rm -rf *~






