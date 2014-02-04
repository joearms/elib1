.SUFFIXES: .erl .beam .yrl

DOCS := $(wildcard *.ehtml)

MODS := $(wildcard *.erl)

%.beam: %.erl
	erlc  -W $<

%.html: %.ehtml
	erl -noshell -s elib1_doc batch $< -s init stop


all: beam html


beam: ${MODS:%.erl=%.beam}

html: ${DOCS:%.ehtml=%.html}

clean:
	rm -rf *.aux *.beam
	rm -rf  ${DOCS:%.ehtml=%.html}
	rm -rf *.log *.tmp erl_crash.dump

veryclean:
	rm ../bin/* ../doc/* ../tmp/*





