.SUFFIXES: .erl .beam .yrl

MODS := $(wildcard *.erl)
YRL := $(wildcard *.yrl)
CHAPS := $(wildcard *.chap)
BOOKS := $(wildcard *.book)
LOGS := $(wildcard *.log)

CWD := $(shell pwd)

../ebin/%.beam: %.erl
	## erlc +warn_missing_spec -o ../ebin -W $< 
	##  grep --silent --invert-match "_test"
	erlc -o ../ebin -W $< 

../ebin/%.beam: ../tmp/%.erl
	erlc -o ../ebin -W $<

../doc/%.html: %.chap
	@erl -noshell -pa ../ebin -s elib1_docmaker batch $< -s init stop

../doc/%.html: %.log
	@erl -noshell -pa ../ebin -s elib1_chunks batch $< -s init stop

../doc/%.html: %.erl
	./mkdoc $<

../tmp/%.erl: %.yrl
	erlc -o ../tmp -W $<

../../pdf/%.pdf: %.chap
	erl -s elib1_doc batch $<
	fop -xml ../tmp/$*.xml -xsl chap2pdf.xsl -pdf ../../pdf/$*.pdf

# ../../html/%.html: %.chap
# 	erl -s elib1_doc batch $<
# 	fop -xml ../tmp/$*.xml -xsl chap2html.xsl -txt  ../../html/$*.html

all: yecc beam html #chapHTML

test:
	dialyzer -Wno_return --src -c "." 

utest:  beam
	erl -noshell -eval "eunit:test(elib1_misc, [verbose])" -s init stop	

edoc:
	erl -noshell -eval "edoc:application(lib, \".\", [{dir,\"../doc\"}])" \
	             -s init stop

html: ${MODS:%.erl=../doc/%.html}

beam: ${MODS:%.erl=../ebin/%.beam}

yecc: ${YRL:%.yrl=../tmp/%.erl} ${YRL:%.yrl=../ebin/%.beam}

chapPDF: ${CHAPS:%.chap=../../pdf/%.pdf}

# chapHTML: ${CHAPS:%.chap=../../html/%.html}	

books: ${BOOKS:%.book=../doc/%.html}

logs: ${LOGS:%.log=../doc/%.html}

clean:
	rm ../ebin/*.beam
	rm -rf *.aux *.beam 
	rm -rf *.log *.tmp erl_crash.dump 

veryclean:
	rm ../bin/* ../doc/* ../tmp/*





