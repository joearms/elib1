.SUFFIXES: .erl .beam .yrl

.erl.beam:
	erlc -W $<

MODS = epeg_pc epeg_bootstrap epeg_compiler

ERL = erl -noshell -boot start_clean 

test1: compile 
	${ERL} -s epeg_bootstrap batch epeg.peg 
	${ERL} -s epeg_bootstrap batch peglib.peg 	
	${ERL} -s epeg_bootstrap batch pegtest.peg 		
	${ERL} -s epeg_bootstrap batch erlterm.peg 			
	erlc -W pegtest.erl peglib.erl epeg.erl erlterm.erl
	@${ERL} -s pegtest test
	@${ERL} -s epeg test
	@${ERL} -s erlterm test	

compile: ${MODS:%=%.beam}

clean:	
	rm -rf *.beam *~ pegtest.erl peglib.erl erlterm.erl erl_crash.dump
	rm -rf *.COVER.out





