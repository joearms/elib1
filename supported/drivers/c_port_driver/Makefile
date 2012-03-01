.SUFFIXES: .erl .beam .yrl

.erl.beam:
	erlc -W $<

MODS = example1

all: ${MODS:%=%.beam} port_driver
	erl -s example1 test

port_driver:  port_driver.c
	gcc -o port_driver port_driver.c

clean:
	rm -rf port_driver erl_crash.dump *.beam *~

