driver: example_drv.so erl

example_drv.so: port_driver.c
	gcc -o example_drv.so -fpic -shared port_driver.c 

erl: complex.beam
	@erl -noshell -s complex test

complex.beam: complex.erl
	erlc complex.erl

clean:
	rm -f example_drv.so complex.beam erl_crash.dump