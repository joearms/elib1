driver: example_drv.so erl

example_drv.so: port_driver.cpp
	g++ -o example_drv.so -fpic -shared port_driver.cpp

erl: complex.beam
	@erl -noshell -s complex test


complex.beam: complex.erl
	erlc complex.erl

clean:
	rm -f example_drv.so complex.beam