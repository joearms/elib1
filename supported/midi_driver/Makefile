MODS := $(wildcard *.erl)

%.beam: %.erl
	erlc -W $<

run: all beam
	erl -s midi_driver test1

beam: ${MODS:%.erl=%.beam}

all: list_devices midi_in_driver midi_out_driver

list_devices: list_devices.c endpoint_name.c
	gcc -o list_devices -framework CoreMIDI\
                            -framework CoreServices\
               list_devices.c endpoint_name.c

midi_in_driver: midi_in_driver.c
	gcc -o midi_in_driver -framework CoreMIDI\
                            -framework CoreServices midi_in_driver.c

midi_out_driver: midi_out_driver.c
	gcc -o midi_out_driver -framework CoreMIDI\
                            -framework CoreServices midi_out_driver.c

clean:
	rm -rf list_devices midi_in_driver midi_out_driver
	rm -rf *.beam
