## Make

all:
	cd lib/src; make
	cd supported/indexer; make
	cd supported/tagger; make
	cd supported/irc; make beam
	cd supported/website; make
	(sleep 1 && bin/openurl http://localhost:2246/supported/website/index.ehtml) & \
           erl -pa ./supported/irc -pa ./supported/website -s simple_web_server start

clean:
	cd lib/src; make clean
	cd supported/indexer; make clean
	cd supported/tagger; make clean
	cd supported/irc; make clean
	cd supported/website; make clean


