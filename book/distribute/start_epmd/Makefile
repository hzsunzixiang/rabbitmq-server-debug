#.PHONY:clean all
.SUFFIXES: .erl .beam


.erl.beam:
	erlc -W $<

ERL = erl -boot start_clean

MODS := $(patsubst %.erl,%, $(wildcard *.erl))

RUN_BIN=distribute
all:compile
	erl  -s $(RUN_BIN) start -s init
	#erl -noshell -run $(RUN_BIN) start -s init stop
	#erl -noshell -s nano_server start client -s init stop


compile:${MODS:%=%.beam} 

#RUN_BIN=hello
#run:
#	erl -noshell -s $(RUN_BIN) start  -s init stop

clean:
	rm -f *.beam *.dump
