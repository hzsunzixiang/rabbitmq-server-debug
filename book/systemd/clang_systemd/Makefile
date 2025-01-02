.PHONY:clean all
CC=g++
CFLAGS=-Wall -g -lsystemd
BIN=helloworld_c
all:$(BIN)
%:%.c
	$(CC) $(CFLAGS)  $< -o $@ 
clean:
	rm -f *.o $(BIN)

