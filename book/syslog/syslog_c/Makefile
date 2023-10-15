.PHONY:clean all
CC=g++
CFLAGS=-Wall -g -lsystemd
BIN=syslog_test
all:$(BIN)
%:%.c
	$(CC) $(CFLAGS)  $< -o $@ 
clean:
	rm -f *.o $(BIN)

