CC=gcc
CFLAGS=-std=c99 -Wall -g
OBJS=8cc.o string.o list.o gen.o

8cc: $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS)

$(OBJS): 8cc.h

tests: tests.o string.o list.o gen.o 8cc.h
	$(CC) $(CFLAGS) -o $@ string.o list.o gen.o tests.o

clean:
	-rm -f 8cc $(OBJS) *.o hello tests

hello: 8cc
	./8cc hello.o
	$(CC) -o hello hello.o

test: hello tests
	@if [ "`./hello`" = 'Hello, world!' ]; then echo "OK"; else echo "NG"; fi
	@./tests

all: 8cc
