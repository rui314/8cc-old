CC=gcc
CFLAGS=-std=c99 -Wall -g
OBJS=8cc.o string.o

8cc: $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS)

tests: tests.o string.o
	$(CC) $(CFLAGS) -o $@ string.o tests.o

clean:
	-rm -f 8cc $(OBJS) *.o hello tests

hello: 8cc
	./8cc hello.o
	$(CC) -o hello hello.o

test: hello tests
	@if [ "`./hello`" = "Hello, world!" ]; then echo "OK"; else echo "NG"; fi
	@./tests
