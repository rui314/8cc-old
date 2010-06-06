CC=gcc
CFLAGS=-std=c99 -Wall
OBJS=8cc.o

8cc: $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS)

clean:
	-rm -f 8cc $(OBJS) hello hello.o

hello: 8cc
	./8cc hello.o
	$(CC) -o hello hello.o

test: hello
	@if [ "`./hello`" = "Hello, world!" ]; then echo "OK"; else echo "Failed"; fi
