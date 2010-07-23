CC=gcc
CFLAGS=-std=c99 -Wall -g
OBJS=string.o list.o gen.o lex.o cpp.o parse.o file.o dict.o error.o elf.o
TESTS=string alltests

.c.o: ../8cc unittest.h
	$(CC) -Wall $(CFLAGS) -c $<

alltests: $(OBJS) main.o
	$(CC) -Wall $(CFLAGS) -o $@ $(OBJS) main.o

test: alltests
	@./alltests

clean:
	-rm -f *.o alltests
