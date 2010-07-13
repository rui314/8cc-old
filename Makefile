CC=gcc
CFLAGS=-std=c99 -Wall -g
OBJS1=string.o list.o gen.o lex.o cpp.o parse.o file.o dict.o error.o elf.o
OBJS=main.o $(OBJS1)

8cc: $(OBJS)
	$(CC) -Wall $(CFLAGS) -o $@ $(OBJS)

$(OBJS): 8cc.h

tests: $(OBJS1) tests.o 8cc.h
	$(CC) -Wall $(CFLAGS) -o $@ tests.o $(OBJS1)

clean:
	-rm -f 8cc $(OBJS) *.o test tests nqueen .tmpTEST*

test: tests 8cc
	@./runtest

nqueen: 8cc sample/nqueen.c
	./8cc sample/nqueen.c nqueen.o
	gcc -o nqueen nqueen.o

all: 8cc
