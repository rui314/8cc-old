SHELL=/bin/bash
CC=gcc
CFLAGS=-std=c99 -Wall -g
LDFLAGS=-ldl -rdynamic
OBJS=main.o string.o list.o gen.o lex.o cpp.o parse.o file.o dict.o error.o elf.o run.o init.o decl.o

all: 8cc

8cc: $(OBJS)
	$(CC) -Wall $(CFLAGS) $(LDFLAGS) -o $@ $(OBJS)

$(OBJS): 8cc.h

nqueen: 8cc sample/nqueen.c
	./8cc sample/nqueen.c nqueen.o
	gcc -o nqueen nqueen.o

test:
	$(MAKE) -C test test

clean:
	-rm -f 8cc $(OBJS) *.o nqueen .tmpTEST*
	$(MAKE) -C test clean

.PHONY: test clean all
