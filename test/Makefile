SHELL=/bin/bash
CC=gcc
CFLAGS=-std=c99 -Wall -g
LDFLAGS=-lpthread -ldl -rdynamic
OBJS=string.o list.o gen.o lex.o cpp.o parse.o file.o dict.o error.o elf.o run.o
HEADERS=../8cc.h unittest.h
TESTS=string alltests

test: alltests
	@./alltests

$(OBJS): %.o: %.c ../%.c $(HEADERS)

alltests: $(OBJS) main.o
	$(CC) -Wall $(CFLAGS) $(LDFLAGS) -o $@ $^

clean:
	-rm -f *.o alltests .tmpTEST*

.PHONEY: clean test
