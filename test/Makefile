CC=gcc
CFLAGS=-std=c99 -Wall -g
LDFLAGS=-lpthread -ldl
OBJS=string.o list.o gen.o lex.o cpp.o parse.o file.o dict.o error.o elf.o run.o
TESTS=string alltests

string.o: ../8cc.h ../string.c string.c
list.o: ../8cc.h ../list.c list.c
gen.o: ../8cc.h ../gen.c gen.c
lex.o: ../8cc.h ../lex.c lex.c
cpp.o: ../8cc.h ../cpp.c cpp.c
parse.o: ../8cc.h ../parse.c parse.c
file.o: ../8cc.h ../file.c file.c
dict.o: ../8cc.h ../dict.c dict.c
error.o: ../8cc.h ../error.c error.c
elf.o: ../8cc.h ../elf.c elf.c
elf.o: ../8cc.h ../run.c run.c

.c.o: unittest.h
	$(CC) -Wall $(CFLAGS) -c $<

alltests: $(OBJS) main.o
	$(CC) -Wall $(CFLAGS) $(LDFLAGS) -o $@ $(OBJS) main.o

test: alltests
	@./alltests

clean:
	-rm -f *.o alltests .tmpTEST*
