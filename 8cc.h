/* -*- c-basic-offset: 4 -*- */
#ifndef EIGHTCC_H
#define EIGHTCC_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <stdint.h>

#define int16  int16_t
#define uint16 uint16_t
#define int32  int32_t
#define uint32 uint32_t
#define int64  int64_t
#define uint64 uint64_t

#define SHT_NULL 0
#define SHT_PROGBITS 1
#define SHT_SYMTAB 2
#define SHT_STRTAB 3
#define SHT_RELA 4
#define SHT_HASH 5
#define SHT_DYNAMIC 6
#define SHT_NOTE 7
#define SHT_NOBITS 8
#define SHT_REL 9
#define SHT_SHLIB 10
#define SHT_DYNSYM 11

#define SHF_WRITE 1
#define SHF_ALLOC 2
#define SHF_EXECINSTR 4

#define SHN_UNDEF 0
#define SHN_LORESERVE 0xff00
#define SHN_LOPROC 0xff00
#define SHN_HIPROC 0xff1f
#define SHN_ABS 0xfff1
#define SHN_COMMON 0xfff2
#define SHN_HIRESERVE 0xffff

#define ELF64_ST_BIND(i) ((i)>>4)
#define ELF64_ST_TYPE(i) ((i)&0xf)
#define ELF64_ST_INFO(b, t) (((b)<<4)+((t)&0xf))

#define STB_LOCAL 0
#define STB_GLOBAL 1
#define STB_WEAK 2

#define STT_NOTYPE 0
#define STT_OBJECT 1
#define STT_FUNC 2
#define STT_SECTION 3
#define STT_FILE 4

#define ELF64_R_SYM(i) ((i) >> 32)
#define ELF64_R_TYPE(i) ((i) & 0xffffffffL)
#define ELF64_R_INFO(s, t) ((((int64_t) (s)) << 32) + (((int64_t) (t)) & 0xffffffffL))

#define R_X86_64_NONE 0
#define R_X86_64_64 1
#define R_X86_64_PC32 2
#define R_X86_64_GOT32 3
#define R_X86_64_PLT32 4
#define R_X86_64_COPY 5
#define R_X86_64_GLOB_DAT 6
#define R_X86_64_JUMP_SLOT 7
#define R_X86_64_RELATIVE 8
#define R_X86_64_GOTPCREL 9
#define R_X86_64_32 10
#define R_X86_64_32S 11
#define R_X86_64_16 12
#define R_X86_64_PC16 13
#define R_X86_64_8 14
#define R_X86_64_PC8 15

/*
 * Byte String
 */

#define STRING_BUILDER_INITIAL_SIZE 32

typedef struct StringBuilder {
    char *buf;
    int nalloc;
    int len;
} StringBuilder;

#define SBUILDER_LEN(b) ((b)->len)
#define SBUILDER_BODY(b) ((b)->buf)

typedef struct String {
    char *body;
    long len;
} String;

extern StringBuilder *make_sbuilder(void);
extern void o1(StringBuilder *b, int byte);
extern void out(StringBuilder *b, void *data, size_t size);
extern void ostr(StringBuilder *b, char *str);
extern void o2(StringBuilder *b, uint16 data);
extern void o4(StringBuilder *b, uint32 data);
extern void o8(StringBuilder *b, uint64 data);
extern void align(StringBuilder *b, int n);

/*
 * List
 */

#define LIST_INITIAL_SIZE 8

typedef struct List {
    void **elems;
    int nalloc;
    int len;
} List;

#define LIST_ELEM(type, lis, i) (((type**)(lis)->elems)[i])
#define LIST_LEN(lis) ((lis)->len)

List *make_list(void);
void list_push(List *list, void *e);

/*
 * ELF headers
 */

typedef struct Symbol {
    char *name;
    long value;
    int bind;
    int type;
    int sectidx;
    int defined;
} Symbol;

typedef struct Reloc {
    long off;
    char *sym;
    char *section;
    int type;
    uint64_t addend;
} Reloc;

typedef struct Section {
    int off;
    void *data;
    int size;
    char *name;
    int shstrtab_off;
    int type;
    int flags;
    int align;
    List *syms;
    int link;
    List *rels;
    int info;
    int entsize;
} Section;

typedef struct Elf {
    Section *sections[20];
    int size;
    int shnum;
    int symtabnum;
    List *syms;
} Elf;

#endif
