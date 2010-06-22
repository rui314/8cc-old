#ifndef ECC_H
#define ECC_H

#define _POSIX_SOURCE
#define _BSD_SOURCE

#include <stdarg.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <sys/types.h>
#include <unistd.h>

/*
 * Fixed size data types
 */

#define s8  int8_t
#define u8  uint8_t
#define s16 int16_t
#define u16 uint16_t
#define s32 int32_t
#define u32 uint32_t
#define s64 int64_t
#define u64 uint64_t

/*
 * ELF file format constants
 */

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
 * Common
 */
extern void error(char *format, ...);

/*
 * Byte String
 */

#define STRING_INITIAL_SIZE 32

typedef struct String {
    char *buf;
    int nalloc;
    int len;
} String;

#define STRING_LEN(b) ((b)->len)
#define STRING_BODY(b) ((b)->buf)

extern String *make_string(void);
extern void o1(String *b, int byte);
extern void out(String *b, void *data, size_t size);
extern void ostr(String *b, char *str);
extern void o2(String *b, u16 data);
extern void o4(String *b, u32 data);
extern void o8(String *b, u64 data);
extern void align(String *b, int n);

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

extern List *make_list(void);
extern void list_push(List *list, void *e);

/*
 * ELF headers (internal representation; not necessarily correspondent
 * to on-disk ELF format)
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
    u64 addend;
} Reloc;

typedef struct Section {
    String *body;
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

/*
 * File
 */

typedef struct File {
    FILE *stream;
    int lineno;
    String *filename;
    int ungotten;
} File;

extern File *make_file(FILE *stream, char *filename);
extern File *open_file(char *path);
extern void unreadc(int c, File *file);
extern int readc(File *file);

/*
 * Parser
 */

#define TOK_NUM   1
#define TOK_IDENT 2
#define TOK_STR   3
#define TOK_CHAR  4

typedef struct Token {
    int val;
    char ch;
    char *str;
    u64 num;
    int lineno;
} Token;

typedef union Cvalue {
    int i;
    float f;
} Cvalue;

#define CTYPE_INT    1
#define CTYPE_FLOAT  2

extern List *parse(File *file, Section *data);

/*
 * Assembler
 */

typedef struct Var {
    enum { VAR_IMM, VAR_LOCAL, VAR_EXTERN, VAR_GLOBAL } stype;
    char *name;
    int ctype;
    Cvalue val;
    Symbol *sym; // for external symbol
} Var;

typedef struct Inst {
    char op;
    Var *arg0;
    Var *arg1;
    List *args;
} Inst;

extern void assemble(Section *text, List *insts);
extern Section *make_section(char *name, int type);
extern Symbol *make_symbol(char *name, long value, int bind, int type, int defined);

extern Var *make_imm(u64 val);
extern Var *make_global(char *name, u64 val);
extern int add_string(Section *data, char *str);
extern Var *make_extern(char *name);
extern Inst *make_func_call(Var *fn, List *args);

#endif /* ECC_H */
