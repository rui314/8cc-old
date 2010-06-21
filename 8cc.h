#ifndef EIGHTCC_H
#define EIGHTCC_H

#include <stdarg.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define s16 int16_t
#define u16 uint16_t
#define s32 int32_t
#define u32 uint32_t
#define s64 int64_t
#define u64 uint64_t

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
extern void o2(StringBuilder *b, u16 data);
extern void o4(StringBuilder *b, u32 data);
extern void o8(StringBuilder *b, u64 data);
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

extern List *make_list(void);
extern void list_push(List *list, void *e);

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
    u64 addend;
} Reloc;

typedef struct Section {
    StringBuilder *body;
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
 * Assembler
 */

#define RINV (-1)
#define RAX 0
#define RCX 1
#define RDX 2
#define RBX 3
#define RSP 4
#define RBP 5
#define RSI 6
#define RDI 7
#define R8  8
#define R9  9
#define R10 10
#define R11 11
#define R12 12
#define R13 13
#define R14 14
#define R15 15

#define OP_NULL   0
#define OP_MOV    1
#define OP_PUSH   2
#define OP_XOR    3
#define OP_CALL   4
#define OP_LEAVE  5
#define OP_RET    6

#define TYPE_REG 1
#define TYPE_IMM 2
#define TYPE_SYM 3

typedef struct Operand {
    int type;
    union {
        int reg;
        u64 imm;
        char *sym;
    } val;
} Operand;

#define IS_REG(op) ((op)->type == TYPE_REG)
#define IS_IMM(op) ((op)->type == TYPE_IMM)
#define IS_SYM(op) ((op)->type == TYPE_SYM)

#define OPERAND_REG(op) ((op)->val.reg)
#define OPERAND_IMM(op) ((op)->val.imm)
#define OPERAND_SYM(op) ((op)->val.sym)

#define IS_REX(op) (OPERAND_REG(op) >= R8)
#define MASK_REG(op) (OPERAND_REG(op) & 7)
#define PACK_REG(code, op1, op2) (((code) << 6) | MASK_REG(op1) << 3| MASK_REG(op2))

typedef struct Insn {
    int op;
    Operand *dst;
    Operand *src;
} Insn;

typedef struct Var {
    enum { VAR_IMM, VAR_LOCAL, VAR_EXTERN, VAR_GLOBAL } type;
    char *name;
    u64 val;
} Var;

typedef struct Inst {
    char op;
    Var *arg0;
    Var *arg1;
    Var **args;
} Inst;

extern List *create_inst_list(Section *text, Section *data);
extern void assemble(Section *text, List *insts);

extern Section *make_section(char *name, int type);

extern Symbol *make_symbol(char *name, long value, int bind, int type, int defined);
extern Reloc *make_reloc(long off, char *sym, char *section, int type, u64 addend);

#endif
