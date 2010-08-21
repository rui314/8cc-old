/*
 * 8cc.h - 8cc C compiler header
 *
 * Copyright 2010 Rui Ueyama <rui314@gmail.com>.  All rights reserved.
 * This code is available under the simplified BSD license.  See LICENSE for details.
 */

#ifndef ECC_H
#define ECC_H

#define _POSIX_SOURCE
#define _BSD_SOURCE

#include <ctype.h>
#include <limits.h>
#include <setjmp.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <time.h>
#include <unistd.h>

/*
 * Primitive data types
 */

typedef int8_t   s8;
typedef uint8_t  u8;
typedef int16_t  s16;
typedef uint16_t u16;
typedef int32_t  s32;
typedef uint32_t u32;
typedef int64_t  s64;
typedef uint64_t u64;
typedef intptr_t intptr;

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

#ifdef __GNUC__
# define NORETURN __attribute__((noreturn))
#else
# define NORETURN
#endif

/*============================================================
 * Byte String
 */

#define STRING_INITIAL_SIZE 32

typedef struct String {
    char *buf;
    int nalloc;
    int len;
    int pos;
} String;

#define STRING_LEN(b) ((b)->len)
#define STRING_BODY(b) ((b)->buf)

extern String *make_string(void);
extern String *to_string(char *str);
extern bool string_equal(String *a, String *b);
extern void string_append(String *b, char *p);
extern String *string_copy(String *b);
extern String *string_prepend(String *b, char *p);
extern void o1(String *b, int byte);
extern void out(String *b, void *data, size_t size);
extern void ostr(String *b, char *str);
extern void o2(String *b, u16 data);
extern void o3(String *b, u32 data);
extern void o4(String *b, u32 data);
extern void o8(String *b, u64 data);
extern void align(String *b, int n);
extern void string_seek(String *b, int pos);
extern void string_vprintf(String *b, char *format, va_list ap);
extern void string_printf(String *b, char *format, ...);
extern String *make_string_printf(char *format, ...);

/*============================================================
 * Error Handlers
 */

typedef struct Exception {
    jmp_buf jmpbuf;
    String *msg;
} Exception;

extern Exception *current_handler;

#define TRY(e_) (current_handler = (e_), !setjmp((e_)->jmpbuf))

extern Exception *make_exception(void);
extern NORETURN void error(char *format, ...);
extern void debug(char *format, ...);
extern void warn(char *format, ...);
extern NORETURN void print_parse_error(int line, int column, char *msg, va_list ap);
extern void print_stack_trace(void);
extern void print_stack_trace_safe(void);

#define panic(fmt, ...)                                                 \
    do {                                                                \
        print_stack_trace();                                            \
        error("[INTERNAL ERROR] %s:%d: " fmt, __FILE__, __LINE__, ## __VA_ARGS__); \
    } while (0)

#define ASSERT(expr)                              \
    do {                                          \
        if (!(expr)) {                            \
            print_stack_trace();                  \
            panic("Assertion failed: %s", #expr); \
        }                                         \
    } while (0)

/*============================================================
 * List
 */

#define LIST_INITIAL_SIZE 8

typedef struct List {
    void **elems;
    int nalloc;
    int len;
} List;

#define LIST_REF(lis, i) (((lis)->elems)[i])
#define LIST_LEN(lis) ((lis)->len)
#define LIST_TOP(lis) (LIST_REF((lis), 0))
#define LIST_BOTTOM(lis) (LIST_REF((lis), LIST_LEN(lis) - 1))
#define LIST_IS_EMPTY(lis) ((lis)->len == 0)

extern List *make_list(void);
extern List *make_list1(void *e0);
extern List *make_list2(void *e0, void *e1);
extern List *make_listn(void *e0, ...);
extern void list_push(List *list, void *e);
extern void list_push(List *list, void *e);
extern void *list_pop(List *list);
extern void *list_unshift(List *list);
extern List *sublist(List *orig, int off);
extern void list_append(List *a, List *b);
extern List *list_reverse(List *list);
extern List *list_copy(List *list);
extern bool list_in(List *list, String *e);
extern List *list_union(List *a, List *b);
extern List *list_union1(List *list, String *b);
extern List *list_intersect(List *a, List *b);

/*============================================================
 * Dictionary (Hash table)
 */

#define DICT_INITIAL_SIZE 16

typedef struct Bucket {
    u32 hashval;
    void *key;
    void *elem;
} Bucket;

#define DICT_TYPE_STRING  0
#define DICT_TYPE_ADDRESS 1

typedef u32 dict_hash_fn(void *e);
typedef bool dict_equal_fn(void *a, void *b);

typedef struct Dict {
    dict_hash_fn *hashfn;
    dict_equal_fn *equalfn;
    Bucket *buckets;
    int nalloc;
    int nelem;
} Dict;

extern Dict *make_dict(dict_hash_fn hashfn, dict_equal_fn equalfn);
extern Dict *make_string_dict(void);
extern Dict *make_string_dict(void);
extern Dict *make_address_dict(void);
extern void dict_put(Dict *dict, void *key, void *obj);
extern void *dict_get(Dict *dict, void *key);
extern bool dict_delete(Dict *dict, void *key);
extern bool dict_has(Dict *dict, void *key);
extern int dict_size(Dict *dict);

typedef struct DictIter {
    Dict *dict;
    int idx;
} DictIter;

extern DictIter *make_dict_iter(Dict* dict);
extern void *dict_iter_next(DictIter* dict);

/*
 * ELF headers (internal representation; not necessarily correspondent
 * to on-disk ELF format)
 */

typedef struct Section {
    String *body;
    char *name;
    int shstrtab_off;
    int type;
    int flags;
    int align;
    int link;
    List *rels; // list of Reloc
    int info;
    int entsize;
    int shndx;
    int symindex;
    // For executing binary in memory.
    void *memory_pos;
} Section;

typedef struct Symbol {
    String *name;
    Section *section;
    long value;
    int bind;
    int type;
    int sectidx;
    int defined;
    int index;
} Symbol;

typedef struct Reloc {
    long off;
    char *sym;
    Section *section;
    int type;
    u64 addend;
} Reloc;

typedef struct Elf {
    List *sections;
    int shnum;
    int symtabnum;
    Dict *syms;
} Elf;

extern Elf *new_elf(void);
extern void write_elf(FILE *outfile, Elf *elf);
extern Section *find_section(Elf *elf, char *name);
extern Symbol *find_symbol(Elf *elf, char *name);

/*============================================================
 * File
 */

#define FILE_STDIO  1
#define FILE_STRING 2

typedef struct File {
    int type;  // FILE_STDIO or FILE_STRING
    // If FILE_STDIO
    FILE *stream;
    // If FILE_STRING
    char *buf;
    int pos;

    int line;
    int column;
    int last_column;
    String *filename;
    int ungotten[2];
    bool eof_flag;
} File;

extern File *make_file(FILE *stream, char *filename);
extern File *make_string_file(String *s);
extern File *open_file(char *path);
extern void close_file(File *file);
extern void unreadc(int c, File *file);
extern int peekc(File *file);
extern int readc(File *file);
extern bool next_char_is(File *file, int c);

/*============================================================
 * Lexer
 */

typedef enum KeywordType {
    KEYWORD_NON_ONE_CHAR_BEGIN = 255,
#define KEYWORD(k, s) k,
#define PUNCT(k, s) k,
# include "keyword.h"
#undef PUNCT
#undef KEYWORD
} KeywordType;

typedef union TokenValue {
    int i;
    float f;
    String *str;
} TokenValue;

typedef enum TokType {
    TOKTYPE_INVALID,
    TOKTYPE_KEYWORD,
    TOKTYPE_IDENT,
    TOKTYPE_PUNCT,
    TOKTYPE_CPPNUM,
    TOKTYPE_CHAR,
    TOKTYPE_STRING,
    TOKTYPE_INT,
    TOKTYPE_FLOAT,
    // The following two types are used only in CPP.
    TOKTYPE_NEWLINE,
    TOKTYPE_SPACE,
    TOKTYPE_MACRO_PARAM,
} TokType;

typedef struct Token {
    TokType toktype;
    TokenValue val;
    int line;
    int column;
    // Used in preprocessor macro expansion.
    List *hideset;
    // True if token follows space character.  Used only when concatenating
    // tokens for # operator (the only case we need to preserve existence of
    // space.)
    bool space;
} Token;

typedef enum {
    COND_IF, COND_ELIF, COND_ELSE, COND_IFDEF, COND_IFNDEF, COND_ENDIF,
} CondInclType;

typedef struct CppContext {
    File *file;
    bool at_bol;
    // For #include.
    List *file_stack;
    // Macro definitions.
    Dict *defs;
    // Pushback buffer for preprocessing tokens.
    List *ungotten;
    // Never read file if true.
    bool in_macro;
    // Used by conditional inclusion, such as #if.
    List *incl;
    // System include paths.
    List *include_path;
    // For __DATE__ and __TIME__ macros.
    struct tm *tm;
} CppContext;

extern Token *read_cpp_token(CppContext *ctx);
extern void expect_newline(CppContext *ctx);
extern Token *copy_token(Token *tok);
extern Token *make_token(CppContext *ctx, TokType type, TokenValue val);
extern Token *make_str_literal(CppContext *ctx, String *val);
extern Token *make_cppnum(CppContext *ctx, String *val);
extern bool is_next_space(CppContext *ctx);
extern CondInclType skip_cond_incl(CppContext *ctx);
extern String *read_header_name(CppContext *ctx, bool *std);

extern CppContext *make_cpp_context(File *file);
extern void do_include(CppContext *ctx, File *file);
extern void unget_cpp_token(CppContext *ctx, Token *tok);
extern Token *peek_cpp_token(CppContext *ctx);
extern NORETURN void error_cpp_ctx(CppContext *ctx, char *msg, ...);

/*
 * A special value indicating that the result of macro expansion
 * varies depending on context.  Used for __DATE__ and __TIME__.
 */
#define VARIABLE_MACRO ((void *)1)

/*============================================================
 * C Preprocessor
 */

struct ReadContext;
extern Token *read_token(struct ReadContext *ctx);
extern Token *read_token_nonnull(struct ReadContext *ctx);
extern void define_predefined_macros(CppContext *ctx);
extern void cpp_write(CppContext *ctx, FILE *out);

/*============================================================
 * Parser
 */

struct Var;
struct Ctype;

/*
 * Represents basic block of program.  Code must contain one of OP_RETURN OP_JMP
 * or OP_IF which is the end of the basic block.  Other instructions following
 * the instruction are dead code and safely be ignored.
 */
typedef struct Block {
    int pos;
    List *code;
} Block;

typedef struct Function {
    String *name;
    struct Ctype *rettype;
    List *params;
    Block *entry;
} Function;

/*
 * Read context for lexer and parser
 */
typedef struct ReadContext {
    File *file;
    Elf *elf;
    List *scope;
    // The entry basic block for the fucntion being read.
    Block *entry;
    // The stack of basic blocks.  Instructions for code being processsed are
    // emitted to the top basic block of the stack.
    List *blockstack;
    // Pushback buffer for tokens.
    List *ungotten;
    // Function being read.
    Function *func;
    // "break" and "continue" targets.  NULL means we're outside of loop or
    // switch.
    Block *onbreak;
    Block *oncontinue;
    // Labels and their jump destination basic blocks.  Used by goto.
    Dict *label;
    // Labels and their jump origination basic blocks.  When the parser visits
    // a forward-referencing goto (having labels which has not yet seen), the
    // label and the block is stored to the dictionary.  Such blocks are
    // processed when the labels are read.
    Dict *label_tbf;
    // For CPP.
    CppContext *cppctx;
    // True if constant expression is needed.
    bool in_const_expr;
} ReadContext;

typedef union Cvalue {
    char c;
    int i;
    long l;
    float f;
    String *s;
} Cvalue;

typedef enum CtypeEnum {
    CTYPE_INVALID,
    CTYPE_PTR,
    CTYPE_ARRAY,
    CTYPE_LLONG,
    CTYPE_LONG,
    CTYPE_INT,
    CTYPE_SHORT,
    CTYPE_CHAR,
    CTYPE_FLOAT,
    CTYPE_DOUBLE,
} CtypeEnum;

typedef struct Ctype {
    CtypeEnum type;
    bool signedp;
    struct Ctype *ptr;
    int size; // valid iff type == CTYPE_ARRAY
} Ctype;

extern List *parse(File *file, Elf *elf);
extern int ctype_sizeof(Ctype *ctype);
extern bool ctype_equal(Ctype *ctype, int type);
extern ReadContext *make_read_context(File *file, Elf *elf, CppContext *ctx);
extern char *token_to_string(Token *tok);
extern struct Var *read_comma_expr(ReadContext *ctx);
extern NORETURN void error_token(Token *tok, char *msg, ...);
extern NORETURN void error_ctx(ReadContext *ctx, char *msg, ...);

extern void parse_expect(ReadContext *ctx, int expected);
extern bool is_keyword(Token *tok, int type);
extern void unget_token(ReadContext *ctx, Token *tok);
extern Token *peek_token(ReadContext *ctx);
extern bool next_token_is(ReadContext *ctx, int keyword);
extern char *keyword_to_string(int v);
extern bool is_type_keyword(Token *tok);

/*============================================================
 * Code generator
 */

extern bool flag_debug;

// x86 general-purpose registers
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

#define EXT_REG(x) ((x) >= R8)

// XMM registers
#define XMM0 0
#define XMM1 1
#define XMM2 2
#define XMM3 3
#define XMM4 4
#define XMM5 5
#define XMM6 6
#define XMM7 7
#define XMM8  8
#define XMM9  9
#define XMM10 10
#define XMM11 11
#define XMM12 12
#define XMM13 13
#define XMM14 14
#define XMM15 15

#define VAR_IMM    0
#define VAR_EXTERN 1
#define VAR_LOCAL  2

typedef struct Var {
    int stype;
    Ctype *ctype;
    String *name;
    Cvalue val;  // Immediate value.  Only valid when stype == VAR_IMM
    struct Var *loc; // Non-NULL if lvalue
    // For register allocation
    bool need_save;
} Var;

enum {
    OP_DUMMY = 255,
#define INST(x) x,
#include "inst.h"
#undef INST
};

typedef struct Inst {
    int op;
    List *args;
    Cvalue val;
} Inst;

extern void assemble(Elf *elf, List *fns);
extern Symbol *make_symbol(String *name, Section *sect, long value, int bind, int type, int defined);

extern Inst *make_inst0(int op);
extern Inst *make_inst1(int op, void *v0);
extern Inst *make_inst2(int op, void *v0, void *v1);
extern Inst *make_inst3(int op, void *v0, void *v1, void *v2);
extern Inst *make_inst4(int op, void *v0, void *v1, void *v2, void *v4);
extern Inst *make_instn(int op, List *args);
extern bool is_flonum(Ctype *ctype);
extern void print_function(Function *func);


/*============================================================
 * Initializer
 */

void eightcc_init(void);

/*============================================================
 * C script
 */

extern int run_main(Elf *elf, int argc, char **argv);
extern int run_string(char *code);


#endif /* ECC_H */
