/*
 * il.h - data structures for intermediate languages
 *
 * Copyright 2010 Rui Ueyama <rui314@gmail.com>.  All rights reserved.
 * This code is available under the simplified BSD license.  See LICENSE for details.
 */

#include "8cc.h"

struct Exp;

/*==============================================================================
 * C data type
 */

typedef enum {
    TVOID,
    TARRAY,
    TPTR,
    TFUNC,
    TINT,
    TFLOAT,
} TypeEnum;

typedef enum {
    CHAR, UCHAR,
    SHORT, USHORT,
    INT, UINT,
    LONG, ULONG,
    LLONG, ULLONG,
} IntKind;

typedef enum {
    FLOAT,
    DOUBLE,
    LDOUBLE,
} FloatKind;

typedef struct Type {
    TypeEnum type;
} Type;

#define TYPE_HEADER TypeEnum type;

typedef struct ArrayType {
    TYPE_HEADER;
    Type *ptr;
    struct Exp *size;
} ArrayType;

typedef struct PtrType {
    TYPE_HEADER;
    Type *ptr;
} PtrType;

typedef struct FuncType {
    TYPE_HEADER;
    Type *ret;
    List *param;
} FuncType;

typedef struct IntType {
    TYPE_HEADER;
    IntKind kind;
} IntType;

typedef struct FloatType {
    TYPE_HEADER;
    FloatKind kind;
} FloatType;

typedef struct VoidType {
    TYPE_HEADER;
} VoidType;

#define ARRAY_TYPE(obj) ((ArrayType *)obj)
#define PTR_TYPE(obj)   ((PtrType *)obj)
#define FUNC_TYPE(obj)  ((FuncType *)obj)
#define INT_TYPE(obj)   ((IntType *)obj)
#define FLOAT_TYPE(obj) ((FloatType *)obj)
#define VOID_TYPE(obj)  ((VoidType *)obj)

extern Type *make_array_type(Type *ptr, struct Exp *size);
extern Type *make_ptr_type(Type *ptr);
extern Type *make_func_type(Type *ret, List *param);
extern Type *get_int_type(IntKind kind);
extern Type *get_float_type(FloatKind kind);
extern Type *get_void_type(void);

/*==============================================================================
 * Data type for Node
 */

typedef enum {
    EADDROF,
    EBINOP,
    ECAST,
    ECONST,
    ELVAL,
    ESTARTOF,
    ESIZEOFTYPE,
    EUNOP,
    ICALL,
    ISET,
    SGOTO,
    SIF,
    SINSTR,
    SLOOP,
    SRETURN,
} NodeType;

#define NODE_HEADER NodeType type
#define NODE_TYPE(e) ((e)->type)

typedef struct Node {
    NODE_HEADER;
} Node;


/*==============================================================================
 * Variables
 */

typedef struct NVar {
    // True if global variable.  False if local.
    enum { GLOBAL, LOCAL } type;
    Type *ctype;
    String *name;
} NVar;

extern NVar *make_nvar(int type, Type *ctype, String *name);


/*==============================================================================
 * Expressions
 */

#define EXP_HEADER NodeType type; Type *ctype

typedef struct Exp {
    EXP_HEADER;
} Exp;

#define EXP_CTYPE(e) ((e)->ctype)

/*
 * Lvalue
 */

typedef struct LvalBase {
    enum { LVAL_VAR, LVAL_MEM } type;
    void *p;
} LvalBase;

typedef struct LvalOff {
    enum { FIELD, INDEX } type;
    Exp *exp;
    struct LvalOff *off;
} LvalOff;

typedef struct LvalExp {
    EXP_HEADER;
    LvalBase base;
    LvalOff *off;
} LvalExp;

#define LVAL_BASE_TYPE(e) ((e)->base.type)
#define LVAL_BASE_VAR(e) ((NVar *)((e)->base.p))
#define LVAL_BASE_EXP(e) ((Exp *)((e)->base.p))

#define LVAL_OFF_TYPE(e) ((e)->off.type)
#define LVAL_OFF_INDEX(e) ((e)->off.exp);
#define LVAL_OFF_OFF(e) ((e)->off.off);

extern LvalOff *make_lval_off(int type, Exp *exp);
extern Node *make_lval_exp(Type *ctype, LvalBase base);

/*
 * Other expressions
 */
typedef struct ConstExp {
    EXP_HEADER;
    union {
        double f;
        long i;
    } v;
} ConstExp;

typedef struct UnopExp {
    EXP_HEADER;
    int op;
    Exp *exp;
} UnopExp;

typedef struct AddrOfExp {
    EXP_HEADER;
    LvalExp *lval;
} AddrOfExp;

typedef struct BinopExp {
    EXP_HEADER;
    Exp *exp0;
    Exp *exp1;
} BinopExp;

typedef struct StartOfExp {
    EXP_HEADER;
    LvalExp *lval;
} StartOfExp;

typedef struct SizeOfTypeExp {
    EXP_HEADER;
    Exp *exp;
} SizeOfTypeExp;

typedef struct CastExp {
    EXP_HEADER;
    Exp *exp;
} CastExp;

extern Node *make_const_double(Type *ctype, double f);
extern Node *make_const_long(Type *ctype, long i);
extern Node *make_unop_exp(Type *ctype, int op, Exp *exp);
extern Node *make_addrof_exp(Type *ctype, LvalExp *lval);
extern Node *make_binop_exp(Type *ctype, Exp *exp0, Exp *exp1);
extern Node *make_startof_exp(Type *ctype, LvalExp *lval);
extern Node *make_sizeoftype_exp(Type *ctype, Exp *exp);
extern Node *make_cast_exp(Type *ctype, Exp *exp);

/*==============================================================================
 * Instructions
 */

typedef struct SetInstr {
    NODE_HEADER;
    LvalExp *lval;
    Exp *exp;
} SetInstr;

typedef struct CallInstr {
    NODE_HEADER;
    LvalExp *retval;
    LvalExp *fn;
    List *param;
} CallInstr;

extern Node *make_set_instr(LvalExp *lval, Exp *exp);
extern Node *make_call_instr(LvalExp *retval, LvalExp *fn, List *param);


/*==============================================================================
 * Statement
 */

typedef struct InstrStmt {
    NODE_HEADER;
    List *instr;
} InstrStmt;

typedef struct GotoStmt {
    NODE_HEADER;
    Node *stmt;
} GotoStmt;

typedef struct ReturnStmt {
    NODE_HEADER;
    Exp *exp;
} ReturnStmt;

typedef struct IfStmt {
    NODE_HEADER;
    Exp *exp;
    List *then;
    List *els;
} IfStmt;

typedef struct LoopStmt {
    NODE_HEADER;
    List *stmt;
} LoopStmt;

extern Node *make_instr_stmt(List *instr);
extern Node *make_goto_stmt(Node *stmt);
extern Node *make_return_stmt(Exp *exp);
extern Node *make_if_stmt(Exp *exp, List *then, List *els);
extern Node *make_loop_stmt(List *stmt);

/*==============================================================================
 * Function
 */

typedef struct NFunction {
    String *name;
    Type *ctype;
    List *param;
    // List of local variables
    List *var;
    // Entry block containing list of instr
    List *stmt;
} NFunction;

extern NFunction *make_nfunction(String *name, Type *ctype, List *param, List *var, List *stmt);

/*==============================================================================
 * Pretty printer
 */

extern String *pp_type(Type *ctype);
extern String *pp_var(NVar *var);
extern String *pp_node(Node *node);
extern String *pp_nfunction(NFunction *fn);
