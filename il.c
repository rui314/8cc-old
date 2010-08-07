/*
 * il.c - data structures for intermediate languages
 *
 * Copyright 2010 Rui Ueyama <rui314@gmail.com>.  All rights reserved.
 * This code is available under the simplified BSD license.  See LICENSE for details.
 */

#include "8cc.h"

/*==============================================================================
 * C types
 */

static void *type_alloc(size_t size, TypeEnum type) {
    Type *r = malloc(size);
    r->type = type;
    return r;
}

Type *make_array_type(Type *ptr, Exp *size) {
    ArrayType *r = type_alloc(sizeof(ArrayType), TARRAY);
    r->ptr = ptr;
    r->size = size;
    return (Type *)r;
}

Type *make_ptr_type(Type *ptr) {
    PtrType *r = type_alloc(sizeof(PtrType), TPTR);
    r->ptr = ptr;
    return (Type *)r;
}


Type *make_func_type(Type *ret, List *param) {
    FuncType *r = type_alloc(sizeof(FuncType), TFUNC);
    r->ret = ret;
    r->param = param;
    return (Type *)r;
}

Type *get_int_type(IntKind kind) {
    static const IntType int_types[] = {
        { TINT, SCHAR },
        { TINT, UCHAR },
        { TINT, SSHORT },
        { TINT, USHORT },
        { TINT, SINT },
        { TINT, UINT },
        { TINT, SLONG },
        { TINT, ULONG },
        { TINT, SLLONG },
        { TINT, ULLONG },
    };
    return (Type *)&int_types[kind];
}

Type *get_float_type(FloatKind kind) {
    static const IntType float_types[] = {
        { TFLOAT, FLOAT },
        { TFLOAT, DOUBLE },
        { TFLOAT, LDOUBLE },
    };
    return (Type *)&float_types[kind];
}

Type *get_void_type(void) {
    static const VoidType void_type = { TVOID };
    return (Type *)&void_type;
}


/*==============================================================================
 * Variables
 */

NVar *make_nvar(int type, Type *ctype, String *name) {
    NVar *r = malloc(sizeof(NVar));
    r->type = type;
    r->ctype = ctype;
    r->name = name;
    return r;
}


/*==============================================================================
 * Expressions
 */

static void *node_alloc(size_t size, NodeType type) {
    Exp *r = malloc(size);
    r->type = type;
    return r;
}

static void *exp_alloc(size_t size, NodeType type, Type *ctype) {
    Exp *r = node_alloc(size, type);
    r->ctype = ctype;
    return r;
}

LvalOff *make_lval_off(int type, Exp *exp) {
    LvalOff *r = malloc(sizeof(LvalOff));
    r->type = type;
    r->exp = exp;
    r->off = NULL;
    return r;
}

Node *make_lval_exp(Type *ctype, LvalBase base) {
    LvalExp *r = exp_alloc(sizeof(LvalExp), ELVAL, ctype);
    r->base = base;
    r->off = NULL;
    return (Node *)r;
}

Node *make_const_double(Type *ctype, double f) {
    ConstExp *r = exp_alloc(sizeof(ConstExp), ECONST, ctype);
    r->v.f = f;
    return (Node *)r;
}

Node *make_const_long(Type *ctype, long i) {
    ConstExp *r = exp_alloc(sizeof(ConstExp), ECONST, ctype);
    r->v.i = i;
    return (Node *)r;
}

Node *make_unop_exp(Type *ctype, int op, Exp *exp) {
    UnopExp *r = exp_alloc(sizeof(UnopExp), EUNOP, ctype);
    r->op= op;
    r->exp = exp;
    return (Node *)r;
}

Node *make_addrof_exp(Type *ctype, LvalExp *lval) {
    AddrOfExp *r = exp_alloc(sizeof(AddrOfExp), EADDROF, ctype);
    r->lval = lval;
    return (Node *)r;
}

Node *make_binop_exp(Type *ctype, Exp *exp0, Exp *exp1) {
    BinopExp *r = exp_alloc(sizeof(BinopExp), EBINOP, ctype);
    r->exp0 = exp0;
    r->exp1 = exp1;
    return (Node *)r;
}

Node *make_startof_exp(Type *ctype, LvalExp *lval) {
    StartOfExp *r = exp_alloc(sizeof(StartOfExp), ESTARTOF, ctype);
    r->lval = lval;
    return (Node *)r;
}

Node *make_sizeoftype_exp(Type *ctype, Exp *exp) {
    SizeOfTypeExp *r = exp_alloc(sizeof(SizeOfTypeExp), ESIZEOFTYPE, ctype);
    r->exp = exp;
    return (Node *)r;
}

Node *make_cast_exp(Type *ctype, Exp *exp) {
    CastExp *r = exp_alloc(sizeof(CastExp), ECAST, ctype);
    r->exp = exp;
    return (Node *)r;
}


/*==============================================================================
 * Instructions
 */

Node *make_set_instr(LvalExp *lval, Exp *exp) {
    SetInstr * r = node_alloc(sizeof(SetInstr), ISET);
    r->lval = lval;
    r->exp = exp;
    return (Node *)r;
}

Node *make_call_instr(LvalExp *retval, LvalExp *fn, List *param) {
    CallInstr * r = node_alloc(sizeof(CallInstr), ICALL);
    r->retval = retval;
    r->fn = fn;
    r->param = param;
    return (Node *)r;
}


/*==============================================================================
 * Statement
 */

Node *make_instr_stmt(List *instr) {
    InstrStmt *r = node_alloc(sizeof(InstrStmt), SINSTR);
    r->instr = instr;
    return (Node *)r;
}

Node *make_goto_stmt(Node *stmt) {
    GotoStmt *r = node_alloc(sizeof(GotoStmt), SGOTO);
    r->stmt = stmt;
    return (Node *)r;
}

Node *make_return_stmt(Exp *exp) {
    ReturnStmt *r = node_alloc(sizeof(ReturnStmt), SRETURN);
    r->exp = exp;
    return (Node *)r;
}

Node *make_if_stmt(Exp *exp, List *then, List *els) {
    IfStmt *r = node_alloc(sizeof(IfStmt), SIF);
    r->exp = exp;
    r->then = then;
    r->els = els;
    return (Node *)r;
}

Node *make_loop_stmt(List *stmt) {
    LoopStmt *r = node_alloc(sizeof(LoopStmt), SLOOP);
    r->stmt = stmt;
    return (Node *)r;
}


/*==============================================================================
 * Function
 */

NFunction *make_nfunction(String *name, Type *ctype, List *param, List *var, List *stmt) {
    NFunction *r = malloc(sizeof(NFunction));
    r->name = name;
    r->ctype = ctype;
    r->param = param;
    r->var = var;
    r->stmt = stmt;
    return r;
}


/*==============================================================================
 * Pretty printer
 */

/*
 * C types
 */

static String *pp_type1(Type *ctype, String *b);

static bool isidchar(char c) {
    return isalpha(c) || c == '_';
}

static String *type_join(String *b0, String *b1) {
    if (isidchar(STRING_BODY(b1)[0]))
        string_append(b0, " ");
    string_append(b0, STRING_BODY(b1));
    return b0;
}

static String *quote_maybe(String *b) {
    char *p = STRING_BODY(b);
    if (p[0] == '*' || strchr(p, ')'))
        return make_string_printf("(%s)", STRING_BODY(b));
    return b;
}

static String *pp_int_type(Type *ctype, String *b) {
    String *r = make_string();
    switch (INT_TYPE(ctype)->kind) {
    case SCHAR:   string_append(r, "char"); break;
    case SSHORT:  string_append(r, "short"); break;
    case SINT:    string_append(r, "int"); break;
    case SLONG:   string_append(r, "long"); break;
    case SLLONG:  string_append(r, "long long"); break;
    case UCHAR:  string_append(r, "unsigned char"); break;
    case USHORT: string_append(r, "unsigned short"); break;
    case UINT:   string_append(r, "unsigned int"); break;
    case ULONG:  string_append(r, "unsigned long"); break;
    case ULLONG: string_append(r, "unsigned long long"); break;
    }
    return type_join(r, b);
}

static String *pp_float_type(Type *ctype, String *b) {
    String *r = make_string();
    switch (FLOAT_TYPE(ctype)->kind) {
    case FLOAT:   string_append(r, "float"); break;
    case DOUBLE:  string_append(r, "double"); break;
    case LDOUBLE: string_append(r, "long double"); break;
    }
    return type_join(r, b);
}

static String *pp_array_type(Type *ctype, String *b) {
    String *r = pp_type1(ARRAY_TYPE(ctype)->ptr, quote_maybe(b));
    // TODO: pp array dimension
    string_append(r, "[]");
    return r;
}

static String *pp_ptr_type(Type *ctype, String *b) {
    return pp_type1(PTR_TYPE(ctype)->ptr, string_prepend(b, "*"));
}

static String *pp_void_type(Type *ctype, String *b) {
    return type_join(to_string("void"), b);
}

static String *pp_func_type(Type *ctype, String *b) {
    List *param = FUNC_TYPE(ctype)->param;
    b = quote_maybe(b);
    if (!LIST_LEN(param)) {
        string_append(b, "(void)");
    } else {
        string_append(b, "(");
        for (int i = 0; i < LIST_LEN(param); i++) {
            Type *ptype = LIST_REF(param, i);
            if (i > 0)
                string_append(b, ",");
            string_append(b, STRING_BODY(pp_type1(ptype, to_string(""))));
        }
        string_append(b, ")");
    }
    return pp_type1(FUNC_TYPE(ctype)->ret, b);
}

static String *pp_type1(Type *ctype, String *b) {
    switch (ctype->type) {
    case TINT:
        return pp_int_type(ctype, b);
    case TFLOAT:
        return pp_float_type(ctype, b);
    case TARRAY:
        return pp_array_type(ctype, b);
    case TFUNC:
        return pp_func_type(ctype, b);
    case TPTR:
        return pp_ptr_type(ctype, b);
    case TVOID:
        return pp_void_type(ctype, b);
    default:
        panic("unsupported type: %d", ctype->type);
    }
}

String *pp_type(Type *ctype) {
    return pp_type1(ctype, to_string(""));
}

/*
 * Variables
 */

String *pp_var(NVar *var){
    return pp_type1(var->ctype, string_copy(var->name));
}
