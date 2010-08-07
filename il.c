/*
 * il.c - data structures for intermediate languages
 *
 * Copyright 2010 Rui Ueyama <rui314@gmail.com>.  All rights reserved.
 * This code is available under the simplified BSD license.  See LICENSE for details.
 */

#include "8cc.h"

/*==============================================================================
 * C data type
 */

static void *type_alloc(size_t size, TypeEnum type) {
    Type *r = malloc(size);
    r->type = type;
    return r;
}

Type *make_array_type(Type *ptr) {
    ArrayType *r = type_alloc(sizeof(ArrayType), TARRAY);
    r->ptr = ptr;
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
        { TINT, CHAR },
        { TINT, UCHAR },
        { TINT, SHORT },
        { TINT, USHORT },
        { TINT, INT },
        { TINT, UINT },
        { TINT, LONG },
        { TINT, ULONG },
        { TINT, LLONG },
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

extern Type *get_void_type(void) {
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

static void *exp_alloc(size_t size, NodeType type, Type *ctype) {
    Exp *r = malloc(size);
    r->type = type;
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
