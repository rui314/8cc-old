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

static Type *make_func_type_full(Type *ret, List *param, bool ellipsis) {
    FuncType *r = type_alloc(sizeof(FuncType), TFUNC);
    r->ret = ret;
    r->param = param;
    r->vars = NULL;
    r->ellipsis = ellipsis;
    return (Type *)r;
}

Type *make_func_type(Type *ret, List *param) {
    return make_func_type_full(ret, param, false);
}

Type *make_old_func_type(Type *ret) {
    return make_func_type_full(ret, NULL, false);
}

Type *make_ellipsis_func_type(Type *ret, List *param) {
    return make_func_type_full(ret, param, true);
}

bool has_param(Type *ctype) {
    assert(ctype->type == TFUNC);
    return ctype->param;
}

Type *make_int_type(IntKind kind) {
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

Type *make_float_type(FloatKind kind) {
    static const IntType float_types[] = {
        { TFLOAT, FLOAT },
        { TFLOAT, DOUBLE },
        { TFLOAT, LDOUBLE },
    };
    return (Type *)&float_types[kind];
}

Type *make_void_type(void) {
    static const VoidType void_type = { TVOID };
    return (Type *)&void_type;
}


/*==============================================================================
 * Variables
 */

void *var_alloc(size_t size, VarType type, Type *ctype, String *name) {
    NVar *r = malloc(size);
    r->type = type;
    r->ctype = ctype;
    r->name = name;
    return r;
}

NVar *make_local_var(Type *ctype, String *name) {
    return var_alloc(sizeof(LocalVar), LOCAL, ctype, name);
}

NVar *make_global_var(Type *ctype, String *name, Exp *init) {
    GlobalVar *r = var_alloc(sizeof(LocalVar), LOCAL, ctype, name);
    r->init = init;
    return (NVar *)r;
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

Exp *make_lval_exp(Type *ctype, LvalBase base) {
    LvalExp *r = exp_alloc(sizeof(LvalExp), ELVAL, ctype);
    r->base = base;
    r->off = NULL;
    return (Exp *)r;
}

Exp *make_const_int(Type *ctype, long i) {
    ConstExp *r = exp_alloc(sizeof(ConstExp), ECONST, ctype);
    r->v.i = i;
    return (Exp *)r;
}

Exp *make_const_float(Type *ctype, double f) {
    ConstExp *r = exp_alloc(sizeof(ConstExp), ECONST, ctype);
    r->v.f = f;
    return (Exp *)r;
}

Exp *make_unop_exp(Type *ctype, int op, Exp *exp) {
    UnopExp *r = exp_alloc(sizeof(UnopExp), EUNOP, ctype);
    r->op= op;
    r->exp = exp;
    return (Exp *)r;
}

Exp *make_addrof_exp(Type *ctype, LvalExp *lval) {
    AddrOfExp *r = exp_alloc(sizeof(AddrOfExp), EADDROF, ctype);
    r->lval = lval;
    return (Exp *)r;
}

Exp *make_binop_exp(Type *ctype, int op, Exp *exp0, Exp *exp1) {
    BinopExp *r = exp_alloc(sizeof(BinopExp), EBINOP, ctype);
    r->op = op;
    r->exp0 = exp0;
    r->exp1 = exp1;
    return (Exp *)r;
}

Exp *make_startof_exp(Type *ctype, LvalExp *lval) {
    StartOfExp *r = exp_alloc(sizeof(StartOfExp), ESTARTOF, ctype);
    r->lval = lval;
    return (Exp *)r;
}

Exp *make_sizeoftype_exp(Type *ctype, Type *argtype) {
    SizeOfTypeExp *r = exp_alloc(sizeof(SizeOfTypeExp), ESIZEOFTYPE, ctype);
    r->argtype = argtype;
    return (Exp *)r;
}

Exp *make_cast_exp(Type *ctype, Exp *exp) {
    CastExp *r = exp_alloc(sizeof(CastExp), ECAST, ctype);
    r->exp = exp;
    return (Exp *)r;
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

static void *stmt_alloc(size_t size, NodeType type) {
    Stmt *r = malloc(size);
    r->type = type;
    r->label = NULL;
    return r;
}

Node *make_instr_stmt(List *instr) {
    InstrStmt *r = stmt_alloc(sizeof(InstrStmt), SINSTR);
    r->instr = instr;
    return (Node *)r;
}

Node *make_goto_stmt(Stmt *stmt) {
    GotoStmt *r = stmt_alloc(sizeof(GotoStmt), SGOTO);
    r->stmt = stmt;
    return (Node *)r;
}

Node *make_return_stmt(Exp *exp) {
    ReturnStmt *r = stmt_alloc(sizeof(ReturnStmt), SRETURN);
    r->exp = exp;
    return (Node *)r;
}

Node *make_if_stmt(Exp *exp, List *then, List *els) {
    IfStmt *r = stmt_alloc(sizeof(IfStmt), SIF);
    r->exp = exp;
    r->then = then;
    r->els = els;
    return (Node *)r;
}

Node *make_loop_stmt(List *stmt) {
    LoopStmt *r = stmt_alloc(sizeof(LoopStmt), SLOOP);
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
 *
 * Printing types in C syntax is tricky as the C delcaration syntax is complex.
 * The PP functions defined here takes two argument.  One is a C type to be
 * printed and the another is a string being constructed.
 *
 * For simple types, such as int, PP just appends the type name at the beginning
 * of the string.  For complex types, such as array or function pointer, PP adds
 * the type names both to the beginning and to the tail. For example, if a
 * string being constructed is "X", a resulting string would be "int X[20]".
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

static String *quote(String *b) {
    return make_string_printf("(%s)", STRING_BODY(b));
}

static String *quote_type_maybe(String *b) {
    char *p = STRING_BODY(b);
    if (p[0] == '*' || strchr(p, ')'))
        return quote(b);
    return b;
}

static String *pp_int_type(Type *ctype, String *b) {
    String *r = make_string();
    switch (INT_TYPE(ctype)->kind) {
    case SCHAR:  string_append(r, "char"); break;
    case SSHORT: string_append(r, "short"); break;
    case SINT:   string_append(r, "int"); break;
    case SLONG:  string_append(r, "long"); break;
    case SLLONG: string_append(r, "long long"); break;
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
    b = quote_type_maybe(b);
    // TODO: pp array dimension
    string_append(b, "[]");
    return pp_type1(ARRAY_TYPE(ctype)->ptr, b);
}

static String *pp_ptr_type(Type *ctype, String *b) {
    return pp_type1(PTR_TYPE(ctype)->ptr, string_prepend(b, "*"));
}

static String *pp_void_type(Type *ctype, String *b) {
    return type_join(to_string("void"), b);
}

static String *pp_func_type(Type *ctype, String *b) {
    List *param = FUNC_TYPE(ctype)->param;
    b = quote_type_maybe(b);
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

String *pp_var(NVar *var) {
    return pp_type1(var->ctype, string_copy(var->name));
}

/*
 * Expressions
 */

static String *pp_exp1(Exp *e, int prec);

static String *pp_op(int op) {
    String *r = make_string();
    if (op < 256) {
        o1(r, op);
        o1(r, 0);
    } else
        string_append(r, keyword_to_string(op));
    return r;
}

static String *quote_exp_maybe(String *b, int thisprec, int parentprec) {
    if (thisprec <= parentprec)
        return b;
    return quote(b);
}

static String *pp_lval_exp(LvalExp *e, int prec) {
    String *b = NULL;
    switch (e->base.type) {
    case LVAL_VAR:
        b = string_copy(((NVar *)((NVar *)e->base.p))->name);
        break;
    case LVAL_MEM:
        b = pp_exp1((Exp *)e->base.p, prec);
        break;
    }
    for (LvalOff *off = e->off; off; off = off->off) {
        string_append(b, "[");
        string_append(b, STRING_BODY(pp_exp1(off->exp, 17)));
        string_append(b, "]");
    }
    return b;
}

static String *pp_const_int_exp(IntKind kind, long val) {
    switch (kind) {
    case SCHAR: case UCHAR:
        return make_string_printf("'%c'", val);
    case SINT:   return make_string_printf("%d", val);
    case SLONG:  return make_string_printf("%ldL", val);
    case SLLONG: return make_string_printf("%lldLL", val);
    case UINT:   return make_string_printf("%uU", val);
    case ULONG:  return make_string_printf("%luUL", val);
    case ULLONG:
        panic("Printing long long is not supported yet");
    case SSHORT: case USHORT:
        panic("Nonexistent literal data type: %d", kind);
    default:
        panic("Unknown integer kind: %d", kind);
    }
}

static String *pp_const_float_exp(FloatKind kind, double val) {
    switch (kind) {
    case FLOAT:   return make_string_printf("%fF", val);
    case DOUBLE:  return make_string_printf("%f", val);
    case LDOUBLE:
        panic("Printing long double is not supported yet");
    default:
        panic("Unknown float kind: %d", kind);
    }
}

static String *pp_const_exp(ConstExp *e) {
    Type *ctype = e->ctype;
    switch (ctype->type) {
    case TINT:
        return pp_const_int_exp(INT_TYPE(ctype)->kind, e->v.i);
    case TFLOAT:
        return pp_const_float_exp(FLOAT_TYPE(ctype)->kind, e->v.f);
    default:
        panic("unsupported const type: %d", ctype->type);
    }
}

static String *pp_unop_exp(UnopExp *e, int prec) {
    int thisprec = precedence(e->op);
    String *b = pp_op(e->op);
    string_append(b, STRING_BODY(pp_exp1(e->exp, thisprec)));
    return quote_exp_maybe(b, thisprec, prec);
}

static String *pp_addrof_exp(AddrOfExp *e, int prec) {
    int thisprec = precedence('&');
    String *b = to_string("&");
    string_append(b, STRING_BODY(pp_lval_exp(e->lval, thisprec)));
    return quote_exp_maybe(b, thisprec, prec);
}

static String *pp_binop_exp(BinopExp *e, int prec) {
    int thisprec = precedence(e->op);
    String *b = pp_exp1(e->exp0, thisprec);
    string_append(b, STRING_BODY(pp_op(e->op)));
    string_append(b, STRING_BODY(pp_exp1(e->exp1, thisprec)));
    return quote_exp_maybe(b, thisprec, prec);
}

static String *pp_startof_exp(StartOfExp *e, int prec) {
    return pp_lval_exp(e->lval, prec);
}

static String *pp_sizeoftype_exp(SizeOfTypeExp *e, int prec) {
    return make_string_printf("sizeof(%s)", STRING_BODY(pp_type(e->argtype)));
}

static String *pp_cast_exp(CastExp *e, int prec) {
    String *b =  make_string_printf("(%s)", STRING_BODY(pp_type(e->ctype)));
    string_append(b, STRING_BODY(pp_exp1(e->exp, 2)));
    return b;
}

static String *pp_exp1(Exp *e, int prec) {
    switch (e->type) {
    case EADDROF:
        return pp_addrof_exp(ADDR_OF_EXP(e), prec);
    case EBINOP:
        return pp_binop_exp(BINOP_EXP(e), prec);
    case ECAST:
        return pp_cast_exp(CAST_EXP(e), prec);
    case ECONST:
        return pp_const_exp(CONST_EXP(e));
    case ELVAL:
        return pp_lval_exp(LVAL_EXP(e), prec);
    case ESTARTOF:
        return pp_startof_exp(START_OF_EXP(e), prec);
    case ESIZEOFTYPE:
        return pp_sizeoftype_exp(SIZE_OF_TYPE_EXP(e), prec);
    case EUNOP:
        return pp_unop_exp(UNOP_EXP(e), prec);
    default:
        panic("unsupported expression type: %d", e->type);
    }
}

static String *pp_exp(Exp *e) {
    return pp_exp1(e, 17);
}

/*
 * Instructions
 */

static void pp_params(String *b, List *param) {
    string_append(b, "(");
    for (int i = 0; i < LIST_LEN(param); i++) {
        Exp *exp = LIST_REF(param, i);
        if (i > 0)
            string_append(b, ",");
        string_append(b, STRING_BODY(pp_exp(exp)));
    }
    string_append(b, ")");
}

static String *pp_set_instr(SetInstr *instr) {
    String *b = make_string();
    string_append(b, STRING_BODY(pp_lval_exp(instr->lval, precedence('='))));
    string_append(b, "=");
    string_append(b, STRING_BODY(pp_exp(instr->exp)));
    string_append(b, ";");
    return b;
}

static String *pp_call_instr(CallInstr *instr) {
    String *b = make_string();
    if (instr->retval) {
        string_append(b, STRING_BODY(pp_lval_exp(instr->retval, precedence('='))));
        string_append(b, "=");
    }
    string_append(b, STRING_BODY(pp_lval_exp(instr->fn, 1)));
    pp_params(b, instr->param);
    string_append(b, ";");
    return b;
}

static String *pp_instr(Node *node) {
    switch (node->type) {
    case ISET:
        return pp_set_instr(SET_INSTR(node));
    case ICALL:
        return pp_call_instr(CALL_INSTR(node));
    default:
        panic("Unknown node type: %d", node->type);
    }
}

/*
 * Statements
 */

static String *pp_stmt(Node *node);

static String *make_label(void) {
    static int i = 0;
    return make_string_printf("L%d", i++);
}

static void pp_stmt_list_pass1(List *list) {
    if (!list)
        return;
    for (int i = 0; i < LIST_LEN(list); i++) {
        Node *stmt = LIST_REF(list, i);
        switch (stmt->type) {
        case SIF:
            pp_stmt_list_pass1(IF_STMT(stmt)->then);
            pp_stmt_list_pass1(IF_STMT(stmt)->els);
            break;
        case SLOOP:
            pp_stmt_list_pass1(LOOP_STMT(stmt)->stmt);
            break;
        case SGOTO:
            if (!GOTO_STMT(stmt)->stmt->label)
                GOTO_STMT(stmt)->stmt->label = make_label();
            break;
        default:
            /* DO NOTHING */;
        }
    }
}

static String *pp_stmt_list(List *list) {
    pp_stmt_list_pass1(list);
    String *b = make_string();
    for (int i = 0; i < LIST_LEN(list); i++) {
        Node *stmt = LIST_REF(list, i);
        string_append(b, STRING_BODY(pp_stmt(stmt)));
    }
    return b;
}

static String *pp_goto_stmt(GotoStmt *stmt) {
    return make_string_printf("goto %s;", STRING_BODY(stmt->stmt->label));
}

static String *pp_if_stmt(IfStmt *stmt) {
    char *exp = STRING_BODY(pp_exp(stmt->exp));
    char *then = STRING_BODY(pp_stmt_list(stmt->then));
    if (!stmt->els)
        return make_string_printf("if(%s){%s}", exp, then);
    char *els = STRING_BODY(pp_stmt_list(stmt->els));
    return make_string_printf("if(%s){%s}else{%s}", exp, then, els);
}

static String *pp_instr_stmt(InstrStmt *stmt) {
    String *b = make_string();
    for (int i = 0; i < LIST_LEN(stmt->instr); i++) {
        Node *instr = LIST_REF(stmt->instr, i);
        string_append(b, STRING_BODY(pp_instr(instr)));
    }
    return b;
}

static String *pp_loop_stmt(LoopStmt *stmt) {
    char *body = STRING_BODY(pp_stmt_list(stmt->stmt));
    return make_string_printf("while(1){%s}", body);
}

static String *pp_return_stmt(ReturnStmt *stmt) {
    if (stmt->exp)
        return make_string_printf("return %s;", STRING_BODY(pp_exp(stmt->exp)));
    return to_string("return;");
}

static String *pp_stmt(Node *node) {
    String *b = STMT(node)->label
        ? make_string_printf("%s:", STRING_BODY(STMT(node)->label))
        : make_string();
    switch (node->type) {
    case SGOTO:
        string_append(b, STRING_BODY(pp_goto_stmt(GOTO_STMT(node))));
        return b;
    case SIF:
        string_append(b, STRING_BODY(pp_if_stmt(IF_STMT(node))));
        return b;
    case SINSTR:
        string_append(b, STRING_BODY(pp_instr_stmt(INSTR_STMT(node))));
        return b;
    case SLOOP:
        string_append(b, STRING_BODY(pp_loop_stmt(LOOP_STMT(node))));
        return b;
    case SRETURN:
        string_append(b, STRING_BODY(pp_return_stmt(RETURN_STMT(node))));
        return b;
    default:
        panic("Unknown statement type: %d", node->type);
    }
}

/*
 * Function
 */

String *pp_nfunction(NFunction *fn) {
    String *b = pp_type1(fn->ctype, string_copy(fn->name));
    pp_params(b, fn->param);
    string_append(b, "{");
    for (int i = 0; i < LIST_LEN(fn->var); i++) {
        NVar *var = LIST_REF(fn->var, i);
        string_append(b, STRING_BODY(pp_var(var)));
        string_append(b, ";");
    }
    string_append(b, STRING_BODY(pp_stmt_list(fn->stmt)));
    string_append(b, "}");
    return b;
}
