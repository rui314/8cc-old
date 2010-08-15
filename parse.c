/*
 * parse.c - C parser
 *
 * Copyright 2010 Rui Ueyama <rui314@gmail.com>.  All rights reserved.
 * This code is available under the simplified BSD license.  See LICENSE for details.
 */

#include "8cc.h"

/*
 * Recursive-descent parser for C.  This emits flat intermediate code
 * during parsing input file.  For example, the following fragment of
 * code
 *
 *   i = func(3 + 4 * j, k++)
 *
 * would be compiled to the following code.
 *
 *   tmp0 = 4 * i
 *   tmp1 = 3 + tmp0
 *   tmp2 = k
 *   k = k + 1
 *   tmp3 = call(func, tmp1, tmp2)
 *   i = tmp3
 *
 * The following book (abbreviated as "C:ARM") contains a BNF grammer
 * for C.
 *
 *   C: A Reference Manual, Fifth Edition by Samuel P. Harbison and Guy
 *   L. Steele, published by Prentice Hall in February 2002, ISBN 0-13-089592X.
 *   http://www.amazon.com/dp/013089592X/
 */

#define SWAP(type, x, y) do { type tmp_ = x; x = y; y = tmp_; } while (0)

Var *read_comma_expr(ReadContext *ctx);

static void emit_assign(ReadContext *ctx, Var *v0, Var *v1);
static Var *read_assign_expr(ReadContext *ctx);
static Var *read_unary_expr(ReadContext *ctx);
static Var *read_func_call(ReadContext *ctx, Token *fntok);
static Var *read_expr1(ReadContext *ctx, Var *v0, int prec0);
static Token *read_ident(ReadContext *ctx);
static void read_compound_stmt(ReadContext *ctx);
static void read_decl_or_stmt(ReadContext *ctx);
static void read_stmt(ReadContext *ctx);
static Var *read_subscript_expr(ReadContext *ctx, Var *a);
static bool is_type_keyword(Token *tok);

/*============================================================
 * Error handlers
 */

NORETURN void error_token(Token *tok, char *msg, ...) {
    va_list ap;
    va_start(ap, msg);
    print_parse_error(tok->line, tok->column, msg, ap);
    va_end(ap);
}

static NORETURN void error_ctx(ReadContext *ctx, char *msg, ...) {
    va_list ap;
    va_start(ap, msg);
    print_parse_error(ctx->file->line, ctx->file->column, msg, ap);
    va_end(ap);
}

/*============================================================
 * Variables
 */

static Ctype *make_ctype(int type) {
    Ctype *r = malloc(sizeof(Ctype));
    r->type = type;
    r->ptr = NULL;
    r->size = 1;
    r->signedp = true;
    return r;
}

static Ctype *make_ctype_ptr(Ctype *type) {
    Ctype *r = malloc(sizeof(Ctype));
    r->type = CTYPE_PTR;
    r->ptr = type;
    r->size = 1;
    r->signedp = true;
    return r;
}

static Ctype *make_ctype_array(Ctype *type, int size) {
    Ctype *r = malloc(sizeof(Ctype));
    r->type = CTYPE_ARRAY;
    r->ptr = type;
    r->size = size;
    r->signedp = true;
    return r;
}

static Var *make_var(Ctype *ctype) {
    Var *r = malloc(sizeof(Var));
    r->stype = VAR_LOCAL;
    r->ctype = ctype;
    r->val.l = 0;
    r->name = NULL;
    r->loc = NULL;
    r->need_save = true;
    return r;
}

static Var *make_deref_var(Var *var) {
    ASSERT(var->ctype->ptr);
    // No expressions except string literals can be lvalues if their
    // type is "array of...".  C:ARM p.204.
    if (var->ctype->ptr->type == CTYPE_ARRAY) {
        Var *r = make_var(var->ctype->ptr);
        r->loc = var;
        return r;
    }
    Var *r = make_var(make_ctype(CTYPE_INVALID));
    r->loc = var;
    return r;
}

static Var *make_imm1(Ctype *type, Cvalue val) {
    Var *r = make_var(type);
    r->stype = VAR_IMM;
    r->val = val;
    return r;
}

static Var *make_imm(int ctype, Cvalue val) {
    return make_imm1(make_ctype(ctype), val);
}

static Var *make_extern(String *name) {
    Var *r = make_var(make_ctype(CTYPE_INT));
    r->name = name;
    r->stype = VAR_EXTERN;
    return r;
}

static Function *make_function(String *name, Ctype *rettype, List *params) {
    Function *r = malloc(sizeof(Function));
    r->name = name;
    r->rettype = rettype;
    r->params = params;
    r->entry = NULL;
    return r;
}

static void ensure_lvalue(ReadContext *ctx, Var *var) {
    if (var->loc || var->name)
        return;
    error_ctx(ctx, "expected lvalue, but got %p", var);
}

int ctype_sizeof(Ctype *ctype) {
    if (ctype->type == CTYPE_ARRAY) {
        return ctype->size * ctype_sizeof(ctype->ptr);
    }
    switch (ctype->type) {
    case CTYPE_FLOAT: return 8;
    case CTYPE_PTR:   return 8;
    case CTYPE_LONG:  return 8;
    case CTYPE_INT:   return 4;
    case CTYPE_SHORT: return 2;
    case CTYPE_CHAR:  return 1;
    default: panic("unknown type: %d", ctype->type);
    }
}

bool ctype_equal(Ctype *ctype, int type) {
    if (ctype->ptr) return false;
    if (ctype->type != type) return false;
    return true;
}

static bool require_consts(ReadContext *ctx, Var *v0, Var *v1) {
    return v0->stype == VAR_IMM
        && v1->stype == VAR_IMM
        && v0->ctype->type == CTYPE_INT
        && v1->ctype->type == CTYPE_INT;
}

/*============================================================
 * Basic block
 */

static Block *make_block() {
    Block *r = malloc(sizeof(Block));
    r->pos = -1;
    r->code = make_list();
    return r;
}

ReadContext *make_read_context(File *file, Elf *elf, CppContext *cppctx) {
    ReadContext *r = malloc(sizeof(ReadContext));
    r->file = file;
    r->elf = elf;
    r->var_frame = make_list1(make_string_dict());
    r->tag = make_list1(make_string_dict());
    r->entry = make_block();
    r->blockstack = make_list();
    list_push(r->blockstack, r->entry);
    r->ungotten = make_list();
    r->onbreak = NULL;
    r->oncontinue = NULL;
    r->label = make_string_dict();
    r->label_tbf = make_string_dict();
    r->cppctx = cppctx;
    return r;
}

static void push_scope(ReadContext *ctx) {
    list_push(ctx->var_frame, make_list());
}

static void pop_scope(ReadContext *ctx) {
    list_pop(ctx->var_frame);
}

static void push_block(ReadContext *ctx, Block *block) {
    list_push(ctx->blockstack, block);
}

static Block *pop_block(ReadContext *ctx) {
    return list_pop(ctx->blockstack);
}

static Block *replace_block(ReadContext *ctx, Block *block) {
    Block *r = LIST_BOTTOM(ctx->blockstack);
    LIST_BOTTOM(ctx->blockstack) = block;
    return r;
}

static void emit(ReadContext *ctx, Inst *inst) {
    if (LIST_LEN(ctx->blockstack) == 0)
        panic("control block stack is empty");
    Block *block = LIST_BOTTOM(ctx->blockstack);
    list_push(block->code, inst);
}

static void add_local_var(ReadContext *ctx, String *name, Var *var) {
    List *current_frame = LIST_BOTTOM(ctx->var_frame);
    list_push(current_frame, name);
    list_push(current_frame, var);
}

static Var *find_var(ReadContext *ctx, String *name) {
    for (int i = LIST_LEN(ctx->var_frame) - 1; i >= 0; i--) {
        List *scope = LIST_REF(ctx->var_frame, i);
        for (int j = 0; j < LIST_LEN(scope); j += 2) {
            String *str = LIST_REF(scope, j);
            if (string_equal(str, name))
                return LIST_REF(scope, j + 1);
        }
    }
    return NULL;
}

static Var *rv(ReadContext *ctx, Var *v) {
    if (!v->loc)
        return v;
    if (v->loc->ctype->ptr->type == CTYPE_ARRAY) {
        Var *r = make_var(make_ctype_ptr(v->loc->ctype->ptr->ptr));
        emit(ctx, make_inst2(OP_ASSIGN, r, v->loc));
        return r;
    }
    if (!v->loc->ctype->ptr)
        panic("pointed variable is not a pointer?");
    Var *r = make_var(v->loc->ctype->ptr);
    emit(ctx, make_inst2(OP_DEREF, r, v->loc));
    return r;
}

/*
 * See C:ARM p.197 6.3.3 The Usual Unary Conversions.
 */
static Var *unary_conv(ReadContext *ctx, Var *var) {
    var = rv(ctx, var);
    if (var->ctype->type == CTYPE_ARRAY) {
        Var *r = make_var(make_ctype_ptr(var->ctype->ptr));
        emit(ctx, make_inst2(OP_ADDRESS, r, var));
        return r;
    }
    if (ctype_sizeof(var->ctype) >= 4)
        return var;
    Var *r = make_var(make_ctype(CTYPE_INT));
    emit(ctx, make_inst2(OP_ASSIGN, r, var));
    return r;
}

/*
 * See C:ARM p.198 6.3.4 The Usual Binary Conversions.
 */
static Ctype *binary_type_conv(Var *v0, Var *v1) {
    if (is_flonum(v0->ctype) || is_flonum(v1->ctype))
        return make_ctype(CTYPE_FLOAT);
    return (ctype_sizeof(v0->ctype) < ctype_sizeof(v1->ctype)) ? v1->ctype : v0->ctype;
}

static Var *calc_const(int op, Var *v0, Var *v1) {
    switch (op) {
#define RETURN_IMM(c_, op_) \
    case c_: return make_imm(CTYPE_INT, (Cvalue)(v0->val.i op_ v1->val.i))
    RETURN_IMM('+', +);
    RETURN_IMM('-', -);
    RETURN_IMM('/', /);
    RETURN_IMM('*', *);
    RETURN_IMM('%', %);
    RETURN_IMM('<', <);
    RETURN_IMM('>', >);
    RETURN_IMM('^', ^);
    RETURN_IMM('|', |);
    RETURN_IMM(OP_SHL, <<);
    RETURN_IMM(OP_SHR, >>);
#undef RETURN_IMM
    default:
        panic("operator %d is not supported yet", op);
    }
}

static Var *emit_arith(ReadContext *ctx, int op, Var *v0, Var *v1) {
    if (require_consts(ctx, v0, v1))
        return calc_const(op, v0, v1);

    switch (op) {
    case '+': {
        // C:ARM p.229 7.6.2 Additive Operators.
        if (v1->ctype->ptr)
            SWAP(Var *, v0, v1);
        if (v0->ctype->type == CTYPE_PTR) {
            if (v1->ctype->type != CTYPE_INT)
                error_ctx(ctx, "arithmetic + is not defined for pointers except integer operand");
            Var *size = make_imm(CTYPE_INT, (Cvalue)ctype_sizeof(v0->ctype->ptr));
            Var *r = make_var(v0->ctype);
            Var *tmp = make_var(make_ctype(CTYPE_LONG));
            emit(ctx, make_inst3('*', tmp, v1, size));
            emit(ctx, make_inst3('+', r, v0, tmp));
            return r;
        }
        Var *r = make_var(binary_type_conv(v0, v1));
        emit(ctx, make_inst3('+', r, v0, v1));
        return r;
    }
    case '-':
        // C:ARM p.230 7.6.2 Subtraction.
        if (v0->ctype->type == CTYPE_PTR && v1->ctype->type == CTYPE_INT) {
            Var *size = make_imm(CTYPE_INT, (Cvalue)ctype_sizeof(v0->ctype->ptr));
            Var *r = make_var(v0->ctype);
            Var *tmp = make_var(make_ctype(CTYPE_LONG));
            emit(ctx, make_inst3('*', tmp, v1, size));
            emit(ctx, make_inst3('-', r, v0, tmp));
            return r;
        } else if (v0->ctype->type == CTYPE_PTR && v1->ctype->type == CTYPE_PTR) {
            Var *size = make_imm(CTYPE_INT, (Cvalue)ctype_sizeof(v0->ctype->ptr));
            // [TODO] CTYPE_LONG should be ptrdiff_t.  See C:ARM p.231.
            Var *r = make_var(make_ctype(CTYPE_LONG));
            emit(ctx, make_inst3('-', r, v0, v1));
            emit(ctx, make_inst3('/', r, r, size));
            return r;
        } else if (v0->ctype->type == CTYPE_PTR) {
            error_ctx(ctx, "arithmetic - is not defined for pointer and type %d", v1->ctype->type);
        }
        Var *r = make_var(binary_type_conv(v0, v1));
        emit(ctx, make_inst3('-', r, v0, v1));
        return r;
    default:
        panic("unsupported operator: %c", op);
    }
}

static Var *emit_arith_inst3(ReadContext *ctx, int op, Var *v0, Var *v1) {
    if (require_consts(ctx, v0, v1))
        return calc_const(op, v0, v1);
    Var *r = make_var(binary_type_conv(v0, v1));
    emit(ctx, make_inst3(op, r, v0, v1));
    return r;
}

static Var *emit_log_inst3(ReadContext *ctx, int op, Var *v0, Var *v1) {
    Var *r = make_var(make_ctype(CTYPE_INT));
    emit(ctx, make_inst3(op, r, v0, v1));
    return r;
}

static Var *type_conv(ReadContext *ctx, Var *dst, Var *src) {
    Var *r = make_var(dst->ctype);
    if (is_flonum(dst->ctype) && src->ctype->type == CTYPE_INT) {
        emit(ctx, make_inst2(OP_I2F, r, src));
        return r;
    }
    if (dst->ctype->type == CTYPE_INT && is_flonum(src->ctype)) {
        emit(ctx, make_inst2(OP_F2I, r, src));
        return r;
    }
    return src;
}

static void emit_assign(ReadContext *ctx, Var *v0, Var *v1) {
    ensure_lvalue(ctx, v0);
    v1 = type_conv(ctx, v0, unary_conv(ctx, v1));
    if (v0->loc) {
        emit(ctx, make_inst2(OP_ASSIGN_DEREF, v0->loc, v1));
    } else {
        emit(ctx, make_inst2(OP_ASSIGN, v0, v1));
    }
}

static Var *emit_post_inc_dec(ReadContext *ctx, Var *var, int op) {
    ensure_lvalue(ctx, var);
    Var *copy = make_var(var->ctype);
    emit(ctx, make_inst2(OP_ASSIGN, copy, var));
    Var *tmp = emit_arith(ctx, op, unary_conv(ctx, var), make_imm(CTYPE_INT, (Cvalue)1));
    emit_assign(ctx, var, tmp);
    return copy;
}

static Var *emit_post_inc(ReadContext *ctx, Var *var) {
    return emit_post_inc_dec(ctx, var, '+');
}

static Var *emit_post_dec(ReadContext *ctx, Var *var) {
    return emit_post_inc_dec(ctx, var, '-');
}

static Var *emit_log_and_or(ReadContext *ctx, Var *condvar, Var *v0, Var *v1, Block *then, Block *els) {
    Block *cont = make_block();
    Var *r = make_var(binary_type_conv(v0, v1));

    emit(ctx, make_inst4(OP_IF, unary_conv(ctx, condvar), then, els, cont));

    push_block(ctx, then);
    emit(ctx, make_inst2(OP_ASSIGN, r, v0));
    pop_block(ctx);

    push_block(ctx, els);
    emit(ctx, make_inst2(OP_ASSIGN, r, v1));
    pop_block(ctx);

    replace_block(ctx, cont);
    return r;
}

static Var *emit_log_and(ReadContext *ctx, Var *v0, Var *v1) {
    Block *then = pop_block(ctx);
    Block *els = make_block();
    return emit_log_and_or(ctx, v0, v1, v0, then, els);
}

static Var *emit_log_or(ReadContext *ctx, Var *v0, Var *v1) {
    Block *then = make_block();
    Block *els = pop_block(ctx);
    return emit_log_and_or(ctx, v0, v0, v1, then, els);
}

/*============================================================
 * Parser
 */

static Token *read_token_nonnull(ReadContext *ctx) {
    Token *tok = read_token(ctx);
    if (!tok)
        error_ctx(ctx, "premature end of input");
    return tok;
}

static bool is_keyword(Token *tok, int type) {
    return tok && tok->toktype == TOKTYPE_KEYWORD && tok->val.i == type;
}

static void unget_token(ReadContext *ctx, Token *tok) {
    if (tok)
        list_push(ctx->ungotten, tok);
}

static Token *peek_token(ReadContext *ctx) {
    Token *r = read_token(ctx);
    unget_token(ctx, r);
    return r;
}

static Token *peek_token_nonnull(ReadContext *ctx) {
    Token *tok = peek_token(ctx);
    if (!tok)
        error_ctx(ctx, "premature end of input");
    return tok;
}

static bool next_token_is(ReadContext *ctx, int keyword) {
    Token *tok = read_token(ctx);
    if (is_keyword(tok, keyword))
        return true;
    unget_token(ctx, tok);
    return false;
}

char *keyword_to_string(int v) {
    switch (v) {
#define KEYWORD(id, str) case id: return str;
#define PUNCT(id, str) case id: return str;
# include "keyword.h"
#undef PUNCT
#undef KEYWORD
    default:
        panic("should not reach here");
    }
}

char *token_to_string(Token *tok) {
    if (!tok)
        return "(nil)";
    String *r = make_string();
    switch (tok->toktype) {
    case TOKTYPE_KEYWORD:
    case TOKTYPE_PUNCT:
        if (tok->val.i < 128)
            string_printf(r, "%c", tok->val.i);
        else
            string_printf(r, "%s", keyword_to_string(tok->val.i));
        break;
    case TOKTYPE_CHAR:
        string_printf(r, "'%c'", tok->val.i);
        break;
    case TOKTYPE_IDENT:
        string_append(r, STRING_BODY(tok->val.str));
        break;
    case TOKTYPE_STRING:
        string_append(r, STRING_BODY(tok->val.str));
        break;
    case TOKTYPE_INT:
        string_printf(r, "%d", tok->val.i);
        break;
    case TOKTYPE_FLOAT:
        string_printf(r, "%f", tok->val.f);
        break;
    case TOKTYPE_CPPNUM:
        string_printf(r, "%s", STRING_BODY(tok->val.str));
        break;
    case TOKTYPE_NEWLINE:
        string_printf(r, "\\n");
        break;
    case TOKTYPE_SPACE:       panic("got TOKTYPE_SPACE");
    case TOKTYPE_INVALID:     panic("got TOKTYPE_INVALID");
    case TOKTYPE_MACRO_PARAM: panic("got TOKTYPE_MACRO_PARAM");
    }
    return STRING_BODY(r);
}

static void expect(ReadContext *ctx, int expected) {
    Token *tok = read_token(ctx);
    if (tok->toktype != TOKTYPE_KEYWORD)
        error_token(tok, "keyword expected, but got '%s'", token_to_string(tok));
    if (!is_keyword(tok, expected))
        error_token(tok, "'%c' expected, but got '%c'", expected, tok->val.i);
}

static void process_break(ReadContext *ctx, Token *tok) {
    if (!ctx->onbreak)
        error_token(tok, "'break' statement not in loop or switch");
    emit(ctx, make_inst1(OP_JMP, ctx->onbreak));
}

static void process_continue(ReadContext *ctx, Token *tok) {
    if (!ctx->oncontinue)
        error_token(tok, "'continue' statement not in loop statement");
    emit(ctx, make_inst1(OP_JMP, ctx->oncontinue));
}

/*
 * primary-expression:
 *     identifier
 *     constant
 *     parenthesized-expression
 *
 * constant:
 *     integer-constant
 *     floating-constant
 *     character-constant
 *     string-constant
 */
static Var *read_primary_expr(ReadContext *ctx) {
    Token *tok = read_token(ctx);
    switch (tok->toktype) {
    case TOKTYPE_CHAR:
        return make_imm(CTYPE_CHAR, (Cvalue)tok->val.i);
    case TOKTYPE_INT:
        return make_imm(CTYPE_INT, (Cvalue)tok->val.i);
    case TOKTYPE_FLOAT:
        return make_imm(CTYPE_FLOAT, (Cvalue)tok->val.f);
    case TOKTYPE_STRING: {
        String *b;
        Token *tok1 = read_token(ctx);
        if (tok1->toktype == TOKTYPE_STRING) {
            b = make_string();
            out(b, STRING_BODY(tok->val.str), STRING_LEN(tok->val.str) - 1);
            do {
                out(b, STRING_BODY(tok1->val.str), STRING_LEN(tok1->val.str) - 1);
                tok1 = read_token(ctx);
            } while (tok1->toktype == TOKTYPE_STRING);
            unget_token(ctx, tok1);
            o1(b, 0);
        } else {
            unget_token(ctx, tok1);
            b = tok->val.str;
        }
        Ctype *type = make_ctype_array(make_ctype(CTYPE_CHAR), STRING_LEN(b));
        return make_imm1(type, (Cvalue)b);
    }
    case TOKTYPE_IDENT: {
        Token *tok1 = read_token(ctx);
        if (is_keyword(tok1, '('))
            return read_func_call(ctx, tok);
        unget_token(ctx, tok1);
        Var *var = find_var(ctx, tok->val.str);
        if (!var) {
            warn("'%s' is not defined", STRING_BODY(tok->val.str));
            var = make_var(make_ctype(CTYPE_INT));
            var->name = tok->val.str;
            add_local_var(ctx, tok->val.str, var);
        }
        return var;
    }
    default:
        error("Line %d: not supported yet", __LINE__);
    }
}

/*
 * postfix-expression:
 *     primary-expression
 *     subscript-expression
 *     component-expression
 *     function-call
 *     postincrement-expression
 *     postdecrement-expression
 *     compound-literal
 */
static Var *read_postfix_expr(ReadContext *ctx) {
    Var *r = read_primary_expr(ctx);
    for (;;) {
        Token *tok = read_token(ctx);
        if (!tok)
            return r;
        else if (is_keyword(tok, KEYWORD_INC))
            r = emit_post_inc(ctx, r);
        else if (is_keyword(tok, KEYWORD_DEC))
            r = emit_post_dec(ctx, r);
        else if (is_keyword(tok, '['))
            r = read_subscript_expr(ctx, r);
        else {
            unget_token(ctx, tok);
            return r;
        }
    }
}

/*
 * function-call:
 *     postfix-expression "(" expression-list? ")"
 *
 * expression-list:
 *     assignment-expression
 *     expression-list "," assignment-expression
 */
static Var *read_func_call(ReadContext *ctx, Token *fntok) {
    List *args = make_list();
    list_push(args, make_extern(fntok->val.str));
    Var *retval = make_var(make_ctype(CTYPE_INT));
    list_push(args, retval);
    Token *tok = read_token(ctx);
    if (!is_keyword(tok, ')')) {
        unget_token(ctx, tok);
        for (;;) {
            Var *v = read_assign_expr(ctx);
            list_push(args, unary_conv(ctx, v));
            Token *sep = read_token(ctx);
            if (sep->toktype != TOKTYPE_KEYWORD)
                error_token(sep, "expected ',', or ')', but got '%c'", sep->val.i);
            if (is_keyword(sep, ')'))
                break;
            if (!is_keyword(sep, ','))
                error_token(sep, "expected ',', but got '%c'", sep->val.i);
        }
    }
    emit(ctx, make_instn(OP_FUNC_CALL, args));
    return retval;
}

static void expect_ident(Token *tok) {
    if (tok->toktype != TOKTYPE_IDENT)
        error_token(tok, "identifier expected, but got '%s'", token_to_string(tok));
}

static Token *read_ident(ReadContext *ctx) {
    Token *r = read_token(ctx);
    expect_ident(r);
    return r;
}

static Var *read_cast_expr(ReadContext *ctx) {
    return read_unary_expr(ctx);
}

/*
 * unary-expression:
 *     postfix-expression
 *     sizeof-expression
 *     unary-minus-expression
 *     unary-plus-expression
 *     logical-negation-expression
 *     bitwise-negation-expression
 *     address-expression
 *     indirection-expression
 *     preincrement-expression
 *     postincrement-expression
 */
static Var *read_unary_expr(ReadContext *ctx) {
    Token *tok = read_token(ctx);
    if (tok->toktype != TOKTYPE_KEYWORD) {
        unget_token(ctx, tok);
        return read_postfix_expr(ctx);
    }
    switch (tok->val.i) {
    case '(': {
        Var *r = read_comma_expr(ctx);
        expect(ctx, ')');
        return r;
    }
    case '+':
    case '-': {
        Var *v = read_cast_expr(ctx);
        Var *tmp = unary_conv(ctx, v);
        Var *zero = is_flonum(tmp->ctype)
            ? make_imm(CTYPE_FLOAT, (Cvalue)0.0f)
            : make_imm(CTYPE_INT, (Cvalue)0);
        Var *r = make_var(binary_type_conv(zero, tmp));
        emit(ctx, make_inst3(tok->val.i, r, zero, tmp));
        return r;
    }
    case '*': {
        Var *pointed = unary_conv(ctx, read_cast_expr(ctx));
        return make_deref_var(pointed);
    }
    case '&': {
        Var *v = read_cast_expr(ctx);
        Var *ptr = make_var(make_ctype_ptr(v->ctype));
        emit(ctx, make_inst2(OP_ADDRESS, ptr, rv(ctx, v)));
        return ptr;
    }
    case '~':
    case '!': {
        Var *v = read_cast_expr(ctx);
        Var *tmp = unary_conv(ctx, v);
        Var *r = make_var(tmp->ctype);
        emit(ctx, make_inst2(tok->val.i, r, tmp));
        return r;
    }
    case KEYWORD_INC:
    case KEYWORD_DEC: {
        Var *v = read_cast_expr(ctx);
        ensure_lvalue(ctx, v);
        char op = tok->val.i == KEYWORD_INC ? '+' : '-';
        Var *tmp = unary_conv(ctx, v);
        tmp = emit_arith(ctx, op, tmp, make_imm(CTYPE_INT, (Cvalue)1));
        emit_assign(ctx, v, tmp);
        return v;
    }
    case KEYWORD_SIZEOF: {
        Var *v = read_unary_expr(ctx);
        return make_imm(CTYPE_INT, (Cvalue)ctype_sizeof(v->ctype));
    }
    default:
        error_token(tok, "expected unary, but got '%s'", token_to_string(tok));
    }
}

static Exp *nread_postfix_expr(ReadContext *ctx) {
    return NULL;
}

static Exp *nread_comma_expr(ReadContext *ctx) {
    return NULL;
}

static Exp *nread_unary_expr(ReadContext *ctx) {
    // ( type-name ) or ( expression )
    if (next_token_is(ctx, '(')) {
        Token *tok = read_token_nonnull(ctx);
        if (is_type_keyword(tok)) {
            unget_token(ctx, tok);
        }
    }

    Token *tok = read_token_nonnull(ctx);
    if (tok->toktype != TOKTYPE_KEYWORD) {
        unget_token(ctx, tok);
        return nread_postfix_expr(ctx);
    }
    switch (tok->val.i) {
    case '(': {
        Exp *r = nread_comma_expr(ctx);
        expect(ctx, ')');
        return r;
    }
    }
    return NULL;
}

/*
 * Returns operators precedence.  There are 15 precedences in C as
 * shown below.
 *
 * 1       () [] -> .                      left
 * 2       ! ~ ++ -- - (type) * & sizeof   right
 * 3       * / %                           left
 * 4       + -                             left
 * 5       >> <<                           left
 * 6       < <= > >=                       left
 * 7       == !=                           left
 * 8       &                               left
 * 9       ^                               left
 * 10      |                               left
 * 11      &&                              left
 * 12      ||                              left
 * 13      ?:                              right
 * 14      = op=                           right
 * 15      ,                               left
 */

int precedence(int op) {
    switch (op) {
    case '[':
        return 1;
    case KEYWORD_INC: case KEYWORD_DEC: case '~':
        return 2;
    case '*': case '/': case '%': case KEYWORD_SIZEOF:
        return 3;
    case '+': case '-':
        return 4;
    case KEYWORD_LSH: case KEYWORD_RSH:
        return 5;
    case '<': case '>': case KEYWORD_GE: case KEYWORD_LE:
        return 6;
    case KEYWORD_EQ: case KEYWORD_NE:
        return 7;
    case '&':
        return 8;
    case '^':
        return 9;
    case '|':
        return 10;
    case KEYWORD_LOG_AND:
        return 11;
    case KEYWORD_LOG_OR:
        return 12;
    case '?':
        return 13;
    case KEYWORD_A_ADD: case KEYWORD_A_SUB: case KEYWORD_A_MUL:
    case KEYWORD_A_DIV: case KEYWORD_A_MOD: case KEYWORD_A_AND:
    case KEYWORD_A_OR:  case KEYWORD_A_XOR: case KEYWORD_A_LSH:
    case KEYWORD_A_RSH: case '=':
        return 14;
    case ',':
        return 15;
    default:
        return -1;
    }
}

static int prec_token(Token *tok) {
    if (!tok || tok->toktype != TOKTYPE_KEYWORD)
        return -1;
    return precedence(tok->val.i);
}


/* Returns true iff a given operator is right-associative. */
static bool is_rassoc(Token *tok) {
    switch (tok->val.i) {
    case KEYWORD_A_ADD: case KEYWORD_A_SUB: case KEYWORD_A_MUL:
    case KEYWORD_A_DIV: case KEYWORD_A_MOD: case KEYWORD_A_AND:
    case KEYWORD_A_XOR: case KEYWORD_INC:   case KEYWORD_DEC:
    case '=': case '?':
        return true;
    default: return false;
    }
}

/*
 * logical-or-expression:
 *     logical-and-expression
 *     logical-or-expression "||" logical-and-expression
 */
static Var *read_logor_expr(ReadContext *ctx) {
    return read_expr1(ctx, read_unary_expr(ctx), 12);
}

/*
 * assignment-expression:
 *     conditional-expression
 *     unary-expression assignment-op assignment-expression
 *
 * assignment-op:
 *    one of: = += -= *= /= %= <<= >>= &= ^= |=
 */
static Var *read_assign_expr(ReadContext *ctx) {
    return read_expr1(ctx, read_unary_expr(ctx), 14);
}

/*
 * comma-expression:
 *     assignment-expression
 *     comma-expression "," assignment-expression
 *
 * This function is public because it is used by preprocessor to evaluate #if
 * directive.
 */
Var *read_comma_expr(ReadContext *ctx) {
    return read_expr1(ctx, read_unary_expr(ctx), 15);
}

/*
 * subscript-expression:
 *     postfix-expression "[" expression "]"
 */
static Var *read_subscript_expr(ReadContext *ctx, Var *a) {
    // a[i] is equivalent to *((a) + (i))
    Var *i = read_comma_expr(ctx);
    expect(ctx, ']');
    Var *var = emit_arith(ctx, '+', unary_conv(ctx, a), unary_conv(ctx, i));
    return make_deref_var(unary_conv(ctx, var));
}

/*
 * conditional-expression:
 *     logical-or-expression
 *     logical-or-expression "?" expression : conditional-expression
 */
static Var *read_cond_expr(ReadContext *ctx, Var *condvar) {
    Block *then = make_block();
    Block *els = make_block();
    Block *cont = make_block();
    Var *r = make_var(make_ctype(CTYPE_INVALID));

    emit(ctx, make_inst4(OP_IF, unary_conv(ctx, condvar), then, els, cont));

    push_block(ctx, then);
    Var *v0 = unary_conv(ctx, read_logor_expr(ctx));
    emit(ctx, make_inst2(OP_ASSIGN, r, v0));
    pop_block(ctx);
    expect(ctx, ':');

    push_block(ctx, els);
    Var *v1 = unary_conv(ctx, read_logor_expr(ctx));
    emit(ctx, make_inst2(OP_ASSIGN, r, v1));
    pop_block(ctx);

    r->ctype = binary_type_conv(v0, v1);
    replace_block(ctx, cont);
    return r;
}

/*
 * Operator-precedence parser.
 */
static Var *read_expr1(ReadContext *ctx, Var *v0, int prec0) {
    for (;;) {
        Token *tok = read_token(ctx);
        int prec1 = prec_token(tok);
        if (prec1 < 0 || prec0 < prec1) {
            unget_token(ctx, tok);
            return v0;
        }
        if (is_keyword(tok, '?')) {
            v0 = read_cond_expr(ctx, v0);
            v0 = unary_conv(ctx, v0);
            continue;
        }
        if (is_keyword(tok, KEYWORD_LOG_AND) || is_keyword(tok, KEYWORD_LOG_OR)) {
            push_block(ctx, make_block());
        }
        Var *v1 = read_unary_expr(ctx);
        for (;;) {
            Token *tok1 = peek_token(ctx);
            int prec2 = prec_token(tok1);
            if (prec2 < 0 || prec1 < prec2 || (prec1 == prec2 && !is_rassoc(tok1))) {
                break;
            }
            v1 = read_expr1(ctx, v1, prec2);
        }
        if (is_keyword(tok, '=')) {
            emit_assign(ctx, v0, v1);
            continue;
        }

        v0 = unary_conv(ctx, v0);
        v1 = unary_conv(ctx, v1);
        int op;
        switch (tok->val.i) {
        case ',':
            v0 = v1;
            break;
        case '+': case '-':
            v0 = emit_arith(ctx, tok->val.i, v0, v1);
            break;
        case '*':
        case '/': {
            v0 = emit_arith_inst3(ctx, tok->val.i, v0, v1);
            break;
        }
        case '^':
        case '&':
        case '|': {
            Var *tmp = make_var(binary_type_conv(v0, v1));
            if (is_flonum(tmp->ctype))
                error_token(tok, "invalid operand to binary '%c'", tok->val.i);
            emit(ctx, make_inst3(tok->val.i, tmp, v0, v1));
            v0 = tmp;
            break;
        }
        case KEYWORD_EQ:
            op = OP_EQ; goto cmp;
        case KEYWORD_NE:
            op = OP_NE; goto cmp;
        case '>':
            SWAP(Var *, v0, v1);
            // FALL THROUGH
        case '<':
            op = '<'; goto cmp;
        case KEYWORD_GE:
            SWAP(Var *, v0, v1);
            // FALL THROUGH
        case KEYWORD_LE:
            op = OP_LE; goto cmp;
        cmp:
            v0 = emit_log_inst3(ctx, op, v0, v1);
            break;
        case KEYWORD_A_ADD:
            op = '+'; goto assign_arith_op;
        case KEYWORD_A_SUB:
            op = '-'; goto assign_arith_op;
        assign_arith_op:
            ensure_lvalue(ctx, v0);
            emit_assign(ctx, v0, emit_arith(ctx, op, v0, v1));
            break;
        case KEYWORD_A_MUL:
            op = '*'; goto assign_op;
        case KEYWORD_A_DIV:
            op = '/'; goto assign_op;
        case KEYWORD_A_AND:
            op = '&'; goto assign_op;
        case KEYWORD_A_OR:
            op = '|'; goto assign_op;
        case KEYWORD_A_XOR:
            op = '^'; goto assign_op;
        case KEYWORD_A_LSH:
            op = OP_SHL; goto assign_op;
        case KEYWORD_A_RSH:
            op = OP_SHR; goto assign_op;
        assign_op:
            ensure_lvalue(ctx, v0);
            emit(ctx, make_inst3(op, v0, v0, v1));
            break;
        case KEYWORD_LOG_AND:
            v0 = emit_log_and(ctx, v0, v1);
            break;
        case KEYWORD_LOG_OR:
            v0 = emit_log_or(ctx, v0, v1);
            break;
        case KEYWORD_LSH:
            v0 = emit_arith_inst3(ctx, OP_SHL, v0, v1);
            break;
        case KEYWORD_RSH:
            v0 = emit_arith_inst3(ctx, OP_SHR, v0, v1);
            break;
        default:
            error_token(tok, "unsupported operator: %s", token_to_string(tok));
        }
    }
    return v0;
}

/*
 * declaration-specifiers:
 *     storage-class-specifier declaration-specifiers?
 *     type-specifier declaration-specifiers?
 *     type-qualifier declaration-specifiers?
 *     function-specifier declaration-specifiers?
 *
 * storage-class-specifier:
 *     one of: "auto" "extern" "register" "static" "typedef"
 *
 * type-specifier:
 *     one of: "void" "char" "short" "int" "long" "float"
 *             "double" "signed" "unsigned" "_Bool" "_Complex"
 *     struct-or-union-specifier
 *     enum-specifier
 *     typedef-name
 *
 * type-qualifier:
 *     one of: "const" "volatile" "restrict"
 *
 * function-specifier:
 *     "inline"
 */
static Ctype *read_declaration_spec(ReadContext *ctx) {
    Ctype *r = NULL;
    Token *tok;
    enum { NONE, SIGNED, UNSIGNED } sign = NONE;
    for (;;) {
        tok = read_token(ctx);
        if (tok->toktype != TOKTYPE_KEYWORD)
            goto end;
        switch (tok->val.i) {
        case KEYWORD_CONST:
            // ignore the type specifier for now.
            break;
        case KEYWORD_SIGNED:
            if (sign == SIGNED)
                error_token(tok, "'signed' specified twice");
            if (sign == UNSIGNED)
                goto sign_error;
            sign = UNSIGNED;
            break;
        case KEYWORD_UNSIGNED:
            if (sign == UNSIGNED)
                error_token(tok, "'unsigned' specified twice");
            if (sign == SIGNED)
                goto sign_error;
            sign = UNSIGNED;
            break;
#define CHECK_DUP()                                                     \
            if (r) error_token(tok, "two or more data types in declaration specifiers");
        case KEYWORD_CHAR:
            CHECK_DUP();
            r = make_ctype(CTYPE_CHAR);
            break;
        case KEYWORD_SHORT:
            CHECK_DUP();
            r = make_ctype(CTYPE_SHORT);
            break;
        case KEYWORD_INT:
            CHECK_DUP();
            r = make_ctype(CTYPE_INT);
            break;
        case KEYWORD_LONG:
            CHECK_DUP();
            r = make_ctype(CTYPE_LONG);
            break;
        case KEYWORD_FLOAT:
            CHECK_DUP();
            if (sign != NONE)
                error_token(tok, "float cannot be signed nor unsigned");
            r = make_ctype(CTYPE_FLOAT);
            break;
#undef CHECK_DUP
        default:
            goto end;
        }
    }
 end:
    if (r)
        r->signedp = (sign != UNSIGNED);
    unget_token(ctx, tok);
    return r ? r : make_ctype(CTYPE_INT);
 sign_error:
    error_token(tok, "both 'signed' and 'unsigned' in declaration specifiers");
}

static void sign_error(Token *tok) {
    error_token(tok, "both 'signed' and 'unsigned' in declaration specifiers");
}

static Ctype *read_array_dimensions(ReadContext *ctx, Ctype *ctype) {
    int size;
    if (next_token_is(ctx, ']')) {
        size = -1;
    } else {
        Token *tok = read_token(ctx);
        if (tok->toktype != TOKTYPE_INT)
            panic("non immediate dimension is not supported yet");
        size = tok->val.i;
        expect(ctx, ']');
    }
    if (next_token_is(ctx, '[')) {
        Ctype *ctype1 = read_array_dimensions(ctx, ctype);
        if (ctype1->size < 0)
            error_ctx(ctx, "Dimension is not specified for a multimentional array");
        return make_ctype_array(ctype1, size);
    }
    return make_ctype_array(ctype, size);
}

static void check_array_dimension(ReadContext *ctx, Exp *exp) {
    if (exp->ctype->type != TINT)
        error_ctx(ctx, "Array dimension must be of int type, but got type %d", exp->type);
    if (exp->type != ECONST)
        error_ctx(ctx, "Only fixed size array is supported");
}

static Exp *nread_array_dimensions(ReadContext *ctx) {
    return NULL;
}

typedef struct Decl {
    Type *ctype;
    String *name;
} Decl;

static Decl *make_decl(Type *ctype, String *name) {
    Decl *r = malloc(sizeof(Decl));
    r->ctype = ctype;
    r->name = name;
    return r;
}

typedef struct FuncParam {
    enum { NO_DECL, KANDR, ANSI } type;
    // List of Decls (ANSI) or Strings (K&R)
    List *param;
} FuncParam;

static FuncParam *make_func_param(int type, List *param) {
    FuncParam *r = malloc(sizeof(FuncParam));
    r->type = type;
    r->param = param;
    return r;
}

static Type *nread_decl_spec(ReadContext *ctx);
static Decl *nread_declarator(ReadContext *ctx, Type *ctype);

static List *nread_param_type_list(ReadContext *ctx) {
    List *r = make_list();
    for (;;) {
        Type *ctype = nread_decl_spec(ctx);
        if (next_token_is(ctx, ',')) {
            list_push(r, make_decl(ctype, NULL));
            continue;
        }
        if (next_token_is(ctx, ')')) {
            list_push(r, make_decl(ctype, NULL));
            break;
        }
        Decl *decl = nread_declarator(ctx, ctype);
        list_push(r, make_decl(decl->ctype, decl->name));
        if (next_token_is(ctx, ','))
            continue;
        if (next_token_is(ctx, ')'))
            break;
    }
    return r;
}

struct List *nread_ident_list(ReadContext *ctx) {
    List *r = make_list();
    for (;;) {
        Token *tok = read_token_nonnull(ctx);
        if (tok->toktype != TOKTYPE_IDENT)
            error("identifier expected, but got '%s'", token_to_string(tok));
        list_push(r, tok->val.str);
        if (next_token_is(ctx, ')'))
            break;
        expect(ctx, ',');
    }
    return r;
}

static FuncParam *nread_func_params(ReadContext *ctx) {
    if (next_token_is(ctx, ')'))
        return make_func_param(NO_DECL, NULL);
    Token *tok = read_token_nonnull(ctx);
    if (is_keyword(tok, KEYWORD_VOID) && next_token_is(ctx, ')'))
        return make_func_param(ANSI, make_list());
    if (is_type_keyword(tok)) {
        unget_token(ctx, tok);
        return make_func_param(ANSI, nread_param_type_list(ctx));
    }
    return make_func_param(KANDR, nread_ident_list(ctx));
}

static Type *nread_func_declarator_params(ReadContext *ctx, Type *ctype) {
    FuncParam *param = nread_func_params(ctx);
    List *ctypes = make_list();
    for (int i = 0; i < LIST_LEN(param->param); i++) {
        Decl *decl = LIST_REF(param->param, i);
        list_push(ctypes, decl->ctype);
    }
    Type *r = make_func_type(ctype, ctypes);
    return r;
}

/*
 * direct-declarator:
 *     identifier
 *     "(" declarator ")"
 *     direct-declarator "[" type-qualifier* assignment-expression? "]"
 *     direct-declarator "[" "static" type-qualifier* assignment-expression "]"
 *     direct-declarator "[" type-qualifier+ "static" assignment-expression "]"
 *     direct-declarator "[" type-qualifier* * "]"
 *     direct-declarator "(" parameter-type-list ")"
 *     direct-declarator "(" identifier-list? ")"
 */
static Var *read_direct_declarator(ReadContext *ctx, Ctype *ctype) {
    Var *r = make_var(ctype);
    Token *tok = read_ident(ctx);
    r->name = tok->val.str;
    if (next_token_is(ctx, '['))
        r->ctype = read_array_dimensions(ctx, r->ctype);
    return r;
}

static Type *nread_direct_or_abst_declarator(ReadContext *ctx, String **pname) {
    if (next_token_is(ctx, '*'))
        return make_ptr_type(nread_direct_or_abst_declarator(ctx, pname));

    Type *ctype = NULL;
    Token *tok = read_token_nonnull(ctx);
    if (tok->toktype == TOKTYPE_IDENT) {
        *pname = tok->val.str;
    } else if (is_keyword(tok, '(')) {
        Token *tok1 = peek_token_nonnull(ctx);
        if (!pname && is_type_keyword(tok1)) {
            ctype = nread_func_declarator_params(ctx, NULL);
            expect(ctx, ')');
            return ctype;
        }
        ctype = nread_direct_or_abst_declarator(ctx, pname);
        expect(ctx, ')');
    } else if (is_keyword(tok, '[')) {
        unget_token(ctx, tok);
    } else {
        return NULL;
    }

    for (;;) {
        Token *tok = read_token_nonnull(ctx);
        if (is_keyword(tok, '(')) {
            ctype = nread_func_declarator_params(ctx, ctype);
        } else if (is_keyword(tok, '[')) {
            ctype = make_array_type(ctype, nread_array_dimensions(ctx));
        } else {
            return ctype;
        }
    }
}

/*
 * pointer-declarator:
 *     pointer direct-declarator
 *
 * pointer:
 *     "*" type-qualifier* pointer?
 */
static Var *read_pointer_declarator(ReadContext *ctx, Ctype *ctype) {
    Var *r = next_token_is(ctx, '*')
        ? read_pointer_declarator(ctx, ctype)
        : read_direct_declarator(ctx, ctype);
    r->ctype = make_ctype_ptr(r->ctype);
    return r;
}

static Decl *nread_direct_declarator(ReadContext *ctx, Type *ctype) {
    Token *tok = read_token_nonnull(ctx);
    if (tok->toktype != TOKTYPE_IDENT)
        panic("direct-declarator is not fully implemented yet");
    return make_decl(ctype, tok->val.str);
}

static Decl *nread_pointer_declarator(ReadContext *ctx, Type *ctype) {
    Decl *r = next_token_is(ctx, '*')
        ? nread_pointer_declarator(ctx, ctype)
        : nread_direct_declarator(ctx, ctype);
    r->ctype = make_ptr_type(r->ctype);
    return r;
}

/*
 * declarator:
 *     pointer-declarator
 *     direct-declarator
 */
static Var *read_declarator(ReadContext *ctx, Ctype *ctype) {
    if (next_token_is(ctx, '*')) {
        return read_pointer_declarator(ctx, ctype);
    }
    return read_direct_declarator(ctx, ctype);
}

static Decl *nread_declarator(ReadContext *ctx, Type *ctype) {
    if (next_token_is(ctx, '*')) {
        return nread_pointer_declarator(ctx, ctype);
    }
    return nread_direct_declarator(ctx, ctype);
}

/*
 * initializer:
 *     assignment-expression
 *     "{" initializer-list ","? "}"
 *
 * initializer-list:
 *     initializer
 *     initializer-list "," initializer
 *     designation initializer
 *     initializer-list "," designation initializer
 *
 * designation:
 *     designator+ "="
 *
 * designator:
 *     pointer-declarator
 *     direct-declarator
 */
static Var *read_initializer(ReadContext *ctx) {
    return read_assign_expr(ctx);
}

/*
 * initialized-declarator:
 *     declarator
 *     declarator "=" initializer
 */
static void read_initialized_declarator(ReadContext *ctx, Ctype *ctype) {
    Var *var = read_declarator(ctx, ctype);
    add_local_var(ctx, var->name, var);
    if (next_token_is(ctx, '=')) {
        Var *val = unary_conv(ctx, read_initializer(ctx));
        emit_assign(ctx, var, val);
        return;
    }
    emit(ctx, make_inst1(OP_ALLOC, var));
}

static bool is_type_keyword(Token *tok) {
    if (tok->toktype != TOKTYPE_KEYWORD)
        return false;
    switch (tok->val.i) {
    case KEYWORD_CONST:
    case KEYWORD_SIGNED:
    case KEYWORD_UNSIGNED:
    case KEYWORD_CHAR:
    case KEYWORD_SHORT:
    case KEYWORD_INT:
    case KEYWORD_LONG:
    case KEYWORD_FLOAT:
        return true;
    default:
        return false;
    }
}

/*
 * declaration:
 *     declaration-specifiers initialized-declarator-list
 *
 * initialized-declarator-list:
 *     initialized-declarator
 *     initialized-declarator-list "," initialized-declarator
 */
static void read_declaration(ReadContext *ctx) {
    Ctype *declspec = read_declaration_spec(ctx);
    for (;;) {
        read_initialized_declarator(ctx, declspec);
        if (!next_token_is(ctx, ','))
            break;
    }
    expect(ctx, ';');
}

/*
 * conditional-statement:
 *     if-statement:
 *     if-else-statement:
 *
 * if-statement:
 *     "if" "(" expression ")" statement
 *
 * if-else-statement:
 *     "if" "(" expression ")" statement "else" statement
 */
static void read_if_stmt(ReadContext *ctx) {
    Block *then = make_block();
    Block *els = make_block();
    Block *cont = make_block();
    expect(ctx, '(');
    Var *cond = read_comma_expr(ctx);
    expect(ctx, ')');

    emit(ctx, make_inst4(OP_IF, unary_conv(ctx, cond), then, els, cont));

    push_block(ctx, then);
    read_stmt(ctx);
    pop_block(ctx);

    if (next_token_is(ctx, KEYWORD_ELSE)) {
        push_block(ctx, els);
        read_stmt(ctx);
        pop_block(ctx);
    }
    replace_block(ctx, cont);
}

/*
 * for-statement:
 *     "for" for-expressions statement
 *
 * for-expressions:
 *     "(" initial-clause? ";" expression? ";" expression? ")"
 *
 * initial-clause:
 *     expression
 *     declaration
 */
static void read_for_stmt(ReadContext *ctx) {
    Block *cond = make_block();
    Block *mod = make_block();
    Block *body = make_block();
    Block *cont = make_block();
    push_scope(ctx);

    expect(ctx, '(');
    if (!next_token_is(ctx, ';'))
        read_decl_or_stmt(ctx);

    emit(ctx, make_inst1(OP_JMP, cond));

    push_block(ctx, cond);
    Var *condvar = read_comma_expr(ctx);
    emit(ctx, make_inst4(OP_IF, condvar, body, NULL, cont));
    pop_block(ctx);
    expect(ctx, ';');

    push_block(ctx, mod);
    read_comma_expr(ctx);
    emit(ctx, make_inst1(OP_JMP, cond));
    pop_block(ctx);
    expect(ctx, ')');

    Block *orig_onbreak = ctx->onbreak;
    Block *orig_oncontinue = ctx->oncontinue;
    ctx->onbreak = cont;
    ctx->oncontinue = mod;
    push_block(ctx, body);
    push_scope(ctx);
    read_stmt(ctx);
    pop_scope(ctx);
    emit(ctx, make_inst1(OP_JMP, mod));
    pop_block(ctx);
    ctx->oncontinue = orig_oncontinue;
    ctx->onbreak = orig_onbreak;

    pop_scope(ctx);
    replace_block(ctx, cont);
}

/*
 * while-statement:
 *     "while" "(" expression ")" statement
 */
static void read_while_stmt(ReadContext *ctx) {
    Block *cond = make_block();
    Block *body = make_block();
    Block *cont = make_block();

    emit(ctx, make_inst1(OP_JMP, cond));

    expect(ctx, '(');
    push_block(ctx, cond);
    Var *condvar = read_comma_expr(ctx);
    emit(ctx, make_inst4(OP_IF, condvar, body, NULL, cont));
    pop_block(ctx);
    expect(ctx, ')');

    Block *orig_onbreak = ctx->onbreak;
    Block *orig_oncontinue = ctx->oncontinue;
    ctx->oncontinue = cond;
    ctx->onbreak = cont;
    push_block(ctx, body);
    push_scope(ctx);
    read_stmt(ctx);
    pop_scope(ctx);
    emit(ctx, make_inst1(OP_JMP, cond));
    pop_block(ctx);
    ctx->oncontinue = orig_oncontinue;
    ctx->onbreak = orig_onbreak;

    replace_block(ctx, cont);
}

/*
 * do-statement:
 *     "do" statmenet "(" expression ")" ";"
 */
static void read_do_stmt(ReadContext *ctx) {
    Block *body = make_block();

    Block *cond = make_block();
    Block *cont = make_block();

    emit(ctx, make_inst1(OP_JMP, body));

    Block *orig_onbreak = ctx->onbreak;
    Block *orig_oncontinue = ctx->oncontinue;
    ctx->oncontinue = body;
    ctx->onbreak = cont;
    push_block(ctx, body);
    push_scope(ctx);
    read_stmt(ctx);
    pop_scope(ctx);
    emit(ctx, make_inst1(OP_JMP, cond));
    pop_block(ctx);
    ctx->oncontinue = orig_oncontinue;
    ctx->onbreak = orig_onbreak;

    expect(ctx, KEYWORD_WHILE);
    expect(ctx, '(');
    push_block(ctx, cond);
    Var *condvar = read_comma_expr(ctx);
    emit(ctx, make_inst4(OP_IF, condvar, body, NULL, cont));
    pop_block(ctx);
    expect(ctx, ')');
    expect(ctx, ';');

    replace_block(ctx, cont);
}

/*
 * labeled-statement:
 *     label ":" statement
 *
 * label:
 *     identifier
 *     "case" identifier
 *     "default"
 */
static void process_label(ReadContext *ctx, Token *tok) {
    expect_ident(tok);
    String *label = tok->val.str;

    if (dict_get(ctx->label, label))
        error_token(tok, "duplicate label: %s", STRING_BODY(label));

    Block *cont = make_block();
    emit(ctx, make_inst1(OP_JMP, cont));
    replace_block(ctx, cont);

    dict_put(ctx->label, label, cont);

    List *tbf = dict_get(ctx->label_tbf, label);
    if (!tbf)
        return;
    for (int i = 0; i < LIST_LEN(tbf); i += 2) {
        Block *block = LIST_REF(tbf, i);
        push_block(ctx, block);
        emit(ctx, make_inst1(OP_JMP, cont));
        pop_block(ctx);
    }
    dict_delete(ctx->label_tbf, label);
}

/*
 * goto-statement:
 *     "goto" identifier ";"
 */
static void read_goto_stmt(ReadContext *ctx) {
    Token *tok = read_ident(ctx);
    expect(ctx, ';');
    String *label = tok->val.str;

    Block *dst = dict_get(ctx->label, label);
    if (dst) {
        emit(ctx, make_inst1(OP_JMP, dst));
        return;
    }
    List *blocks = dict_get(ctx->label_tbf, label);
    if (!blocks) {
        blocks = make_list();
        dict_put(ctx->label_tbf, label, blocks);
    }
    Block *cur = replace_block(ctx, make_block());
    list_push(blocks, cur);
    list_push(blocks, tok);
}

/*
 * return-statement:
 *     "return" expression? ";"
 */
static void read_return_stmt(ReadContext *ctx) {
    Token *tok = read_token(ctx);
    Var *retval;
    if (is_keyword(tok, ';')) {
        retval = make_imm(CTYPE_INT, (Cvalue)0);
    } else {
        unget_token(ctx, tok);
        retval = read_comma_expr(ctx);
        expect(ctx, ';');
    }
    emit(ctx, make_inst1(OP_RETURN, retval));
}

static void check_context(ReadContext *ctx) {
    DictIter *iter = make_dict_iter(ctx->label_tbf);
    void **p;
    for (p = dict_iter_next(iter); p; p = dict_iter_next(iter)) {
        List *list = p[1];
        Token *tok = LIST_REF(list, 1);
        error_token(tok, "dangling goto label: '%s'", STRING_BODY(tok->val.str));
    }
}

/*
 * statement:
 *     expression-statement
 *     labeled-statement
 *     compound-statement
 *     conditional-statement
 *     iterative-statement
 *     switch-statement
 *     break-statement
 *     continue-statement
 *     return-statement
 *     goto-statement
 *     null-statement
 *
 * iterative-statement:
 *     while-statement
 *     do-statement
 *     for-statement
 *
 * null-statement:
 *     ";"
 */
static void read_stmt(ReadContext *ctx) {
    Token *tok = read_token(ctx);
    if (is_keyword(tok, KEYWORD_IF)) {
        read_if_stmt(ctx);
    } else if (is_keyword(tok, KEYWORD_FOR)) {
        read_for_stmt(ctx);
    } else if (is_keyword(tok, KEYWORD_WHILE)) {
        read_while_stmt(ctx);
    } else if (is_keyword(tok, KEYWORD_DO)) {
        read_do_stmt(ctx);
    } else if (is_keyword(tok, KEYWORD_BREAK)) {
        expect(ctx, ';');
        process_break(ctx, tok);
    } else if (is_keyword(tok, KEYWORD_CONTINUE)) {
        expect(ctx, ';');
        process_continue(ctx, tok);
    } else if (is_keyword(tok, KEYWORD_GOTO)) {
        read_goto_stmt(ctx);
    } else if (is_keyword(tok, KEYWORD_RETURN)) {
        read_return_stmt(ctx);
    } else if (is_keyword(tok, '{')) {
        read_compound_stmt(ctx);
    } else {
        Token *tok1 = read_token(ctx);
        if (is_keyword(tok1, ':')) {
            process_label(ctx, tok);
        } else {
            unget_token(ctx, tok1);
            unget_token(ctx, tok);
        }
        read_comma_expr(ctx);
        Token *tok2 = read_token(ctx);
        if (!is_keyword(tok2, ';'))
            error_token(tok2, "';' expected, but got '%s'", token_to_string(tok2));
    }
}

/*
 * declaration-or-statemnet:
 *     declaration
 *     statement
 */
static void read_decl_or_stmt(ReadContext *ctx) {
    Token *tok = peek_token(ctx);
    if (is_type_keyword(tok)) {
        read_declaration(ctx);
    } else {
        read_stmt(ctx);
    }
}

/*
 * compound-statemnet:
 *     "{" declaration-or-statment* "}"
 */
static void read_compound_stmt(ReadContext *ctx) {
    push_scope(ctx);
    for (;;) {
        Token *tok = read_token(ctx);
        if (is_keyword(tok, '}'))
            break;
        unget_token(ctx, tok);
        read_decl_or_stmt(ctx);
    }
    pop_scope(ctx);
}

/*
 * parameter-type-list:
 *     parameter-list
 *     parameter-list "," "..."
 *
 * parameter-list:
 *     parameter-declaration
 *     parameter-list "," parameter-declaration
 *
 * parameter-declaration:
 *     declaration-specifiers declarator
 *     declaration-specifiers abstract-declarator?
 */
static List *read_param_type_list(ReadContext *ctx) {
    List *params = make_list();
    if (next_token_is(ctx, ')'))
        return params;
    for (;;) {
        Ctype *type = read_declaration_spec(ctx);
        Var *param = read_declarator(ctx, type);
        if (param->ctype->type == CTYPE_ARRAY)
            param->ctype = make_ctype_ptr(param->ctype->ptr);
        add_local_var(ctx, param->name, param);
        list_push(params, param);
        if (next_token_is(ctx, ')'))
            return params;
        expect(ctx, ',');
    }
}

/*
 * function-declaration:
 *     function-def-specifier compound-statement
 *
 * function-def-specifier:
 *     declaration-specifiers? declarator declaration*
 *
 * function-declarator:
 *     direct-declarator "(" parameter-type-list ")"
 *     direct-declarator "(" identifier* ")"
 *
 * abstract-declarator:
 *     pointer? direct-abstract-declarator
 *
 * direct-abstract-declarator:
 *     "(" abstract-declarator ")"
 *     direct-abstract-declarator? "[" constant-expression? "]"
 *     direct-abstract-declarator? "[" expression "]"
 *     direct-abstract-declarator? "[" "*" "]"
 *     direct-abstract-declarator? "(" parameter-type-list? ")"
 */
static Function *read_func_declaration(ReadContext *ctx) {
    Ctype *rettype = read_declaration_spec(ctx);
    Token *fname = read_ident(ctx);
    expect(ctx, '(');
    push_scope(ctx);
    List *params = read_param_type_list(ctx);

    Function *fn = make_function(fname->val.str, rettype, params);
    ctx->func = fn;

    expect(ctx, '{');
    read_compound_stmt(ctx);
    pop_scope(ctx);

    Block *epilogue = make_block();
    emit(ctx, make_inst1(OP_JMP, epilogue));
    push_block(ctx, epilogue);
    emit(ctx, make_inst1(OP_RETURN, make_imm(CTYPE_INT, (Cvalue)0)));
    pop_block(ctx);

    fn->entry = ctx->entry;
    return fn;
}

/*
 * struct-declaration-list:
 *     struct-declaration
 *     struct-declaration-list struct-declaration
 *
 * struct-declaration:
 *     specifier-qualifier-list struct-declarator-list ";"
 *
 * specifier-qualifier-list:
 *     type-specifier specifier-qualifier-list?
 *     type-qualifier specifier-qualifier-listopt
 *
 * struct-declarator-list:
 *     struct-declarator
 *     struct-declarator-list "," struct-declarator
 *
 * struct-declarator:
 *     declarator
 *     declarator? ":" constant-expression
 */

static Field *make_field(Type *ctype, String *name, int bitfield) {
    Field *r = malloc(sizeof(Field));
    r->ctype = ctype;
    r->name = name;
    r->bitfield = bitfield;
    return r;
}

static int read_const_expr(ReadContext *ctx) {
    Token *tok = read_token_nonnull(ctx);
    if (tok->toktype != TOKTYPE_INT)
        panic("read_const_expr is not fully implemented yet");
    return tok->val.i;
}

static Field *read_struct_decl(ReadContext *ctx) {
    Type *ctype = NULL;
    String *name = NULL;
    int bitfield = -1;

    ctype = nread_decl_spec(ctx);
    Token *tok = read_token_nonnull(ctx);
    if (!is_keyword(tok, ':')) {
        unget_token(ctx, tok);
        Decl *decl = nread_declarator(ctx, ctype);
        ctype = decl->ctype;
        name = decl->name;
        tok = read_token_nonnull(ctx);
    }
    if (is_keyword(tok, ':'))
        bitfield = read_const_expr(ctx);
    else
        unget_token(ctx, tok);
    expect(ctx, ';');
    return make_field(ctype, name, bitfield);
}

static List *read_struct_decl_list(ReadContext *ctx) {
    List *r = make_list();
    for (;;) {
        Token *tok = peek_token_nonnull(ctx);
        if (is_keyword(tok, '}'))
            return r;
        Field *field = read_struct_decl(ctx);
        list_push(r, field);
    }
}

static Type *find_struct_or_union(ReadContext *ctx, String *tag) {
    for (int i = LIST_LEN(ctx->tag) - 1; i >= 0; i--) {
        Dict *dict = LIST_REF(ctx->tag, i);
        Type *r = dict_get(dict, tag);
        if (r) return r;
    }
    return NULL;
}

static void add_struct_or_union(ReadContext *ctx, String *tag, Type *ctype) {
    Dict *dict = LIST_BOTTOM(ctx->tag);
    dict_put(dict, tag, ctype);
}

/*
 * struct-or-union-specifier:
 *     struct-or-union identifier? "{" struct-declaration-list "}"
 *     struct-or-union identifier
 */
static Type *nread_struct_or_union_spec(ReadContext *ctx, TypeEnum type) {
    Token *nametok = NULL;
    String *name = NULL;

    Token *tok = read_token_nonnull(ctx);
    if (tok->toktype == TOKTYPE_IDENT) {
        nametok = tok;
        name = tok->val.str;
        tok = read_token(ctx);
    }

    if (is_keyword(tok, '{')) {
        List *field = read_struct_decl_list(ctx);
        return make_struct_or_union_type(type, name, field);
    } else if (!nametok) {
        error_token(nametok, "'{' or identifier expected, but got %s", token_to_string(nametok));
    } else
        unget_token(ctx, tok);

    Type *r = find_struct_or_union(ctx, name);
    if (!r) {
        r = make_struct_or_union_type(type, name, NULL);
        add_struct_or_union(ctx, name, r);
        return r;
    }
    if ((r->type == TSTRUCT && type == TUNION)
        || (r->type == TUNION && type == TSTRUCT))
        error_token(nametok, "tag type '%s' does not match previous declaration", name);
    return r;
}

/*
 * declaration-specifiers:
 *     storage-class-specifier declaration-specifiers?
 *     type-specifier declaration-specifiers?
 *     type-qualifier declaration-specifiers?
 *     function-specifier declaration-specifiers?
 *
 * storage-class-specifier:
 *     one of: "auto" "extern" "register" "static" "typedef"
 *
 * type-specifier:
 *     one of: "void" "char" "short" "int" "long" "float"
 *             "double" "signed" "unsigned" "_Bool" "_Complex"
 *     struct-or-union-specifier
 *     enum-specifier
 *     typedef-name
 *
 * type-qualifier:
 *     one of: "const" "volatile" "restrict"
 *
 * function-specifier:
 *     "inline"
 */
static Type *nread_decl_spec(ReadContext *ctx) {
    Type *r = NULL;
    Token *tok;
    enum { NONE, SIGNED, UNSIGNED } sign = NONE;
    for (;;) {
        tok = read_token(ctx);
        if (tok->toktype != TOKTYPE_KEYWORD)
            goto end;
        switch (tok->val.i) {
        case KEYWORD_STRUCT:
            r = nread_struct_or_union_spec(ctx, TSTRUCT);
            break;
        case KEYWORD_UNION:
            r = nread_struct_or_union_spec(ctx, TUNION);
            break;
        case KEYWORD_CONST:
            // ignore the type specifier for now.
            break;
        case KEYWORD_SIGNED:
            if (sign == SIGNED)
                error_token(tok, "'signed' specified twice");
            if (sign == UNSIGNED)
                sign_error(tok);
            sign = SIGNED;
            break;
        case KEYWORD_UNSIGNED:
            if (sign == UNSIGNED)
                error_token(tok, "'unsigned' specified twice");
            if (sign == SIGNED)
                sign_error(tok);
            sign = UNSIGNED;
            break;
#define CHECK_DUP()                                                     \
            if (r) error_token(tok, "two or more data types in declaration specifiers");
        case KEYWORD_CHAR:
            CHECK_DUP();
            r = make_int_type(ICHAR);
            break;
        case KEYWORD_SHORT:
            CHECK_DUP();
            r = make_int_type(ISHORT);
            break;
        case KEYWORD_INT:
            CHECK_DUP();
            r = make_int_type(IINT);
            break;
        case KEYWORD_LONG:
            CHECK_DUP();
            r = make_int_type(ILONG);
            break;
        case KEYWORD_FLOAT:
            CHECK_DUP();
            if (sign != NONE)
                error_token(tok, "float cannot be signed nor unsigned");
            r = make_float_type(FFLOAT);
            break;
        case KEYWORD_DOUBLE:
            CHECK_DUP();
            if (sign != NONE)
                error_token(tok, "double cannot be signed nor unsigned");
            r = make_float_type(FDOUBLE);
            break;
#undef CHECK_DUP
        default:
            goto end;
        }
    }
 end:
    unget_token(ctx, tok);
    if (!r)
        r = make_int_type(IINT);
    if (sign == UNSIGNED)
        r = make_int_type(INT_TYPE(r)->kind + 1);
    return r;
}

static List *nread_compound_stmt(ReadContext *ctx) {
    return NULL;
}

/*
 * external-declaration:
 *     function-definition
 *     declaration
 */
static void nread_external_decl(ReadContext *ctx, Global *global) {
    Type *ctype = nread_decl_spec(ctx);
    Decl *decl = nread_declarator(ctx, ctype);
    if (decl->ctype->type == TFUNC) {
        if (next_token_is(ctx, '{')) {
            List *entry = nread_compound_stmt(ctx);
            make_nfunction(decl->name, decl->ctype, NULL, NULL, entry);
        }
    }
}

/*
 * translation-unit:
 *     external-declaration
 *     translation-unit external-declaration
 *
 * external-declaration:
 *     function-definition
 *     declaration
 */

static Global *make_global(void) {
    Global *r = malloc(sizeof(Global));
    r->var = make_list();
    r->func = make_list();
    return r;
}

static Global *nread_trans_unit(ReadContext *ctx) {
    Global *global = make_global();
    nread_external_decl(ctx, global);
    return global;
}

/*============================================================
 * Entry function
 */

List *parse(File *file, Elf *elf) {
    List *r = make_list();
    CppContext *cppctx = make_cpp_context(file);
    for (;;) {
        ReadContext *ctx = make_read_context(file, elf, cppctx);
        if (!peek_token(ctx))
            break;
        Function *f = read_func_declaration(ctx);
        check_context(ctx);
        list_push(r, f);
    }
    return r;
}
