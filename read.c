/*
 * read.c - C parser
 *
 *   Copyright 2010 Rui Ueyama <rui314@gmail.com>.  All rights reserved.
 *
 *   Redistribution and use in source and binary forms, with or without
 *   modification, are permitted provided that the following conditions are met:
 *
 *      1. Redistributions of source code must retain the above copyright
 *         notice, this list of conditions and the following disclaimer.
 *
 *      2. Redistributions in binary form must reproduce the above copyright
 *         notice, this list of conditions and the following disclaimer in the
 *         documentation and/or other materials provided with the distribution.
 *
 *   THIS SOFTWARE IS PROVIDED BY COPYRIGHT HOLDER ``AS IS'' AND ANY EXPRESS OR
 *   IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 *   OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN
 *   NO EVENT SHALL COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
 *   INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 *   (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 *   SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 *   CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 *   LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 *   OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
 *   DAMAGE.
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

#define NOT_SUPPORTED()                                                 \
    do { error("line %d: not supported yet", __LINE__); } while (0)

#define SWAP(type, x, y) do { type tmp_ = x; x = y; y = tmp_; } while (0)

static void emit_assign(ReadContext *ctx, Var *v0, Var *v1);
static Var *read_assign_expr(ReadContext *ctx);
static Var *read_comma_expr(ReadContext *ctx);
static Var *read_unary_expr(ReadContext *ctx);
static Var *read_func_call(ReadContext *ctx, Token *fntok);
static Var *read_expr1(ReadContext *ctx, Var *v0, int prec0);
static Token *read_ident(ReadContext *ctx);
static void read_compound_stmt(ReadContext *ctx);
static void read_decl_or_stmt(ReadContext *ctx);
static void read_stmt(ReadContext *ctx);

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
    return r;
}

static Ctype *make_ctype_array(Ctype *type, int size) {
    Ctype *r = malloc(sizeof(Ctype));
    r->type = CTYPE_ARRAY;
    r->ptr = type;
    r->size = size;
    return r;
}

static Var *make_var(Ctype *ctype) {
    Var *r = malloc(sizeof(Var));
    r->stype = VAR_LOCAL;
    r->ctype = ctype;
    r->val.l = 0;
    r->name = NULL;
    r->loc = NULL;
    return r;
}

static Var *make_deref_var(Var *var) {
    assert(var->ctype->ptr);
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

static Function *make_function(String *name, List *params, Block *entry) {
    Function *r = malloc(sizeof(Function));
    r->name = name;
    r->params = params;
    r->entry = entry;
    return r;
}

static void ensure_lvalue(Var *var) {
    if (var->loc || var->name)
        return;
    error("expected lvalue, but got %p", var);
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

/*============================================================
 * Basic block
 */
Block *make_block() {
    Block *r = malloc(sizeof(Block));
    r->pos = -1;
    r->code = make_list();
    return r;
}

ReadContext *make_read_context(File *file, Elf *elf) {
    ReadContext *r = malloc(sizeof(ReadContext));
    r->file = file;
    r->elf = elf;
    r->scope = make_list();
    r->entry = make_block();
    r->blockstack = make_list();
    list_push(r->blockstack, r->entry);
    r->ungotten = make_list();
    r->onbreak = NULL;
    r->oncontinue = NULL;
    r->label = make_string_dict();
    r->label_tbf = make_string_dict();
    return r;
}

static void push_scope(ReadContext *ctx) {
    list_push(ctx->scope, make_list());
}

static void pop_scope(ReadContext *ctx) {
    list_pop(ctx->scope);
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
    List *current_scope = LIST_BOTTOM(ctx->scope);
    list_push(current_scope, name);
    list_push(current_scope, var);
}

static Var *find_var(ReadContext *ctx, String *name) {
    for (int i = LIST_LEN(ctx->scope) - 1; i >= 0; i--) {
        List *scope = LIST_ELEM(ctx->scope, i);
        for (int j = 0; j < LIST_LEN(scope); j += 2) {
            String *str = LIST_ELEM(scope, j);
            if (string_equal(str, name))
                return LIST_ELEM(scope, j + 1);
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
Ctype *binary_type_conv(Var *v0, Var *v1) {
    if (is_flonum(v0->ctype) || is_flonum(v1->ctype))
        return make_ctype(CTYPE_FLOAT);
    return (ctype_sizeof(v0->ctype) < ctype_sizeof(v1->ctype)) ? v1->ctype : v0->ctype;
}

static Var *emit_arith(ReadContext *ctx, int op, Var *v0, Var *v1) {
    switch (op) {
    case '+': {
        // C:ARM p.229 7.6.2 Additive Operators.
        if (v1->ctype->ptr)
            SWAP(Var *, v0, v1);
        if (v0->ctype->type == CTYPE_PTR) {
            if (v1->ctype->type != CTYPE_INT)
                error("arithmetic + is not defined for pointers except integer operand");
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
            error("arithmetic - is not defined for pointer and type %d", v1->ctype->type);
        }
        Var *r = make_var(binary_type_conv(v0, v1));
        emit(ctx, make_inst3('-', r, v0, v1));
        return r;
    default:
        panic("unsupported operator: %c", op);
    }
}

static Var *emit_arith_inst3(ReadContext *ctx, int op, Var *v0, Var *v1) {
    Var *r = make_var(binary_type_conv(v0, v1));
    emit(ctx, make_inst3(op, r, v0, v1));
    return r;
}

static Var *emit_log_inst3(ReadContext *ctx, int op, Var *v0, Var *v1) {
    Var *r = make_var(make_ctype(CTYPE_INT));
    emit(ctx, make_inst3(op, r, v0, v1));
    return r;
}

static void emit_assign(ReadContext *ctx, Var *v0, Var *v1) {
    ensure_lvalue(v0);
    if (v0->loc) {
        emit(ctx, make_inst2(OP_ASSIGN_DEREF, v0->loc, unary_conv(ctx, v1)));
    } else {
        emit(ctx, make_inst2(OP_ASSIGN, v0, unary_conv(ctx, v1)));
    }
}

static Var *emit_post_inc_dec(ReadContext *ctx, Var *var, int op) {
    ensure_lvalue(var);
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

static Dict *reserved_word;

static Token *make_token(ReadContext *ctx) {
    Token *r = malloc(sizeof(Token));
    r->toktype = TOKTYPE_INVALID;
    r->line = ctx->file->line;
    r->column = ctx->file->column;
    return r;
}

static Token *make_token1(ReadContext *ctx, TokType toktype, TokenValue val) {
    Token *r = malloc(sizeof(Token));
    r->toktype = toktype;
    r->val = val;
    r->line = ctx->file->line;
    r->column = ctx->file->column;
    return r;
}

static Token *make_keyword(ReadContext *ctx, int k) {
    Token *r = malloc(sizeof(Token));
    r->toktype = TOKTYPE_KEYWORD;
    r->val.k = k;
    r->line = ctx->file->line;
    r->column = ctx->file->column;
    return r;
}

static void skip_comment(File *file) {
    int line = file->line;
    int column = file->column;
    int prev = '\0';
    for (;;) {
        int c = readc(file);
        if (c == EOF)
            error("Line %d:%d: premature end of input file in comment", line, column);
        if (c == '/' && prev == '*')
            return;
        prev = c;
    }
}

static void skip_line_comment(File *file) {
    for (;;) {
        int c = readc(file);
        if (c == EOF) return;
        if (c == '\n') {
            unreadc(c, file);
            return;
        }
    }
}

/*
 * integer-constant:
 *     decimal-constant integer-suffix?
 *     octal-constant integer-suffix?
 *     hexadecimal-constant integer-suffix?
 *
 * decimal-constant:
 *     [1-9] [0-9]*
 *
 * octal-constant:
 *     "0" [0-7]*
 *
 * hexadecimal-constant
 *     "0" [xX] [0-9a-fA-F]+
 *
 * integer-suffix:
 *     long-suffix unsigned-suffix?
 *     long-long-suffix unsigned-suffix?
 *     unsigned-suffix long-suffix?
 *     unsigned-suffix long--longsuffix?
 *
 * long-suffix;
 *     "l"
 *     "L"
 *
 * long-long-suffix:
 *     "ll"
 *     "LL"
 *
 * unsigned-suffix:
 *     "u"
 *     "U"
 *
 * floating-constant:
 *     decimal-floating-constant
 *     hexadecimal-floating-constant
 *
 * decimal-floating-constant:
 *     digit-sequence exponent? floating-suffix?
 *
 * hexadecimal-floating-constant:
 *     hex-prefix dotted-hex-digits binary-exponent floating-suffix?
 *     hex-prefix hex-digit-sequence binary-exponent floating-suffix?
 *
 * hex-prefix:
 *     0x
 *     0X
 *
 * dotted-hex-digits:
 *     hex-digit-sequence "."
 *     hex-digit-sequence "." hex-digit-sequence
 *     "." hex-digit-sequence
 *
 * exponent:
 *     [eE] [-+]? digit-sequence
 *
 * binary-exponent:
 *     [pP] [-+]? digit-sequence
 *
 * floating-suffix:
 *     [fFlL]
 *
 * digit-sequence:
 *     [0-9]+
 *
 * hex-digit-sequence:
 *     [0-9a-fA-F]+
 */
static Token *read_num(ReadContext *ctx) {
    Token *tok = make_token(ctx);
    String *buf = make_string();
    for (;;) {
        int c = readc(ctx->file);
        if (c == EOF) {
            goto ret_int;
        } else if ('0' <= c && c <= '9') {
            o1(buf, c);
        } else if (c == '.') {
            o1(buf, c);
            break;
        } else {
            unreadc(c, ctx->file);
            goto ret_int;
        }
    }
    for (;;) {
        int c = readc(ctx->file);
        if (c == EOF) {
            goto ret_float;
        } else if ('0' <= c && c <= '9') {
            o1(buf, c);
        } else {
            unreadc(c, ctx->file);
            goto ret_float;
        }
    }
 ret_int:
    tok->toktype = TOKTYPE_INT;
    tok->val.i = atoi(STRING_BODY(buf));
    return tok;
 ret_float:
    tok->toktype = TOKTYPE_FLOAT;
    tok->val.f = atof(STRING_BODY(buf));
    return tok;
}

static int hextodec(char c) {
    c = tolower(c);
    if ('0' <= c && c <= '9')
        return c - '0';
    return c - 'a' + 10;
}

/*
 * escape-character:
 *     "\" escape-code
 *     universal-character-name
 *
 * escape-code:
 *     character-escape-code
 *     octal-escape-code
 *     hex-escape-code
 *
 * character-escape-code:
 *     one of: n t b r f v \ ' " a ? e
 *     ('\e' is GNU extension)
 *
 * octal-escape-code:
 *     [0-7]{,3}
 *
 * hex-escape-code:
 *     "x" [0-9a-fA-F]+
 *
 * universal-character-name:
 *     "\u" [0-9a-fA-F]{4}
 *     "\U" [0-9a-fA-F]{8}
 */
static char read_escape_char(File *file) {
    int c = readc(file);
    int r;
    switch (c) {
    case EOF:
        error("line %d:%d: premature end of input file while reading a literal string or a character", file->line, file->column);
    case 'a': return '\a';
    case 'b': return '\b';
    case 'e': return '\033'; // GNU extension
    case 't': return '\t';
    case 'n': return '\n';
    case 'v': return '\v';
    case 'f': return '\f';
    case 'r': return '\r';
    case '0': case '1': case '2': case '3': case '4':
    case '5': case '6': case '7': case '8': case '9':
        r = c - '0';
        c = readc(file);
        if (isdigit(c)) {
            r = r * 8 + (c - '0');
            c = readc(file);
            if (isdigit(c)) {
                r = r * 8 + (c - '0');
            } else {
                unreadc(c, file);
            }
        } else {
            unreadc(c, file);
        }
        return r;
    case 'x':
        c = readc(file);
        if (!isxdigit(c))
            error("hexdigit expected, but got '%c'", c);
        r = hextodec(c);
        c = readc(file);
        if (isxdigit(c)) {
            r = r * 16 + hextodec(c);
        } else {
            unreadc(c, file);
        }
        return r;
    default: return (char)c;
    }
}

/*
 * string-constant:
 *     '"' s-char* '"'
 *     'L"' s-char* '"'
 *
 * s-char:
 *    any source character except the double quote, backslash or newline
 *    escape-character
 */
static String *read_str(File *file) {
    String *b = make_string();
    for (;;) {
        int c = readc(file);
        switch (c) {
        case '"':
            o1(b, '\0');
            return b;
        case '\\':
            o1(b, read_escape_char(file));
            break;
        case EOF:
            error("line %d:%d: premature end of input file while reading a literal string", file->line, file->column);
        default:
            o1(b, c);
        }
    }
}

/*
 * character-constant:
 *     "'" c-char* "'"
 *     "L'" c-char* "'"
 *
 * c-char:
 *    any source character except the single quote, backslash or newline
 *    escape-character
 */
static char read_char(File *file) {
    int c = readc(file);
    switch (c) {
    case EOF:
        error("line %d:%d: premature end of input file while reading a literal character", file->line, file->column);
    case '\\': return read_escape_char(file);
    default: return (char)c;
    }
}

static String *read_word(File *file, char c0) {
    String *b = make_string();
    o1(b, c0);
    for (;;) {
        int c = readc(file);
        if (isalnum(c) || c == '_') {
            o1(b, c);
        } else if (c != EOF) {
            unreadc(c, file);
            return b;
        } else {
            return b;
        }
    }
}

void unget_token(ReadContext *ctx, Token *tok) {
    list_push(ctx->ungotten, tok);
}

Token *read_token(ReadContext *ctx) {
    if (!LIST_IS_EMPTY(ctx->ungotten))
        return list_pop(ctx->ungotten);

    File *file = ctx->file;
    String *str;
    for (;;) {
        int c = readc(file);
        int c1;
        switch (c) {
        case ' ': case '\t': case '\r': case '\n':
            continue;
        case '0': case '1': case '2': case '3': case '4':
        case '5': case '6': case '7': case '8': case '9':
            unreadc(c, file);
            return read_num(ctx);
        case '"':
            return make_token1(ctx, TOKTYPE_STRING, (TokenValue)read_str(file));
        case '\'': {
            Token *r = make_token1(ctx, TOKTYPE_CHAR, (TokenValue)read_char(file));
            c1 = read_char(file);
            if (c1 != '\'')
                error("single quote expected, but got %c", c1);
            return r;
        }
        case 'a': case 'b': case 'c': case 'd': case 'e': case 'f': case 'g':
        case 'h': case 'i': case 'j': case 'k': case 'l': case 'm': case 'n':
        case 'o': case 'p': case 'q': case 'r': case 's': case 't': case 'u':
        case 'v': case 'w': case 'x': case 'y': case 'z': case 'A': case 'B':
        case 'C': case 'D': case 'E': case 'F': case 'G': case 'H': case 'I':
        case 'J': case 'K': case 'L': case 'M': case 'N': case 'O': case 'P':
        case 'Q': case 'R': case 'S': case 'T': case 'U': case 'V': case 'W':
        case 'X': case 'Y': case 'Z': case '_':
            str = read_word(file, c);
            int id = (intptr)dict_get(reserved_word, str);
            if (id)
                return make_keyword(ctx, id);
            return make_token1(ctx, TOKTYPE_IDENT, (TokenValue)str);
        case '=':
            if (next_char_is(ctx->file, '='))
                return make_keyword(ctx, KEYWORD_EQ);
            return make_keyword(ctx, '=');
        case '/':
            if (next_char_is(ctx->file, '*')) {
                skip_comment(file);
                return read_token(ctx);
            }
            if (next_char_is(ctx->file, '/')) {
                skip_line_comment(file);
                return read_token(ctx);
            }
            goto read_equal;
        case '<':
            if (next_char_is(ctx->file, '<'))
                c = KEYWORD_LSH;
            goto read_equal;
        case '>':
            if (next_char_is(ctx->file, '>'))
                c = KEYWORD_RSH;
            goto read_equal;
        case '+':
            if (next_char_is(ctx->file, '+'))
                return make_keyword(ctx, KEYWORD_INC);
            goto read_equal;
        case '-':
            if (next_char_is(ctx->file, '-'))
                return make_keyword(ctx, KEYWORD_DEC);
            goto read_equal;
        case '&':
            if (next_char_is(ctx->file, '&'))
                return make_keyword(ctx, KEYWORD_LOG_AND);
            goto read_equal;
        case '|':
            if (next_char_is(ctx->file, '|'))
                return make_keyword(ctx, KEYWORD_LOG_OR);
            goto read_equal;
        case '*': case '%': case '~': case '^': case '!':
        read_equal:
            if (next_char_is(ctx->file, '=')) {
                int k = c == '+' ? KEYWORD_A_ADD
                    : c == '-' ? KEYWORD_A_SUB
                    : c == '*' ? KEYWORD_A_MUL
                    : c == '/' ? KEYWORD_A_DIV
                    : c == '%' ? KEYWORD_A_MOD
                    : c == '&' ? KEYWORD_A_AND
                    : c == '|' ? KEYWORD_A_OR
                    : c == '^' ? KEYWORD_A_XOR
                    : c == '<' ? KEYWORD_LE
                    : c == '>' ? KEYWORD_GE
                    : c == '!' ? KEYWORD_NE
                    : c == KEYWORD_LSH ? KEYWORD_A_LSH
                    : c == KEYWORD_RSH ? KEYWORD_A_RSH
                    : panic("unknown op: %c", c);
                return make_keyword(ctx, k);
            }
            // FALL THROUGH
        case '(': case ')': case ',': case ';': case '[': case ']':
        case '{': case '}': case ':': case '?':
            return make_keyword(ctx, c);
        case EOF:
            return NULL;
        default:
            error("line %d:%d: unimplemented '%c'", file->line, file->column, c);
        }
    }
    return NULL;
}

Token *peek_token(ReadContext *ctx) {
    Token *r = read_token(ctx);
    unget_token(ctx, r);
    return r;
}


bool next_token_is(ReadContext *ctx, int keyword) {
    Token *tok = read_token(ctx);
    if (IS_KEYWORD(tok, keyword))
        return true;
    unget_token(ctx, tok);
    return false;
}

String *token_to_string(Token *tok) {
    char buf[20];
    String *r = make_string();
    switch (tok->toktype) {
    case TOKTYPE_KEYWORD:
        o1(r, tok->val.k);
        o1(r, '\0');
        break;
    case TOKTYPE_CHAR:
        sprintf(buf, "'%c'", tok->val.c);
        ostr(r, buf);
        break;
    case TOKTYPE_IDENT:
        ostr(r, STRING_BODY(tok->val.str));
        break;
    case TOKTYPE_STRING:
        ostr(r, STRING_BODY(tok->val.str));
        break;
    case TOKTYPE_INT:
        sprintf(buf, "%d", tok->val.i);
        ostr(r, buf);
        break;
    case TOKTYPE_FLOAT:
        sprintf(buf, "%f", tok->val.f);
        ostr(r, buf);
        break;
    case TOKTYPE_INVALID:
        panic("got TOKTYPE_INVALID");
    }
    return r;
}

static void expect(ReadContext *ctx, int expected) {
    Token *tok = read_token(ctx);
    if (tok->toktype != TOKTYPE_KEYWORD)
        error("line %d:%d: keyword expected, but got %s", ctx->file->line, ctx->file->column, STRING_BODY(token_to_string(tok)));
    if (!IS_KEYWORD(tok, expected))
        error("line %d:%d: '%c' expected, but got '%c'", ctx->file->line, ctx->file->column, expected, tok->val.k);
}

static void process_break(ReadContext *ctx) {
    if (!ctx->onbreak)
        error("'break' statement not in loop or switch");
    emit(ctx, make_inst1(OP_JMP, ctx->onbreak));
}

static void process_continue(ReadContext *ctx) {
    if (!ctx->oncontinue)
        error("'continue' statement not in loop statement");
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
Var *read_primary_expr(ReadContext *ctx) {
    Token *tok = read_token(ctx);
    switch (tok->toktype) {
    case TOKTYPE_CHAR:
        return make_imm(CTYPE_CHAR, (Cvalue)tok->val.c);
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
        if (IS_KEYWORD(tok1, '('))
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
        NOT_SUPPORTED();
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
Var *read_postfix_expr(ReadContext *ctx) {
    return read_primary_expr(ctx);
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
    if (!IS_KEYWORD(tok, ')')) {
        unget_token(ctx, tok);
        for (;;) {
            Var *v = read_assign_expr(ctx);
            list_push(args, unary_conv(ctx, v));
            Token *sep = read_token(ctx);
            if (sep->toktype != TOKTYPE_KEYWORD)
                error("line %d:%d: expected ',', or ')', but got '%c'", sep->line, sep->column, sep->val.c);
            if (IS_KEYWORD(sep, ')'))
                break;
            if (!IS_KEYWORD(sep, ','))
                error("line %d:%d: expected ',', but got '%c'", sep->line, sep->column, sep->val.c);
        }
    }
    emit(ctx, make_instn(OP_FUNC_CALL, args));
    return retval;
}

static void expect_ident(Token *tok) {
    if (tok->toktype != TOKTYPE_IDENT)
        error("line %d:%d: identifier expected", tok->line, tok->column);
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
    switch (tok->val.k) {
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
        emit(ctx, make_inst3(tok->val.k, r, zero, tmp));
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
        emit(ctx, make_inst2(tok->val.k, r, tmp));
        return r;
    }
    case KEYWORD_INC:
    case KEYWORD_DEC: {
        Var *v = read_cast_expr(ctx);
        ensure_lvalue(v);
        char op = tok->val.k == KEYWORD_INC ? '+' : '-';
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
        error("expected unary, but got %d", tok->val.k);
    }
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
static int prec(Token *tok) {
    if (tok->toktype != TOKTYPE_KEYWORD)
        return -1;
    switch (tok->val.k) {
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


/* Returns true iff a given operator is right-associative. */
static bool is_rassoc(Token *tok) {
    switch (tok->val.k) {
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
 */
static Var *read_comma_expr(ReadContext *ctx) {
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
        int prec1 = prec(tok);
        if (prec1 < 0 || prec0 < prec1) {
            unget_token(ctx, tok);
            return v0;
        }
        if (IS_KEYWORD(tok, '[')) {
            v0 = read_subscript_expr(ctx, v0);
            continue;
        }
        if (IS_KEYWORD(tok, KEYWORD_INC)) {
            v0 = emit_post_inc(ctx, v0);
            continue;
        }
        if (IS_KEYWORD(tok, KEYWORD_DEC)) {
            v0 = emit_post_dec(ctx, v0);
            continue;
        }
        if (IS_KEYWORD(tok, '?')) {
            v0 = read_cond_expr(ctx, v0);
            v0 = unary_conv(ctx, v0);
            continue;
        }
        if (IS_KEYWORD(tok, KEYWORD_LOG_AND) || IS_KEYWORD(tok, KEYWORD_LOG_OR)) {
            push_block(ctx, make_block());
        }
        Var *v1 = read_unary_expr(ctx);
        for (;;) {
            Token *tok1 = peek_token(ctx);
            int prec2 = prec(tok1);
            if (prec2 < 0 || prec1 < prec2 || (prec1 == prec2 && !is_rassoc(tok1))) {
                break;
            }
            v1 = read_expr1(ctx, v1, prec2);
        }
        if (IS_KEYWORD(tok, '=')) {
            emit_assign(ctx, v0, v1);
            continue;
        }

        v0 = unary_conv(ctx, v0);
        v1 = unary_conv(ctx, v1);
        int op;
        switch (tok->val.k) {
        case ',':
            v0 = v1;
            break;
        case '+': case '-':
            v0 = emit_arith(ctx, tok->val.k, v0, v1);
            break;
        case '*':
        case '/': {
            v0 = emit_arith_inst3(ctx, tok->val.k, v0, v1);
            break;
        }
        case '^':
        case '&':
        case '|': {
            Var *tmp = make_var(binary_type_conv(v0, v1));
            if (is_flonum(tmp->ctype))
                error("invalid operand to binary '%c'", tok->val.k);
            emit(ctx, make_inst3(tok->val.k, tmp, v0, v1));
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
            ensure_lvalue(v0);
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
            ensure_lvalue(v0);
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
            error("unsupported operator: %c", tok->val.k);
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
 *     one of: auto extern register static typedef
 *
 * type-specifier:
 *     enumeration-type-specifier
 *     floating-point-type-specifier
 *     integer-type-type-specifier
 *     structure-type-type-specifier
 *     typedef-name
 *     union-type-specifier
 *     void-type-specifier
 *
 * type-qualifier:
 *     one of: const volatile restrict
 *
 * function-specifier:
 *     "inline"
 */
Ctype *read_declaration_spec(ReadContext *ctx) {
    Ctype *r = NULL;
    Token *tok;
    enum { NONE, SIGNED, UNSIGNED } sign = NONE;
    for (;;) {
        tok = read_token(ctx);
        if (tok->toktype != TOKTYPE_KEYWORD)
            goto end;
        switch (tok->val.k) {
        case KEYWORD_CONST:
            // ignore the type specifier for now.
            break;
        case KEYWORD_SIGNED:
            if (sign == SIGNED)
                error("Line %d:%d: 'signed' specified twice", tok->line, tok->column);
            if (sign == UNSIGNED)
                goto sign_error;
            sign = UNSIGNED;
            break;
        case KEYWORD_UNSIGNED:
            if (sign == UNSIGNED)
                error("Line %d:%d: 'unsigned' specified twice", tok->line, tok->column);
            if (sign == SIGNED)
                goto sign_error;
            sign = UNSIGNED;
            break;
#define CHECK_DUP()                                                     \
            if (r) error("Line %d:%d: two or more data types in declaration specifiers", tok->line, tok->column);
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
                error("Line %d:%d: float cannot be signed nor unsigned", tok->line, tok->column);
            r = make_ctype(CTYPE_FLOAT);
            break;
#undef CHECK_DUP
        default:
            goto end;
        }
    }
 end:
    r->signedp = (sign != UNSIGNED);
    unget_token(ctx, tok);
    return r ? r : make_ctype(CTYPE_INT);
 sign_error:
    error("Line %d:%d: both 'signed' and 'unsigned' in declaration specifiers", tok->line, tok->column);
}

Ctype *read_array_dimensions(ReadContext *ctx, Ctype *ctype) {
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
            error("Line %d:%d: Dimension is not specified for a multimentional array", ctx->file->line, ctx->file->column);
        return make_ctype_array(ctype1, size);
    }
    return make_ctype_array(ctype, size);
}

/*
 * direct-declarator:
 *     simple-declarator
 *     "(" declarator ")"
 *     function-declarator
 *     array-declarator
 *
 * array-declarator:
 *     direct-declarator "[" constant-expression? "]"
 *     direct-declarator "[" array-qualifier* array-size-expression? "]"
 *     direct-declarator "[" array-qualifier* "*" "]"
 *
 * array-qualifier:
 *     one of: static restrict const volatile
 *
 * simple-declarator:
 *     identifier
 */
Var *read_direct_declarator(ReadContext *ctx, Ctype *ctype) {
    Var *r = make_var(ctype);
    Token *tok = read_ident(ctx);
    r->name = tok->val.str;
    if (next_token_is(ctx, '['))
        r->ctype = read_array_dimensions(ctx, r->ctype);
    return r;
}

/*
 * pointer-declarator:
 *     pointer direct-declarator
 *
 * pointer:
 *     "*" type-qualifier* pointer?
 */
Var *read_pointer_declarator(ReadContext *ctx, Ctype *ctype) {
    Var *r = next_token_is(ctx, '*')
        ? read_pointer_declarator(ctx, ctype)
        : read_direct_declarator(ctx, ctype);
    r->ctype = make_ctype_ptr(r->ctype);
    return r;
}

/*
 * declarator:
 *     pointer-declarator
 *     direct-declarator
 */
Var *read_declarator(ReadContext *ctx, Ctype *ctype) {
    if (next_token_is(ctx, '*')) {
        return read_pointer_declarator(ctx, ctype);
    }
    return read_direct_declarator(ctx, ctype);
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
Var *read_initializer(ReadContext *ctx) {
    return read_assign_expr(ctx);
}

/*
 * initialized-declarator:
 *     declarator
 *     declarator "=" initializer
 */
void read_initialized_declarator(ReadContext *ctx, Ctype *ctype) {
    Var *var = read_declarator(ctx, ctype);
    add_local_var(ctx, var->name, var);
    if (next_token_is(ctx, '=')) {
        Var *val = unary_conv(ctx, read_initializer(ctx));
        emit(ctx, make_inst2(OP_ASSIGN, var, val));
        return;
    }
    emit(ctx, make_inst1(OP_ALLOC, var));
}

static bool is_type_keyword(Token *tok) {
    if (tok->toktype != TOKTYPE_KEYWORD)
        return false;
    switch (tok->val.k) {
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
        error("duplicate label: %s", STRING_BODY(label));

    Block *cont = make_block();
    emit(ctx, make_inst1(OP_JMP, cont));
    replace_block(ctx, cont);

    dict_put(ctx->label, label, cont);

    List *tbf = dict_get(ctx->label_tbf, label);
    if (!tbf)
        return;
    for (int i = 0; i < LIST_LEN(tbf); i++) {
        Block *block = LIST_ELEM(tbf, i);
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
}

/*
 * return-statement:
 *     "return" expression? ";"
 */
static void read_return_stmt(ReadContext *ctx) {
    Token *tok = read_token(ctx);
    Var *retval;
    if (IS_KEYWORD(tok, ';')) {
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
        String *label = p[0];
        error("dangling goto label: '%s'", STRING_BODY(label));
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
    if (IS_KEYWORD(tok, KEYWORD_IF)) {
        read_if_stmt(ctx);
    } else if (IS_KEYWORD(tok, KEYWORD_FOR)) {
        read_for_stmt(ctx);
    } else if (IS_KEYWORD(tok, KEYWORD_WHILE)) {
        read_while_stmt(ctx);
    } else if (IS_KEYWORD(tok, KEYWORD_DO)) {
        read_do_stmt(ctx);
    } else if (IS_KEYWORD(tok, KEYWORD_BREAK)) {
        expect(ctx, ';');
        process_break(ctx);
    } else if (IS_KEYWORD(tok, KEYWORD_CONTINUE)) {
        expect(ctx, ';');
        process_continue(ctx);
    } else if (IS_KEYWORD(tok, KEYWORD_GOTO)) {
        read_goto_stmt(ctx);
    } else if (IS_KEYWORD(tok, KEYWORD_RETURN)) {
        read_return_stmt(ctx);
    } else if (IS_KEYWORD(tok, '{')) {
        read_compound_stmt(ctx);
    } else {
        Token *tok1 = read_token(ctx);
        if (IS_KEYWORD(tok1, ':')) {
            process_label(ctx, tok);
        } else {
            unget_token(ctx, tok1);
            unget_token(ctx, tok);
        }
        read_comma_expr(ctx);
        Token *tok2 = read_token(ctx);
        if (!IS_KEYWORD(tok2, ';'))
            error("';' expected");
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
        if (IS_KEYWORD(tok, '}'))
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
    Token *fname = read_ident(ctx);
    expect(ctx, '(');
    push_scope(ctx);
    List *params = read_param_type_list(ctx);
    expect(ctx, '{');
    read_compound_stmt(ctx);
    pop_scope(ctx);

    Block *epilogue = make_block();
    emit(ctx, make_inst1(OP_JMP, epilogue));
    push_block(ctx, epilogue);
    emit(ctx, make_inst1(OP_RETURN, make_imm(CTYPE_INT, (Cvalue)0)));
    pop_block(ctx);

    return make_function(fname->val.str, params, ctx->entry);
}

/*============================================================
 * Initializer
 */

void parser_init() {
    if (reserved_word != NULL)
        return;
    reserved_word = make_string_dict();
#define KEYWORD(id_, str_) \
    dict_put(reserved_word, to_string(str_), (void *)id_);
#define OP(_)
# include "keyword.h"
#undef OP
#undef KEYWORD
}

/*============================================================
 * Entry function
 */

List *parse(File *file, Elf *elf) {
    parser_init();
    List *r = make_list();
    for (;;) {
        ReadContext *ctx = make_read_context(file, elf);
        if (!peek_token(ctx))
            break;
        Function *f = read_func_declaration(ctx);
        check_context(ctx);
        list_push(r, f);
    }
    return r;
}
