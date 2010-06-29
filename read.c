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
 * The following book contains a BNF grammer for C.
 *
 *   C: A Reference Manual, Fifth Edition by Samuel P. Harbison and Guy
 *   L. Steele, published by Prentice Hall in February 2002, ISBN 0-13-089592X.
 *   http://www.amazon.com/dp/013089592X/
 */

#define NOT_SUPPORTED()                                                 \
    do { error("line %d: not supported yet", __LINE__); } while (0)

static void read_compound_stmt(ReadContext *ctx);
static void read_stmt(ReadContext *ctx);

static Token *make_token(int toktype, int lineno) {
    Token *r = malloc(sizeof(Token));
    r->toktype = toktype;
    r->lineno = lineno;
    return r;
}

ControlBlock *make_control_block() {
    ControlBlock *r = malloc(sizeof(ControlBlock));
    r->pos = -1;
    r->code = make_list();
    return r;
}

ReadContext *make_read_context(File *file, Elf *elf) {
    ReadContext *r = malloc(sizeof(ReadContext));
    r->file = file;
    r->elf = elf;
    r->scope = make_list();
    r->entry = make_control_block();
    r->blockstack = make_list();
    list_push(r->blockstack, r->entry);
    r->lasttok = NULL;
    return r;
}

static void push_scope(ReadContext *ctx) {
    list_push(ctx->scope, make_list());
}

static void pop_scope(ReadContext *ctx) {
    list_pop(ctx->scope);
}

static void push_control_block(ReadContext *ctx) {
    list_push(ctx->blockstack, make_control_block());
}

static ControlBlock *pop_control_block(ReadContext *ctx) {
    return list_pop(ctx->blockstack);
}

static void replace_control_block(ReadContext *ctx, ControlBlock *block) {
    LIST_BOTTOM(ctx->blockstack) = block;
}

static void emit(ReadContext *ctx, Inst *inst) {
    if (LIST_LEN(ctx->blockstack) == 0)
        error("[internal error] control block stack is empty");
    ControlBlock *block = LIST_BOTTOM(ctx->blockstack);
    list_push(block->code, inst);
}

static void add_local_var(ReadContext *ctx, String *name, Var *var) {
    List *current_scope = LIST_TOP(ctx->scope);
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

void skip_comment(File *file) {
    int prev = '\0';
    for (;;) {
        int c = readc(file);
        if (c == EOF)
            error("premature end of input file in comment");
        if (c == '/' && prev == '*')
            return;
        prev = c;
    }
}

static Token *read_num(File *file, char first, int lineno) {
    Token *tok;
    String *buf = make_string();
    o1(buf, first);
    for (;;) {
        int c = readc(file);
        if (c == EOF) {
            goto ret_int;
        } else if ('0' <= c && c <= '9') {
            o1(buf, c);
        } else if (c == '.') {
            o1(buf, c);
            break;
        } else {
            unreadc(c, file);
            goto ret_int;
        }
    }
    for (;;) {
        int c = readc(file);
        if (c == EOF) {
            goto ret_float;
        } else if ('0' <= c && c <= '9') {
            o1(buf, c);
        } else {
            unreadc(c, file);
            goto ret_float;
        }
    }
 ret_int:
    tok = make_token(TOKTYPE_INT, lineno);
    tok->val.i = atoi(STRING_BODY(buf));
    return tok;
 ret_float:
    tok = make_token(TOKTYPE_FLOAT, lineno);
    tok->val.f = atof(STRING_BODY(buf));
    return tok;
}

static int hextodec(char c) {
    c = tolower(c);
    if ('0' <= c && c <= '9')
        return c - '0';
    return c - 'a' + 10;
}

static char read_escape_char(File *file) {
    int c = readc(file);
    int r;
    switch (c) {
    case EOF:
        error("line %d: premature end of input file while reading a literal string or a character", file->lineno);
    case 'a': return '\a';
    case 'b': return '\b';
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
            error("line %d: premature end of input file while reading a literal string", file->lineno);
        default:
            o1(b, c);
        }
    }
}

static char read_char(File *file) {
    int c = readc(file);
    switch (c) {
    case EOF:
        error("line %d: premature end of input file while reading a literal character", file->lineno);
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

static void unget_token(ReadContext *ctx, Token *tok) {
    if (ctx->lasttok)
        error("[internal error] token buffer full");
    ctx->lasttok = tok;
}

Token *read_token(ReadContext *ctx) {
    Token *r;
    if (ctx->lasttok) {
        r = ctx->lasttok;
        ctx->lasttok = NULL;
        return r;
    }

    File *file = ctx->file;
    String *str;
    for (;;) {
        int c = readc(file);
        switch (c) {
        case ' ': case '\t': case '\r': case '\n':
            continue;
        case '0': case '1': case '2': case '3': case '4':
        case '5': case '6': case '7': case '8': case '9':
            return read_num(file, c, file->lineno);
        case '"':
            r = make_token(TOKTYPE_STRING, file->lineno);
            r->val.str = read_str(file);
            return r;
        case '\'': {
            r = make_token(TOKTYPE_CHAR, file->lineno);
            r->val.c = read_char(file);
            int c1 = read_char(file);
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
#define KEYWORD(type_, val_)                                    \
            if (!strcmp(STRING_BODY(str), (type_))) {           \
                r = make_token(TOKTYPE_KEYWORD, file->lineno);  \
                r->val.k = (val_);                              \
                return r;                                       \
            }
            KEYWORD("int",   KEYWORD_INT);
            KEYWORD("float", KEYWORD_FLOAT);
            KEYWORD("if",    KEYWORD_IF);
            KEYWORD("else",  KEYWORD_ELSE);
#undef KEYWORD
            r = make_token(TOKTYPE_IDENT, file->lineno);
            r->val.str = str;
            return r;
        case '!': case '%': case '&': case '(': case ')': case '*': case '+':
        case ',': case '-': case '/': case ';': case '=': case '[': case ']':
        case '^': case '{': case '|': case '}': case '~':
            r = make_token(TOKTYPE_KEYWORD, file->lineno);
            r->val.c = c;
            return r;
        case EOF:
            return NULL;
        default:
            error("line %d: unimplemented '%c'", file->lineno, c);
        }
    }
    return NULL;
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
    }
    return r;
}

static void expect(ReadContext *ctx, char expected) {
    Token *tok = read_token(ctx);
    if (tok->toktype != TOKTYPE_KEYWORD)
        error("line %d: keyword expected, but got %s", ctx->file->lineno, STRING_BODY(token_to_string(tok)));
    if (!IS_KEYWORD(tok, expected))
        error("line %d: '%c' expected, but got '%c'", ctx->file->lineno, expected, tok->val.k);
}

static Var *read_func_call(ReadContext *ctx, Token *fntok) {
    Section *data = find_section(ctx->elf, ".data");
    List *argtoks = make_list();
    for (;;) {
        Token *arg = read_token(ctx);
        list_push(argtoks, arg);
        Token *sep = read_token(ctx);
        if (sep->toktype != TOKTYPE_KEYWORD)
            error("line %d: expected ',', or ')', but got '%c'", sep->lineno, sep->val.c);
        if (sep->val.c == ')')
            break;
        if (sep->val.c != ',')
            error("line %d: expected ',', but got '%c'", sep->lineno, sep->val.c);
    }

    List *args = make_list();
    list_push(args, make_extern(fntok->val.str));
    Var *val = make_var(CTYPE_INT, NULL);
    list_push(args, val);
    int i;
    for (i = 0; i < LIST_LEN(argtoks); i++) {
        Token *arg = LIST_ELEM(argtoks, i);
        switch (arg->toktype) {
        case TOKTYPE_INT:
            list_push(args, make_imm(CTYPE_INT, (Cvalue)arg->val.i));
            break;
        case TOKTYPE_FLOAT:
            list_push(args, make_imm(CTYPE_FLOAT, (Cvalue)arg->val.f));
            break;
        case TOKTYPE_IDENT: {
            Var *var = find_var(ctx, arg->val.str);
            list_push(args, var);
            break;
        }
        case TOKTYPE_STRING: {
            int off = add_string(data, arg->val.str);
            Var *var = make_imm(CTYPE_PTR, (Cvalue)off);
            list_push(args, var);
            break;
        }
        case TOKTYPE_CHAR:
            NOT_SUPPORTED();
        default:
            error("unknown token type: %d", arg->toktype);
        }
    }
    emit(ctx, make_instn(OP_FUNC_CALL, args));
    return val;
}

static Token *read_ident(ReadContext *ctx) {
    Token *r = read_token(ctx);
    if (r->toktype != TOKTYPE_IDENT)
        error("identifier expected");
    return r;
}

static Var *read_unary_expr(ReadContext *ctx) {
    Token *tok = read_token(ctx);
    switch (tok->toktype) {
    case TOKTYPE_CHAR:
        return make_imm(CTYPE_CHAR, (Cvalue)tok->val.c);
    case TOKTYPE_INT:
        return make_imm(CTYPE_INT, (Cvalue)tok->val.i);
    case TOKTYPE_FLOAT:
        return make_imm(CTYPE_FLOAT, (Cvalue)tok->val.f);
    case TOKTYPE_STRING: {
        Section *data = find_section(ctx->elf, ".data");
        int off = add_string(data, tok->val.str);
        return make_imm(CTYPE_PTR, (Cvalue)off);
    }
    case TOKTYPE_KEYWORD:
        error("expected unary, but got '%s'", STRING_BODY(tok->val.str));
    case TOKTYPE_IDENT: {
        Token *tok1 = read_token(ctx);
        if (IS_KEYWORD(tok1, '('))
            return read_func_call(ctx, tok);
        unget_token(ctx, tok1);
        Var *var = find_var(ctx, tok->val.str);
        if (!var) {
            warn("'%s' is not defined", STRING_BODY(tok->val.str));
            var = make_var(CTYPE_INT, tok->val.str);
            add_local_var(ctx, tok->val.str, var);
        }
        var->is_lvalue = true;
        return var;
    }
    default:
        NOT_SUPPORTED();
    }
}

static Var *read_mul_expr(ReadContext *ctx) {
    Var *v0 = read_unary_expr(ctx);
    for (;;) {
        Token *tok = read_token(ctx);
        if (IS_KEYWORD(tok, '*') || IS_KEYWORD(tok, '/')) {
            Var *v1 = read_unary_expr(ctx);
            Var *r = make_var(CTYPE_INT, NULL);
            emit(ctx, make_inst3(tok->val.k, r, v0, v1));
            v0 = r;
            continue;
        }
        unget_token(ctx, tok);
        return v0;
    }
}

static Var *read_add_expr(ReadContext *ctx) {
    Var *v0 = read_mul_expr(ctx);
    for (;;) {
        Token *tok = read_token(ctx);
        if (IS_KEYWORD(tok, '+') || IS_KEYWORD(tok, '-')) {
            Var *v1 = read_mul_expr(ctx);
            Var *r = make_var(CTYPE_INT, NULL);
            emit(ctx, make_inst3(tok->val.k, r, v0, v1));
            v0 = r;
            continue;
        }
        unget_token(ctx, tok);
        return v0;
    }
}

static Var *read_expr(ReadContext *ctx) {
    return read_add_expr(ctx);
}

static void read_decl(ReadContext *ctx, Ctype *ctype) {
    Token *ident = read_ident(ctx);
    if (ident->toktype != TOKTYPE_IDENT)
        error("identifier expected");
    expect(ctx, '=');
    Var *val = read_expr(ctx);
    expect(ctx, ';');
    Var *var = make_var(CTYPE_INT, ident->val.str);
    add_local_var(ctx, ident->val.str, var);
    emit(ctx, make_inst2('=', var, val));
}

static Ctype *read_type(ReadContext *ctx) {
    Token *tok = read_token(ctx);
    if (IS_KEYWORD(tok, KEYWORD_INT)) {
        return make_ctype(CTYPE_INT);
    }
    unget_token(ctx, tok);
    return NULL;
}

static void ensure_lvalue(Var *var) {
    if (!var->is_lvalue)
        error("must be lvalue: '%s'", STRING_BODY(var->name));
}

static void read_if_stmt(ReadContext *ctx) {
    ControlBlock *then, *els = NULL;
    expect(ctx, '(');
    Var *cond = read_expr(ctx);
    expect(ctx, ')');

    ControlBlock *cont = make_control_block();
    push_control_block(ctx);
    expect(ctx, '{');
    read_compound_stmt(ctx);
    then = pop_control_block(ctx);

    Token *tok = read_token(ctx);
    if (IS_KEYWORD(tok, KEYWORD_ELSE)) {
        expect(ctx, '{');
        push_control_block(ctx);
        read_compound_stmt(ctx);
        els = pop_control_block(ctx);
    } else {
        unget_token(ctx, tok);
    }
    emit(ctx, make_inst4(OP_IF, cond, then, els, cont));
    replace_control_block(ctx, cont);
}

static Var *read_assignment_expr(ReadContext *ctx) {
    Var *var = read_unary_expr(ctx);
    Token *tok = read_token(ctx);
    if (IS_KEYWORD(tok, '=')) {
        ensure_lvalue(var);
        Var *val = read_expr(ctx);
        emit(ctx, make_inst2('=', var, val));
    } else if (IS_KEYWORD(tok, ';')) {
        return var;
    }
    NOT_SUPPORTED();
}

static void read_stmt(ReadContext *ctx) {
    Token *tok = read_token(ctx);
    if (IS_KEYWORD(tok, KEYWORD_IF)) {
        read_if_stmt(ctx);
        return;
    }
    unget_token(ctx, tok);
    read_assignment_expr(ctx);
}

static void read_stmt_or_decl(ReadContext *ctx) {
    Ctype *type = read_type(ctx);
    if (type == NULL) {
        read_stmt(ctx);
    } else {
        read_decl(ctx, type);
    }
}

static void read_compound_stmt(ReadContext *ctx) {
    push_scope(ctx);
    for (;;) {
        Token *tok = read_token(ctx);
        if (IS_KEYWORD(tok, '}'))
            break;
        unget_token(ctx, tok);
        read_stmt_or_decl(ctx);
    }
    pop_scope(ctx);
}

static void read_func_def(ReadContext *ctx) {
    Section *text = find_section(ctx->elf, ".text");
    Token *fname = read_token(ctx);
    if (fname->toktype != TOKTYPE_IDENT)
        error("line %d: identifier expected", fname->lineno);
    expect(ctx, '(');
    expect(ctx, ')');
    expect(ctx, '{');
    read_compound_stmt(ctx);
    Symbol *fsym = make_symbol(fname->val.str, text, 0, STB_GLOBAL, STT_NOTYPE, 1);
    dict_put(ctx->elf->syms, fname->val.str, fsym);
}

ControlBlock *parse(File *file, Elf *elf) {
    ReadContext *ctx = make_read_context(file, elf);
    read_func_def(ctx);
    return ctx->entry;
}
