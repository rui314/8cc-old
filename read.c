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

static Var *read_assign_expr(ReadContext *ctx);
static Var *read_comma_expr(ReadContext *ctx);
static void read_compound_stmt(ReadContext *ctx);
static void read_stmt(ReadContext *ctx);

static Token *make_token(ReadContext *ctx) {
    Token *r = malloc(sizeof(Token));
    r->toktype = TOKTYPE_INVALID;
    r->line = ctx->file->line;
    r->column = ctx->file->column;
    return r;
}

Block *make_block() {
    Block *r = malloc(sizeof(Block));
    r->pos = -1;
    r->code = make_list();
    r->name = NULL;
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

static Block *push_block(ReadContext *ctx) {
    Block *r = make_block();
    list_push(ctx->blockstack, r);
    return r;
}

static void push_block1(ReadContext *ctx, Block *block) {
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
        error("[internal error] control block stack is empty");
    Block *block = LIST_BOTTOM(ctx->blockstack);
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

static char read_escape_char(File *file) {
    int c = readc(file);
    int r;
    switch (c) {
    case EOF:
        error("line %d:%d: premature end of input file while reading a literal string or a character", file->line, file->column);
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
            error("line %d:%d: premature end of input file while reading a literal string", file->line, file->column);
        default:
            o1(b, c);
        }
    }
}

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
    Token *r = make_token(ctx);
    if (!LIST_IS_EMPTY(ctx->ungotten))
        return list_pop(ctx->ungotten);

    File *file = ctx->file;
    String *str;
    for (;;) {
        int c = readc(file);
        switch (c) {
        case ' ': case '\t': case '\r': case '\n':
            continue;
        case '0': case '1': case '2': case '3': case '4':
        case '5': case '6': case '7': case '8': case '9':
            unreadc(c, file);
            return read_num(ctx);
        case '"':
            r->toktype = TOKTYPE_STRING;
            r->val.str = read_str(file);
            return r;
        case '\'': {
            r->toktype = TOKTYPE_CHAR;
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
                r->toktype = TOKTYPE_KEYWORD;                   \
                r->val.k = (val_);                              \
                return r;                                       \
            }
            KEYWORD("int",   KEYWORD_INT);
            KEYWORD("float", KEYWORD_FLOAT);
            KEYWORD("if",    KEYWORD_IF);
            KEYWORD("else",  KEYWORD_ELSE);
            KEYWORD("for",   KEYWORD_FOR);
            KEYWORD("while", KEYWORD_WHILE);
            KEYWORD("do",    KEYWORD_DO);
            KEYWORD("break", KEYWORD_BREAK);
            KEYWORD("continue", KEYWORD_CONTINUE);
            KEYWORD("goto",  KEYWORD_GOTO);
            KEYWORD("return", KEYWORD_RETURN);
#undef KEYWORD
            r->toktype = TOKTYPE_IDENT;
            r->val.str = str;
            return r;
        case '=': {
            int c1 = readc(file);
            if (c1 == '=') {
                r->toktype = TOKTYPE_KEYWORD;
                r->val.k = KEYWORD_EQUAL;
                return r;
            } else {
                unreadc(c1, file);
            }
            // FALL THROUGH
        }
        case '!': case '%': case '&': case '(': case ')': case '*': case '+':
        case ',': case '-': case '/': case ';': case '[': case ']': case '^':
        case '{': case '|': case '}': case '~': case ':':
            r->toktype = TOKTYPE_KEYWORD;
            r->val.c = c;
            return r;
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
        error("[internal error] got TOKTYPE_INVALID");
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

static Var *read_func_call(ReadContext *ctx, Token *fntok) {
    List *args = make_list();
    list_push(args, make_extern(fntok->val.str));
    Var *retval = make_var(CTYPE_INT, NULL);
    list_push(args, retval);
    Token *tok = read_token(ctx);
    if (!IS_KEYWORD(tok, ')')) {
        unget_token(ctx, tok);
        for (;;) {
            Var *v = read_assign_expr(ctx);
            list_push(args, v);
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
    case TOKTYPE_KEYWORD: {
        if (IS_KEYWORD(tok, '(')) {
            Var *r = read_comma_expr(ctx);
            expect(ctx, ')');
            return r;
        }
        error("expected unary, but got '%s'", STRING_BODY(tok->val.str));
    }
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

static void ensure_lvalue(Var *var) {
    if (!var->is_lvalue)
        error("must be lvalue: '%s'", STRING_BODY(var->name));
}

static int prec(Token *tok) {
    if (tok->toktype != TOKTYPE_KEYWORD)
        return -1;
    int r = 0;
    switch (tok->val.k) {
    case '*': case '/': r++;
    case '+': case '-': r++;
    case KEYWORD_EQUAL: r++;
    case '=': r++;
    case ',': return r;
    default: return -1;
    }
}

/* Returns true iff a given operator is right-associative. */
static bool is_rassoc(Token *tok) {
    return tok->val.k == '=';
}

static Var *read_expr1(ReadContext *ctx, Var *v0, int prec0) {
    for (;;) {
        Token *tok = read_token(ctx);
        int prec1 = prec(tok);
        if (prec1 < 0 || prec1 < prec0) {
            unget_token(ctx, tok);
            return v0;
        }
        Var *v1 = read_unary_expr(ctx);
        for (;;) {
            Token *tok1 = peek_token(ctx);
            int prec2 = prec(tok1);
            if (prec2 < 0 || prec2 < prec1 || (prec1 == prec2 && !is_rassoc(tok1))) {
                break;
            }
            v1 = read_expr1(ctx, v1, prec2);
        }
        Var *tmp;
        switch (tok->val.k) {
        case ',':
            v0 = v1;
            break;
        case '=':
            ensure_lvalue(v0);
            emit(ctx, make_inst2(tok->val.k, v0, v1));
            break;
        case '+': case '-': case '*': case '/':
            tmp = make_var(CTYPE_INT, NULL);
            emit(ctx, make_inst3(tok->val.k, tmp, v0, v1));
            v0 = tmp;
            break;
        case KEYWORD_EQUAL:
            tmp = make_var(CTYPE_INT, NULL);
            emit(ctx, make_inst3(OP_EQUAL, tmp, v0, v1));
            v0 = tmp;
            break;
        default:
            error("unsupported operator: %c", tok->val.k);
        }
    }
    return v0;
}

static Var *read_assign_expr(ReadContext *ctx) {
    return read_expr1(ctx, read_unary_expr(ctx), 1);
}

static Var *read_comma_expr(ReadContext *ctx) {
    return read_expr1(ctx, read_unary_expr(ctx), 0);
}

static void read_decl(ReadContext *ctx, Ctype *ctype) {
    Token *ident = read_ident(ctx);
    expect(ctx, '=');
    Var *val = read_assign_expr(ctx);
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

static void read_if_stmt(ReadContext *ctx) {
    Block *then, *els;
    expect(ctx, '(');
    Var *cond = read_comma_expr(ctx);
    expect(ctx, ')');

    push_block(ctx);
    read_stmt(ctx);
    then = pop_block(ctx);

    Token *tok = read_token(ctx);
    if (IS_KEYWORD(tok, KEYWORD_ELSE)) {
        push_block(ctx);
        read_stmt(ctx);
        els = pop_block(ctx);
    } else {
        unget_token(ctx, tok);
        els = NULL;
    }
    Block *cont = make_block();
    emit(ctx, make_inst4(OP_IF, cond, then, els, cont));
    replace_block(ctx, cont);
}

static void read_for_stmt(ReadContext *ctx) {
    Block *cond = make_block();
    Block *mod = make_block();
    Block *body = make_block();
    Block *cont = make_block();

    expect(ctx, '(');
    read_comma_expr(ctx);
    expect(ctx, ';');

    emit(ctx, make_inst1(OP_JMP, cond));

    push_block1(ctx, cond);
    Var *condvar = read_comma_expr(ctx);
    emit(ctx, make_inst4(OP_IF, condvar, body, NULL, cont));
    pop_block(ctx);
    expect(ctx, ';');

    push_block1(ctx, mod);
    read_comma_expr(ctx);
    emit(ctx, make_inst1(OP_JMP, cond));
    pop_block(ctx);
    expect(ctx, ')');

    Block *orig_onbreak = ctx->onbreak;
    Block *orig_oncontinue = ctx->oncontinue;
    ctx->onbreak = cont;
    ctx->oncontinue = mod;
    push_block1(ctx, body);
    read_stmt(ctx);
    emit(ctx, make_inst1(OP_JMP, mod));
    pop_block(ctx);
    ctx->oncontinue = orig_oncontinue;
    ctx->onbreak = orig_onbreak;

    replace_block(ctx, cont);
}

static void read_while_stmt(ReadContext *ctx) {
    Block *cond = make_block();
    Block *body = make_block();
    Block *cont = make_block();

    emit(ctx, make_inst1(OP_JMP, cond));

    expect(ctx, '(');
    push_block1(ctx, cond);
    Var *condvar = read_comma_expr(ctx);
    emit(ctx, make_inst4(OP_IF, condvar, body, NULL, cont));
    pop_block(ctx);
    expect(ctx, ')');

    Block *orig_onbreak = ctx->onbreak;
    Block *orig_oncontinue = ctx->oncontinue;
    ctx->oncontinue = cond;
    ctx->onbreak = cont;
    push_block1(ctx, body);
    read_stmt(ctx);
    emit(ctx, make_inst1(OP_JMP, cond));
    pop_block(ctx);
    ctx->oncontinue = orig_oncontinue;
    ctx->onbreak = orig_onbreak;

    replace_block(ctx, cont);
}

static void read_do_stmt(ReadContext *ctx) {
    Block *body = make_block();
    Block *cond = make_block();
    Block *cont = make_block();

    emit(ctx, make_inst1(OP_JMP, body));

    Block *orig_onbreak = ctx->onbreak;
    Block *orig_oncontinue = ctx->oncontinue;
    ctx->oncontinue = body;
    ctx->onbreak = cont;
    push_block1(ctx, body);
    read_stmt(ctx);
    emit(ctx, make_inst1(OP_JMP, cond));
    pop_block(ctx);
    ctx->oncontinue = orig_oncontinue;
    ctx->onbreak = orig_onbreak;

    expect(ctx, KEYWORD_WHILE);
    expect(ctx, '(');
    push_block1(ctx, cond);
    Var *condvar = read_comma_expr(ctx);
    emit(ctx, make_inst4(OP_IF, condvar, body, NULL, cont));
    pop_block(ctx);
    expect(ctx, ')');
    expect(ctx, ';');

    replace_block(ctx, cont);
}

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
        push_block1(ctx, block);
        emit(ctx, make_inst1(OP_JMP, cont));
        pop_block(ctx);
    }
    dict_delete(ctx->label_tbf, label);
}

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
    Token *fname = read_ident(ctx);
    expect(ctx, '(');
    expect(ctx, ')');
    expect(ctx, '{');
    read_compound_stmt(ctx);

    Block *epilogue = make_block();
    emit(ctx, make_inst1(OP_JMP, epilogue));
    push_block1(ctx, epilogue);
    emit(ctx, make_inst1(OP_RETURN, make_imm(CTYPE_INT, (Cvalue)0)));
    pop_block(ctx);

    ctx->entry->name = fname->val.str;
}

List *parse(File *file, Elf *elf) {
    List *r = make_list();
    for (;;) {
        ReadContext *ctx = make_read_context(file, elf);
        if (!peek_token(ctx))
            break;
        read_func_def(ctx);
        check_context(ctx);
        list_push(r, ctx->entry);
    }
    return r;
}
