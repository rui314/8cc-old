/*
 * cpp.c - C preprocessor
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

/*==============================================================================
 * Utility functions for token handling.
 */

static Token *read_if(CppContext *ctx, char *str) {
    Token *tok = read_cpp_token(ctx);
    if (tok->toktype == TOKTYPE_IDENT && !strcmp(STRING_BODY(tok->val.str), str))
        return tok;
    unget_cpp_token(ctx, tok);
    return NULL;
}

/*==============================================================================
 * Keyword recognizer.
 */

static Dict *keyword_dict(void) {
    static Dict *dict;
    if (dict != NULL)
        return dict;
    dict = make_string_dict();
#define KEYWORD(id_, str_) \
    dict_put(dict, to_string(str_), (void *)id_);
#define OP(_)
# include "keyword.h"
#undef OP
#undef KEYWORD
    return dict;
}

static Token *make_keyword(CppContext *ctx, int k) {
    Token *r = malloc(sizeof(Token));
    r->toktype = TOKTYPE_KEYWORD;
    r->val.i = k;
    r->line = ctx->file->line;
    r->column = ctx->file->column;
    return r;
}

static Token *to_keyword(CppContext *ctx, Token *tok) {
    assert(tok->toktype == TOKTYPE_IDENT);
    int id = (intptr)dict_get(keyword_dict(), tok->val.str);
    if (id)
        return make_keyword(ctx, id);
    return tok;
}

static Token *cppnum_to_num(Token *tok) {
    Token *r = copy_token(tok);
    char *p = STRING_BODY(tok->val.str);
    if (strchr(p, '.')) {
        r->toktype = TOKTYPE_FLOAT;
        r->val.f = atof(p);
    } else {
        r->toktype = TOKTYPE_INT;
        r->val.i = atoi(p);
    }
    return r;
}

/*==============================================================================
 * Predefined macros.
 */

static Token *handle_variable_macro(CppContext *cpp, Token *tok) {
    if (!strcmp("__FILE__", STRING_BODY(tok->val.str)))
        return make_str_literal(cpp, cpp->file->filename);
    if (!strcmp("__LINE__", STRING_BODY(tok->val.str))) {
        char buf[10];
        sprintf(buf, "%d", cpp->file->line);
        return cppnum_to_num(make_cppnum(cpp, to_string(buf)));
    }
    panic("Unknown variable macro: '%s'", STRING_BODY(tok->val.str));
}

/*==============================================================================
 * C preprocessor.
 */

static void read_define(CppContext *ctx) {
    Token *name = read_cpp_token(ctx);
    if (name->toktype != TOKTYPE_IDENT)
        error_cpp_ctx(ctx, "macro name must be an identifier, but got '%s'", token_to_string(name));
    List *val = make_list();
    Token *tok = read_cpp_token(ctx);
    while (tok && tok->toktype != TOKTYPE_NEWLINE) {
        list_push(val, tok);
        tok = read_cpp_token(ctx);
    }
    dict_put(ctx->defs, name->val.str, val);
}

// WG14/N1256 6.10.5 Error directive.
static void read_error_dir(CppContext *ctx, Token *define) {
    String *buf = make_string();
    Token *tok = read_cpp_token(ctx);
    while(tok &&  tok->toktype != TOKTYPE_NEWLINE) {
        o1(buf, ' ');
        string_append(buf, token_to_string(tok));
        tok = read_cpp_token(ctx);
    }
    error_token(define, "error: #error:%s", STRING_BODY(buf));
}

static void read_directive(CppContext *ctx) {
    Token *tok;
    if (read_if(ctx, "define"))
        read_define(ctx);
    else if ( (tok = read_if(ctx, "error")) )
        read_error_dir(ctx, tok);
    else
        error_cpp_ctx(ctx, "'#' must be followed by 'define' for now");
}

Token *read_token(ReadContext *ctx) {
    if (!LIST_IS_EMPTY(ctx->ungotten))
        return list_pop(ctx->ungotten);

    CppContext *cppctx = ctx->cppctx;
    for (;;) {
        Token *tok = read_cpp_token(cppctx);
        if (!tok)
            return NULL;
        if (tok->toktype == TOKTYPE_NEWLINE) {
            cppctx->at_bol = true;
            continue;
        }
        if (cppctx->at_bol && tok->toktype == TOKTYPE_PUNCT && tok->val.i == '#') {
            read_directive(cppctx);
            cppctx->at_bol = true;
            continue;
        }
        cppctx->at_bol = false;
        if (tok->toktype == TOKTYPE_IDENT) {
            List *defs = dict_get(cppctx->defs, tok->val.str);
            if (defs == VARIABLE_MACRO)
                return handle_variable_macro(cppctx, tok);
            if (!defs)
                return to_keyword(cppctx, tok);
            for (int i = LIST_LEN(defs) - 1; i >= 0; i--)
                unget_cpp_token(cppctx, (Token *)LIST_REF(defs, i));
            continue;
        }
        if (tok->toktype == TOKTYPE_CPPNUM)
            return cppnum_to_num(tok);
        if (tok->toktype == TOKTYPE_PUNCT) {
            Token *r = copy_token(tok);
            r->toktype = TOKTYPE_KEYWORD;
            return r;
        }
        assert(tok->toktype == TOKTYPE_CHAR || tok->toktype == TOKTYPE_STRING);
        return tok;
    }
}
