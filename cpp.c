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

#define EMPTY ((void *)-1UL)

/*============================================================
 * Utility functions for token handling.
 */

Token *peek_token(ReadContext *ctx) {
    Token *r = read_token(ctx);
    unget_token(ctx, r);
    return r;
}

void unget_token(ReadContext *ctx, Token *tok) {
    list_push(ctx->ungotten, tok);
}

bool next_token_is(ReadContext *ctx, int keyword) {
    Token *tok = read_token(ctx);
    if (IS_KEYWORD(tok, keyword))
        return true;
    unget_token(ctx, tok);
    return false;
}

/*============================================================
 * Keyword recognizer.
 */

Dict *reserved_word(void) {
    static Dict *reserved_word;
    if (reserved_word != NULL)
        return reserved_word;
    reserved_word = make_string_dict();
#define KEYWORD(id_, str_) \
    dict_put(reserved_word, to_string(str_), (void *)id_);
#define OP(_)
# include "keyword.h"
#undef OP
#undef KEYWORD
    return reserved_word;
}

static Token *to_keyword(ReadContext *ctx, Token *tok) {
    if (tok->toktype != TOKTYPE_IDENT)
        return tok;
    int id = (intptr)dict_get(reserved_word(), tok->val.str);
    if (id)
        return make_keyword(ctx, id);
    return tok;
}

/*============================================================
 * C preprocessor.
 */

static bool skip_cpp_token(ReadContext *ctx, char c) {
    Token *tok = read_cpp_token(ctx);
    if (tok->toktype == TOKTYPE_CPP && tok->val.c == c)
        return true;
    unget_token(ctx, tok);
    return false;
}

static void read_define(ReadContext *ctx) {
    if (!skip_cpp_token(ctx, ' ')) {
        Token *tok = read_token(ctx);
        error_token(tok, "#define must be followed by space");
    }
    Token *name = read_token(ctx);
    if (name->toktype != TOKTYPE_IDENT)
        error_token(name, "#define must be followed by an identifier");
    Token *val;
    if (skip_cpp_token(ctx, '\n'))
        val = EMPTY;
    else
        val = read_token(ctx);
    dict_put(ctx->defs, name->val.str, val);
}

static void read_pound(ReadContext *ctx) {
    skip_cpp_token(ctx, ' ');
    Token *tok = read_cpp_token(ctx);
    if (tok->toktype == TOKTYPE_IDENT && string_equal(tok->val.str, to_string("define")))
        read_define(ctx);
    else
        error_token(tok, "'#' must be followed by 'define'");
}

Token *read_token(ReadContext *ctx) {
    for (;;) {
        Token *tok = read_cpp_token(ctx);
        if (!tok)
            return NULL;
        if (tok->toktype == TOKTYPE_KEYWORD)
            return tok;
        if (tok->toktype == TOKTYPE_IDENT) {
            Token *val = dict_get(ctx->defs, tok->val.str);
            if (!val)
                return to_keyword(ctx, tok);
            if (val == EMPTY)
                continue;
            return val;
        }
        if (tok->toktype != TOKTYPE_CPP)
            return tok;
        if (tok->val.c == '#')
            read_pound(ctx);
    }
}
