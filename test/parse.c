/*
 * Copyright 2010 Rui Ueyama <rui314@gmail.com>.  All rights reserved.
 * This code is available under the simplified BSD license.  See LICENSE for details.
 */

#include "../parse.c"
#include "unittest.h"

/*
 * Parser
 */

static ReadContext *make_context(char *str) {
    File *file = mkfile(str);
    return make_read_context(file, NULL, make_cpp_context(file));
}

TEST(read_comment) {
    ReadContext *ctx = make_context("/* 1 * */ 2 // 3 \n 4");
    EQ(2, read_token(ctx)->val.i);
    EQ(4, read_token(ctx)->val.i);
    EQ(NULL, read_token(ctx));
}

TEST(read_float) {
    ReadContext *ctx = make_context("1 2.0");

    Token *tok = read_token(ctx);
    EQ(TOKTYPE_INT, tok->toktype);
    EQ(1, tok->val.i);

    tok = read_token(ctx);
    EQ(TOKTYPE_FLOAT, tok->toktype);
    EQ(2.0, tok->val.f);
}

TEST(read_char) {
    ReadContext *ctx = make_context("'a' '\\n' '\\0' '\\23' '\\233' '\\x3' '\\x3f'");

    Token *tok = read_token(ctx);
    EQ(TOKTYPE_CHAR, tok->toktype);
    EQ('a', tok->val.i);
    EQ('\n', read_token(ctx)->val.i);
    EQ('\0', read_token(ctx)->val.i);
    EQ('\23', read_token(ctx)->val.i);
    EQ('\233', read_token(ctx)->val.i);
    EQ('\x3', read_token(ctx)->val.i);
    EQ('\x3f', read_token(ctx)->val.i);
}

#define TEST_READ_KEYWORDS(ctx_, type_)    \
    do {                                   \
        Token *tok = read_token(ctx_);     \
        EQ(TOKTYPE_KEYWORD, tok->toktype); \
        EQ(tok->val.i, type_);             \
    } while (0)

TEST(read_keywords) {
    ReadContext *ctx = make_context("int float ( ) { } ! = ^ == ++ -- ||");

    TEST_READ_KEYWORDS(ctx, KEYWORD_INT);
    TEST_READ_KEYWORDS(ctx, KEYWORD_FLOAT);
    TEST_READ_KEYWORDS(ctx, '(');
    TEST_READ_KEYWORDS(ctx, ')');
    TEST_READ_KEYWORDS(ctx, '{');
    TEST_READ_KEYWORDS(ctx, '}');
    TEST_READ_KEYWORDS(ctx, '!');
    TEST_READ_KEYWORDS(ctx, '=');
    TEST_READ_KEYWORDS(ctx, '^');
    TEST_READ_KEYWORDS(ctx, KEYWORD_EQ);
    TEST_READ_KEYWORDS(ctx, KEYWORD_INC);
    TEST_READ_KEYWORDS(ctx, KEYWORD_DEC);
    TEST_READ_KEYWORDS(ctx, KEYWORD_LOG_OR);
}

TEST(read_unget_token) {
    ReadContext *ctx = make_context("int float (");

    Token *t0 = read_token(ctx);
    Token *t1 = read_token(ctx);
    Token *t2 = read_token(ctx);
    unget_token(ctx, t2);
    unget_token(ctx, t1);
    unget_token(ctx, t0);

    TEST_READ_KEYWORDS(ctx, KEYWORD_INT);
    TEST_READ_KEYWORDS(ctx, KEYWORD_FLOAT);
    TEST_READ_KEYWORDS(ctx, '(');
}

/*
 * A function used by sizeof operator.
 */
TEST(ctype_sizeof) {
    EQ(1, ctype_sizeof(make_ctype(CTYPE_CHAR)));
    EQ(2, ctype_sizeof(make_ctype(CTYPE_SHORT)));
    EQ(4, ctype_sizeof(make_ctype(CTYPE_INT)));
    EQ(8, ctype_sizeof(make_ctype(CTYPE_LONG)));
    EQ(8, ctype_sizeof(make_ctype_ptr(make_ctype(CTYPE_CHAR))));
    EQ(8, ctype_sizeof(make_ctype_ptr(make_ctype(CTYPE_INT))));
    EQ(8, ctype_sizeof(make_ctype_ptr(make_ctype_ptr((make_ctype(CTYPE_INT))))));
    EQ(20, ctype_sizeof(make_ctype_array(make_ctype(CTYPE_CHAR), 20)));
    EQ(36, ctype_sizeof(make_ctype_array(make_ctype_array(make_ctype(CTYPE_INT), 3), 3)));
}

/*==============================================================================
 * Tests for the new intermediate language
 */

TEST(nread_declaration_spec) {
    EQ(get_int_type(SINT), nread_declaration_spec(make_context("int x")));
    EQ(get_int_type(SINT), nread_declaration_spec(make_context("signed x")));
    EQ(get_int_type(SINT), nread_declaration_spec(make_context("signed int x")));

    EQ(get_int_type(UINT), nread_declaration_spec(make_context("unsigned x")));
    EQ(get_int_type(UINT), nread_declaration_spec(make_context("unsigned int x")));

    EQ(get_int_type(SLONG), nread_declaration_spec(make_context("long x")));
    EQ(get_int_type(ULONG), nread_declaration_spec(make_context("unsigned long x")));

    EQ(get_float_type(FLOAT), nread_declaration_spec(make_context("float x")));
    EQ(get_float_type(DOUBLE), nread_declaration_spec(make_context("double x")));
}
