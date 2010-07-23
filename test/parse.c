#include "../parse.c"
#include "unittest.h"

/*
 * Parser
 */

TEST(read_comment) {
    FILE *stream = create_file("/* 1 * */ 2 // 3 \n 4");
    File *file = make_file(stream, "-");
    ReadContext *ctx = make_read_context(file, NULL, make_cpp_context(file));
    EQ(2, read_token(ctx)->val.i);
    EQ(4, read_token(ctx)->val.i);
    EQ(NULL, read_token(ctx));
}

TEST(read_float) {
    FILE *stream = create_file("1 2.0");
    File *file = make_file(stream, "-");
    ReadContext *ctx = make_read_context(file, NULL, make_cpp_context(file));

    Token *tok = read_token(ctx);
    EQ(TOKTYPE_INT, tok->toktype);
    EQ(1, tok->val.i);

    tok = read_token(ctx);
    EQ(TOKTYPE_FLOAT, tok->toktype);
    EQ(2.0, tok->val.f);
}

TEST(read_char) {
    FILE *stream = create_file("'a' '\\n' '\\0' '\\23' '\\233' '\\x3' '\\x3f'");
    File *file = make_file(stream, "-");
    ReadContext *ctx = make_read_context(file, NULL, make_cpp_context(file));

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
    FILE *stream = create_file("int float ( ) { } ! = ^ == ++ -- ||");
    File *file = make_file(stream, "-");
    ReadContext *ctx = make_read_context(file, NULL, make_cpp_context(file));

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
    FILE *stream = create_file("int float (");
    File *file = make_file(stream, "-");
    ReadContext *ctx = make_read_context(file, NULL, make_cpp_context(file));

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

