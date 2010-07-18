/*
 * test.c - unit tests
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

#define NOT_NULL(p) do { if (!(p)) error("Line %d: must not be null " #p, __LINE__); } while (0)
#define EQ(x, y) do { if ((x) != (y)) error("Line %d: must be the same: '%s' and '%s'", __LINE__, #x, #y); } while (0)
#define EQ_CHAR(x, y) do { eq_char(__LINE__, (x), (y)); } while (0)
#define EQ_STR(x, y)  do { eq_str(__LINE__, (x), (y)); } while (0)

static void eq_str(int line, char *expected, char *got) {
    if (strcmp(expected, got)) {
        error("line %d: \"%s\" expected, but got \"%s\"", line, expected, got);
    }
}

static void eq_char(int line, int expected, int got) {
    if (expected != got) {
        error("line %d: '%c' expected, but got '%c'", line, expected, got);
    }
}

/*
 * String
 */

static int64_t qword(char *str) {
    int64_t r = 0;
    for (int i = strlen(str) - 1; i >= 0; i--)
        r = (r << 8) | str[i];
    return r;
}

static void test_string(void) {
    String *b = make_string();
    NOT_NULL(b);
    EQ(STRING_LEN(b), 0);

    o1(b, 'a');
    EQ(STRING_LEN(b), 1);
    EQ(STRING_BODY(b)[0], 'a');

    o8(b, qword("bcdefghi"));
    EQ(STRING_LEN(b), 9);
    EQ(strcmp(STRING_BODY(b), "abcdefghi"), 0);
}

/*
 * List
 */

static void test_list(void) {
    List *list = make_list();
    EQ(0, LIST_LEN(list));

    list_push(list, (void *)17);
    list_push(list, (void *)42);
    EQ(2, (intptr)LIST_LEN(list));
    EQ(17, (intptr)LIST_REF(list, 0));
    EQ(42, (intptr)LIST_REF(list, 1));
    EQ(42, (intptr)list_pop(list));
    EQ(17, (intptr)list_pop(list));
    EQ(0, LIST_LEN(list));

    list_push(list, (void *)17);
    list_push(list, (void *)42);
    EQ(17, (intptr)list_unshift(list));
    EQ(42, (intptr)list_unshift(list));
}

/*
 * Dictionary
 */

static void test_dict(void) {
    Dict *dict = make_string_dict();
    EQ(0, dict->nelem);
    String *k = to_string("abc");
    dict_put(dict, k, (void *)-1);
    EQ(1, dict->nelem);
    EQ(-1, (long) dict_get(dict, k));
    EQ(NULL, dict_get(dict, to_string("nonexistent")));

    // test rehashing
    for (int i = 0; i < DICT_INITIAL_SIZE * 2; i++) {
        char buf[] = "0123456789";
        sprintf(buf, "key%d", i);
        dict_put(dict, to_string(buf), (void *)(long)i);
    }
    EQ(1 + DICT_INITIAL_SIZE * 2, dict->nelem);
    for (int i = 0; i < DICT_INITIAL_SIZE * 2; i++) {
        char buf[] = "0123456789";
        sprintf(buf, "key%d", i);
        EQ(i, (int)(long)dict_get(dict, to_string(buf)));
    }

    // Store duplicate key
    dict_put(dict, k, (void *)-2);
    EQ(-2, (long) dict_get(dict, k));
    EQ(1 + DICT_INITIAL_SIZE * 2, dict->nelem);

    // Removal
    bool existed = dict_delete(dict, k);
    EQ(DICT_INITIAL_SIZE * 2, dict->nelem);
    EQ(NULL, dict_get(dict, k));
    EQ(true, existed);
    existed = dict_delete(dict, k);
    EQ(false, existed);

    // Address dictionary
    dict = make_address_dict();
    k = to_string("abc");
    dict_put(dict, k, (void *)-1);
    EQ(-1, (long) dict_get(dict, k));
    EQ(NULL, dict_get(dict, to_string("abc")));
}

static void test_dict_iter(void) {
    Dict *dict = make_string_dict();
    DictIter *iter = make_dict_iter(dict);
    EQ(NULL, dict_iter_next(iter));

    dict_put(dict, to_string("key"), (void *)-1);
    iter = make_dict_iter(dict);
    void **p = dict_iter_next(iter);
    EQ_STR(STRING_BODY((String *)to_string("key")), STRING_BODY((String *)p[0]));
    EQ(-1, (intptr)p[1]);
    EQ(NULL, dict_iter_next(iter));
}


/*
 * File IO
 */
static FILE *create_file(char *content) {
    char tmpl[] = "tmpXXXXXX";
    int fd = mkstemp(tmpl);
    if (fd < 0) {
        perror("fd: ");
        exit(-1);
    }
    unlink(tmpl);
    write(fd, content, strlen(content));
    lseek(fd, 0, SEEK_SET);
    return fdopen(fd, "r");
}

static void test_file_unreadc(void) {
    FILE *stream = create_file("a\n");
    File *file = make_file(stream, "-");
    EQ(1, file->line);
    EQ(1, file->column);
    EQ_CHAR('a', readc(file));
    EQ(2, file->column);
    EQ_CHAR('\n', readc(file));
    EQ(2, file->line);
    EQ(1, file->column);
    unreadc('\n', file);
    EQ(1, file->line);
    EQ(2, file->column);
    EQ_CHAR('\n', readc(file));
    EQ(2, file->line);
    EQ(1, file->column);
}

static void test_file_next_char_is(void) {
    FILE *stream = create_file("ab");
    File *file = make_file(stream, "-");
    EQ(false, next_char_is(file, 'b'));
    EQ(false, next_char_is(file, 'b'));
    EQ(true,  next_char_is(file, 'a'));
    EQ(false, next_char_is(file, 'a'));
    EQ(true,  next_char_is(file, 'b'));
}

static void test_file_simple(void) {
    char *data = "ab\nc\r\r\nd\r";
    FILE *stream = create_file(data);
    File *file = make_file(stream, "foo");
    EQ_STR(STRING_BODY(file->filename), "foo");

    EQ(1, file->line);
    EQ(1, file->column);
    EQ_CHAR('a', readc(file));
    EQ_CHAR('b', readc(file));
    EQ(3, file->column);
    EQ_CHAR('\n', readc(file));
    EQ(2, file->line);
    EQ(1, file->column);
    EQ_CHAR('c', readc(file));
    EQ_CHAR('\n', readc(file));
    EQ(3, file->line);
    EQ_CHAR('\n', readc(file));
    EQ(4, file->line);
    EQ_CHAR('d', readc(file));
    EQ_CHAR('\n', readc(file));
    EQ(5, file->line);
    EQ_CHAR(EOF, readc(file));
}

static void test_file_backslash_at_eol(void) {
    char *data = "2\\\n0\\\r\n10";
    FILE *stream = create_file(data);
    File *file = make_file(stream, "foo");

    EQ(1, file->line);
    EQ(1, file->column);
    EQ_CHAR('2', readc(file));
    EQ_CHAR('0', readc(file));
    EQ_CHAR('1', readc(file));
    EQ_CHAR('0', readc(file));
    EQ(3, file->line);
    EQ(3, file->column);
}

/*
 * Parser
 */

static void test_read_comment(void) {
    FILE *stream = create_file("/* 1 * */ 2 // 3 \n 4");
    File *file = make_file(stream, "-");
    ReadContext *ctx = make_read_context(file, NULL, make_cpp_context(file));
    EQ(2, read_token(ctx)->val.i);
    EQ(4, read_token(ctx)->val.i);
    EQ(NULL, read_token(ctx));
}

static void test_read_float(void) {
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

static void test_read_char(void) {
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

static void test_read_keywords(void) {
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

static void test_read_unget_token(void) {
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

/*
 * Entry point
 */

int main(int argc, char **argv) {
    printf("Running unit tests... ");
    fflush(stdout);

    test_string();
    test_list();
    test_dict();
    test_dict_iter();

    test_file_simple();
    test_file_unreadc();
    test_file_next_char_is();
    test_file_backslash_at_eol();

    test_read_comment();
    test_read_float();
    test_read_char();
    test_read_keywords();
    test_read_unget_token();

    printf("OK\n");
    return 0;
}
