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

#define NOT_NULL(p) do { if (!(p)) error("line %d: must not be null " #p, __LINE__); } while (0)
#define EQ(x, y) do { if ((x) != (y)) error("line %d: must be the same", __LINE__); } while (0)
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
    EQ(-1, (long)dict_iter_next(iter));
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
    File *file = make_file(stream, "foo");
    EQ(1, file->lineno);
    EQ_CHAR('a', readc(file));
    EQ_CHAR('\n', readc(file));
    EQ(2, file->lineno);
    unreadc('\n', file);
    EQ(1, file->lineno);
    EQ_CHAR('\n', readc(file));
    EQ(2, file->lineno);
}

static void test_file_simple(void) {
    char *data = "ab\nc\r\r\nd\r";
    FILE *stream = create_file(data);
    File *file = make_file(stream, "foo");
    EQ_STR(STRING_BODY(file->filename), "foo");

    EQ(1, file->lineno);
    EQ_CHAR('a', readc(file));
    EQ_CHAR('b', readc(file));
    EQ_CHAR('\n', readc(file));
    EQ(2, file->lineno);
    EQ_CHAR('c', readc(file));
    EQ_CHAR('\n', readc(file));
    EQ(3, file->lineno);
    EQ_CHAR('\n', readc(file));
    EQ(4, file->lineno);
    EQ_CHAR('d', readc(file));
    EQ_CHAR('\n', readc(file));
    EQ(5, file->lineno);
    EQ_CHAR(EOF, readc(file));
}

/*
 * Parser
 */

static void test_read_float(void) {
    FILE *stream = create_file("1 2.0");
    File *file = make_file(stream, "-");
    ReadContext *ctx = make_read_context(file, NULL);

    Token *tok = read_token(ctx);
    EQ(TOKTYPE_INT, tok->toktype);
    EQ(1, tok->val.i);

    tok = read_token(ctx);
    EQ(TOKTYPE_FLOAT, tok->toktype);
    EQ(2.0, tok->val.f);
}

static void test_read_char(void) {
    FILE *stream = create_file("'a'");
    File *file = make_file(stream, "-");
    ReadContext *ctx = make_read_context(file, NULL);

    Token *tok = read_token(ctx);
    EQ(TOKTYPE_CHAR, tok->toktype);
    EQ('a', tok->val.c);
}

static void test_read_keywords_int(ReadContext *ctx, int type) {
    Token *tok = read_token(ctx);
    EQ(TOKTYPE_KEYWORD, tok->toktype);
    EQ(tok->val.k, type);
}

static void test_read_keywords(void) {
    FILE *stream = create_file("int float ( ) { }");
    File *file = make_file(stream, "-");
    ReadContext *ctx = make_read_context(file, NULL);

    test_read_keywords_int(ctx, KEYWORD_INT);
    test_read_keywords_int(ctx, KEYWORD_FLOAT);
    test_read_keywords_int(ctx, '(');
    test_read_keywords_int(ctx, ')');
    test_read_keywords_int(ctx, '{');
    test_read_keywords_int(ctx, '}');
}

/*
 * Entry point
 */

int main(int argc, char **argv) {
    test_string();

    test_dict();
    test_dict_iter();

    test_file_simple();
    test_file_unreadc();

    test_read_float();
    test_read_char();
    test_read_keywords();

    printf("OK\n");
    return 0;
}
