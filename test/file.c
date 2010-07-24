/*
 * Copyright 2010 Rui Ueyama <rui314@gmail.com>.  All rights reserved.
 * This code is available under the simplified BSD license.  See LICENSE for details.
 */

#include "unittest.h"
#include "../file.c"

/*
 * File IO
 */

TEST(file_unreadc) {
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

TEST(file_next_char_is) {
    FILE *stream = create_file("ab");
    File *file = make_file(stream, "-");
    EQ(false, next_char_is(file, 'b'));
    EQ(false, next_char_is(file, 'b'));
    EQ(true,  next_char_is(file, 'a'));
    EQ(false, next_char_is(file, 'a'));
    EQ(true,  next_char_is(file, 'b'));
}

TEST(file_simple) {
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

TEST(file_backslash_at_eol) {
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
