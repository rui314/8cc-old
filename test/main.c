/*
 * Copyright 2010 Rui Ueyama <rui314@gmail.com>.  All rights reserved.
 * This code is available under the simplified BSD license.  See LICENSE for details.
 */

#include "unittest.h"

List* test_funcs;

void eq_str(int line, char *expected, char *got) {
    if (strcmp(expected, got))
        error("line %d: \"%s\" expected, but got \"%s\"", line, expected, got);
}

void eq_char(int line, int expected, int got) {
    if (expected != got)
        error("line %d: '%c' expected, but got '%c'", line, expected, got);
}

void contains(int line, char *expected, char *got) {
    if (!strstr(got, expected))
        error("line %d: '%s' expected, but got '%s'", line, expected, got);
}

File *mkfile(char *str) {
    return make_string_file(to_string(str));
}

/*
 * Entry point
 */

RUN_TESTS()
