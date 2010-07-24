/*
 * error.c - error handlers
 *
 * Copyright 2010 Rui Ueyama <rui314@gmail.com>.  All rights reserved.
 * This code is available under the simplified BSD license.  See LICENSE for details.
 */

#include "8cc.h"

jmp_buf *on_error_dst;
String *on_error_msg;

static void print(char *pre, char *format, va_list ap) {
    fprintf(stderr, "%s", pre);
    vfprintf(stderr, format, ap);
    fprintf(stderr, "\n");
}

static NORETURN void verror(char *format, va_list ap) {
    if (on_error_dst) {
        on_error_msg = make_string();
        string_append(on_error_msg, "ERROR: ");
        string_vprintf(on_error_msg, format, ap);
        longjmp(*on_error_dst, 1);
    }
    print("ERROR: ", format, ap);
    exit(-1);
}

static void vwarn(char *format, va_list ap) {
    print("WARN: ", format, ap);
}

NORETURN void error(char *format, ...) {
    va_list ap;
    va_start(ap, format);
    verror(format, ap);
}

void warn(char *format, ...) {
    va_list ap;
    va_start(ap, format);
    vwarn(format, ap);
}

NORETURN void print_parse_error(int line, int column, char *msg, va_list ap) {
    String *b = make_string();
    string_printf(b, "Line %d:%d: ", line, column);
    string_append(b, msg);
    verror(STRING_BODY(b), ap);
}
