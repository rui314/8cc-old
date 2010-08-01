/*
 * error.c - error handlers
 *
 * Copyright 2010 Rui Ueyama <rui314@gmail.com>.  All rights reserved.
 * This code is available under the simplified BSD license.  See LICENSE for details.
 */

#include "8cc.h"

Exception *current_handler;

Exception *make_exception(void) {
    Exception *e = malloc(sizeof(Exception));
    e->msg = NULL;
    return e;
}

static void print(char *pre, char *format, va_list ap) {
    fprintf(stderr, "%s", pre);
    vfprintf(stderr, format, ap);
    fprintf(stderr, "\n");
}

static NORETURN void verror(char *format, va_list ap) {
    if (current_handler) {
        Exception *e = current_handler;
        current_handler = NULL;
        e->msg = to_string("ERROR: ");
        string_vprintf(e->msg, format, ap);
        longjmp(e->jmpbuf, 1);
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
    va_end(ap);
}

void warn(char *format, ...) {
    va_list ap;
    va_start(ap, format);
    vwarn(format, ap);
    va_end(ap);
}

NORETURN void print_parse_error(int line, int column, char *msg, va_list ap) {
    String *b = make_string_printf("Line %d:%d: ", line, column);
    string_append(b, msg);
    verror(STRING_BODY(b), ap);
}
