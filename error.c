/*
 * error.c - error handlers
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

static void print(char *pre, char *format, va_list ap) {
    fprintf(stderr, "%s", pre);
    vfprintf(stderr, format, ap);
    fprintf(stderr, "\n");
}

ATTRIBUTE((noreturn)) static void verror(char *format, va_list ap) {
    print("ERROR: ", format, ap);
    exit(-1);
}

static void vwarn(char *format, va_list ap) {
    print("WARN: ", format, ap);
}

void error(char *format, ...) {
    va_list ap;
    va_start(ap, format);
    verror(format, ap);
}

void warn(char *format, ...) {
    va_list ap;
    va_start(ap, format);
    vwarn(format, ap);
}

void print_parse_error(int line, int column, char *msg, va_list ap) {
    String *b = make_string();
    string_printf(b, "Line %d:%d: ", line, column);
    string_append(b, msg);
    verror(STRING_BODY(b), ap);
}
