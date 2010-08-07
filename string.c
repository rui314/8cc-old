/*
 * string.c - byte string implementation
 *
 * Copyright 2010 Rui Ueyama <rui314@gmail.com>.  All rights reserved.
 * This code is available under the simplified BSD license.  See LICENSE for details.
 */

#include "8cc.h"

static String *make_string_int(int size) {
    String *obj = malloc(sizeof(String));
    obj->buf = malloc(size);
    obj->nalloc = size;
    obj->len = 0;
    obj->pos = 0;
    return obj;
}

String *make_string(void) {
    return make_string_int(STRING_INITIAL_SIZE);
}

String *to_string(char *str) {
    int size = STRING_INITIAL_SIZE;
    int needed = strlen(str) + 1;
    if (size < needed) size <<= 1;
    String *r = make_string_int(size);
    string_append(r, str);
    return r;
}

String *string_copy(String *b) {
    String *r = malloc(sizeof(String));
    *r = *b;
    return r;
}

bool string_equal(String *a, String *b) {
    return !strcmp(STRING_BODY(a), STRING_BODY(b));
}

void string_append(String *b, char *p) {
    out(b, p, strlen(p) + 1);
    string_seek(b, b->len - 1);
}

String *string_prepend(String *b, char *p) {
    String *r = make_string();
    string_append(r, p);
    string_append(r, STRING_BODY(b));
    return r;
}

static void ensure_room(String *b, long room) {
    if (b->nalloc >= (b->pos + room))
        return;
    long newsize = b->nalloc * 2;
    char *buf = malloc(newsize);
    memcpy(buf, b->buf, b->len);
    b->buf = buf;
    b->nalloc = newsize;
}

void o1(String *b, int byte) {
    ensure_room(b, 1);
    b->buf[b->pos++] = byte;
    if (b->len < b->pos)
        b->len = b->pos;
}

void out(String *b, void *data, size_t size) {
    ensure_room(b, size);
    for (int i = 0; i < size; i++)
        o1(b, ((char*)data)[i]);
}

void ostr(String *b, char *str) {
    out(b, str, strlen(str) + 1);
}

void o2(String *b, u16 data) {
    out(b, &data, 2);
}

void o3(String *b, u32 data) {
    out(b, &data, 3);
}

void o4(String *b, u32 data) {
    out(b, &data, 4);
}

void o8(String *b, u64 data) {
    out(b, &data, 8);
}

void align(String *b, int n) {
    int pad = n - b->len % n;
    for (int i = 0; i < pad; i++)
        o1(b, 0);
}

void string_seek(String *b, int pos) {
    if (pos > b->len)
        error("can't seek beyond the string boundary");
    b->pos = pos;
}

void string_vprintf(String *b, char *format, va_list ap) {
    char buf[256];
    int required = vsnprintf(buf, sizeof(buf), format, ap);
    if (required < sizeof(buf)) {
        string_append(b, buf);
        return;
    }
    char *p = malloc(required + 1);
    vsnprintf(p, required + 1, format, ap);
    string_append(b, p);
}

void string_printf(String *b, char *format, ...) {
    va_list ap;
    va_start(ap, format);
    string_vprintf(b, format, ap);
    va_end(ap);
}

String *make_string_printf(char *format, ...) {
    va_list ap;
    va_start(ap, format);
    String *b = make_string();
    string_vprintf(b, format, ap);
    va_end(ap);
    return b;
}
