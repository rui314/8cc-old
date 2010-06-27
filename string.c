/*
 * string.c - byte string implementation
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

static String *make_string_int(int size) {
    String *obj = malloc(sizeof(String));
    obj->buf = malloc(size);
    obj->nalloc = size;
    obj->len = 0;
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
    out(r, str, needed);
    return r;
}

bool string_equal(String *a, String *b) {
    return !strcmp(STRING_BODY(a), STRING_BODY(b));
}

static void ensure_room(String *b, long room) {
    if (b->nalloc >= (b->len + room))
        return;
    long newsize = b->nalloc * 2;
    char *buf = malloc(newsize);
    memcpy(buf, b->buf, b->len);
    b->buf = buf;
    b->nalloc = newsize;
}

void o1(String *b, int byte) {
    ensure_room(b, 1);
    b->buf[b->len++] = byte;
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
