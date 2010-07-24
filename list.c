/*
 * list.c - array list implementation
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

static List *make_list_int(int size) {
    List *obj = malloc(sizeof(List));
    obj->elems = malloc(sizeof(void*) * size);
    for (int i = 0; i < size; i++)
        obj->elems[i] = NULL;
    obj->nalloc = size;
    obj->len = 0;
    return obj;
}

List *make_list(void) {
    return make_list_int(LIST_INITIAL_SIZE);
}

List *make_list1(void *e) {
    List *r = make_list();
    list_push(r, e);
    return r;
}

static void ensure_room(List *list) {
    if (list->len < list->nalloc) return;
    int newsize = list->nalloc * 2;
    void **buf = malloc(sizeof(void*) * newsize);
    memcpy(buf, list->elems, sizeof(void*) * list->len);
    for (int i = list->len; i < newsize; i++)
        buf[i] = NULL;
    list->elems = buf;
    list->nalloc = newsize;
}

void list_push(List *list, void *e) {
    ensure_room(list);
    list->elems[list->len++] = e;
}

void *list_pop(List *list) {
    if (list->len == 0)
        error("list empty");
    void *r = list->elems[--list->len];
    list->elems[list->len] = NULL;
    return r;
}

void *list_unshift(List *list) {
    if (list->len == 0)
        panic("list empty");
    void *r = list->elems[0];
    for (int i = 1; i < list->len; i++)
        list->elems[i - 1] = list->elems[i];
    list->len--;
    return r;
}

List *sublist(List *orig, int off) {
    List *r = malloc(sizeof(List));
    r->elems = orig->elems + off;
    r->nalloc = orig->nalloc - off;
    r->len = orig->len - off;
    return r;
}

void list_append(List *a, List *b) {
    for (int i = 0; i < LIST_LEN(b); i++)
        list_push(a, LIST_REF(b, i));
}

List *list_reverse(List *list) {
    List *r = make_list_int(list->nalloc);
    for (int i = LIST_LEN(list) - 1; i >= 0; i--)
        list_push(r, LIST_REF(list, i));
    return r;
}

List *list_copy(List *list) {
    List *r = make_list_int(list->nalloc);
    for (int i = 0; i < LIST_LEN(list); i++)
        list_push(r, LIST_REF(list, i));
    return r;
}

bool list_in(List *list, String *e) {
    for (int i = 0; i < LIST_LEN(list); i++)
        if (string_equal(LIST_REF(list, i), e))
            return true;
    return false;
}

List *list_union(List *a, List *b) {
    if (LIST_IS_EMPTY(a)) return b;
    if (LIST_IS_EMPTY(b)) return a;
    List *r = list_copy(a);
    for (int i = 0; i < LIST_LEN(b); i++)
        if (!list_in(r, LIST_REF(b, i)))
            list_push(r, LIST_REF(b, i));
    return r;
}

List *list_union1(List *list, String *e) {
    if (list_in(list, e))
        return list;
    List *r = list_copy(list);
    list_push(r, e);
    return r;
}

List *list_intersect(List *a, List *b) {
    if (LIST_IS_EMPTY(a)) return a;
    if (LIST_IS_EMPTY(b)) return b;
    List *r = make_list();
    for (int i = 0; i < LIST_LEN(a); i++)
        if (list_in(b, LIST_REF(a, i)))
            list_push(r, LIST_REF(a, i));
    return r;
}
