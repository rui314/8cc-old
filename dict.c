/*
 * dict.c - dictionary (hash table) implementation
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

static Dict *make_dict_int(int type, int size) {
    Dict *r = malloc(sizeof(Dict));
    r->type = type;
    r->buckets = malloc(sizeof(Bucket) * size);
    r->nalloc = size;
    r->nelem = 0;
    for (int i = 0; i < size; i++)
        r->buckets[i].hashval = 0;
    return r;
}

Dict *make_string_dict(void) {
    return make_dict_int(DICT_TYPE_STRING, DICT_INITIAL_SIZE);
}

Dict *make_address_dict(void) {
    return make_dict_int(DICT_TYPE_ADDRESS, DICT_INITIAL_SIZE);
}

static inline u32 string_hash(String *str) {
    u32 hv = 0;
    char *ptr = STRING_BODY(str);
    for (int i = 0; i < STRING_LEN(str); i++) {
        hv = (hv << 5) - hv + (unsigned char)*ptr++;
    }
    return hv == 0 ? 1 : hv;
}

static inline u32 address_hash(void *ptr) {
    return ((u32)(intptr_t)ptr) * 2654435761UL;
}

static inline u32 calculate_hash(Dict *dict, void *obj) {
    switch (dict->type) {
    case DICT_TYPE_STRING:
        return string_hash(obj);
    case DICT_TYPE_ADDRESS:
        return address_hash(obj);
    }
    error("[internal error] unknown dictionary type: %d", dict->type);
}

static Bucket *find_bucket(Dict *dict, void *key, u32 hv) {
    int start = hv % dict->nalloc;
    Bucket *ent;
    for (int i = start; i < start + dict->nalloc; i++) {
        ent = &dict->buckets[i % dict->nalloc];
        if (!ent->hashval) return ent;
        if (ent->hashval != hv)
            continue;
        if (dict->type == DICT_TYPE_STRING && string_equal(ent->key, key))
            return ent;
        if (dict->type == DICT_TYPE_ADDRESS && ent->key == key)
            return ent;
    }
    error("[internal errror] no space found in dictionary");
}

static bool store(Dict *dict, void *key, u32 hv, void *obj) {
    Bucket *ent = find_bucket(dict, key, hv);
    bool r = !!ent->hashval;
    ent->hashval = hv;
    ent->key = key;
    ent->elem = obj;
    return r;
}

static void rehash(Dict *dict) {
    Dict *newdict = make_dict_int(dict->type, dict->nalloc * 2);
    for (int i = 0; i < dict->nalloc; i++) {
        Bucket *ent = &dict->buckets[i];
        if (!ent->hashval)
            continue;
        store(newdict, ent->key, ent->hashval, ent->elem);
    }
    dict->buckets = newdict->buckets;
    dict->nalloc = newdict->nalloc;
}

static void ensure_room(Dict *dict) {
    if (dict->nelem < (dict->nalloc * 3 / 4))
        return;
    rehash(dict);
}

void dict_put(Dict *dict, void *key, void *obj) {
    ensure_room(dict);
    u32 hv = calculate_hash(dict, key);
    bool overwrite = store(dict, key, hv, obj);
    if (!overwrite) dict->nelem++;
}

bool dict_delete(Dict *dict, void *key) {
    Bucket *ent = find_bucket(dict, key, calculate_hash(dict, key));
    if (!ent->hashval) return false;
    ent->hashval = 0;
    ent->key = NULL;
    ent->elem = NULL;
    dict->nelem--;
    return true;
}

void *dict_get(Dict *dict, void *key) {
    Bucket *ent = find_bucket(dict, key, calculate_hash(dict, key));
    if (!ent->hashval) return NULL;
    return ent->elem;
}

/*
 * Iterator
 */
DictIter *make_dict_iter(Dict* dict) {
    DictIter *r = malloc(sizeof(DictIter));
    r->dict = dict;
    r->idx = 0;
    return r;
}

void *dict_iter_next(DictIter* iter) {
    while (iter->idx < iter->dict->nalloc) {
        Bucket *ent = &iter->dict->buckets[iter->idx];
        iter->idx++;
        if (ent->hashval)
            return ent->elem;
    }
    return NULL;
}
