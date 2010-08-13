/*
 * dict.c - dictionary (hash table) implementation
 *
 * Copyright 2010 Rui Ueyama <rui314@gmail.com>.  All rights reserved.
 * This code is available under the simplified BSD license.  See LICENSE for details.
 */

#include "8cc.h"

#define DELETED ((void *)-1)
#define BUCKET_EMPTY(ent) ((ent)->key == NULL || (ent)->key == DELETED)

static bool store(Dict *dict, void *key, u32 hv, void *obj);

/*
 * This is an implementation of open-addressing hash table.  Buckets
 * having NULL in 'key' field are considered as vacant buckets.
 */

static Dict *make_dict_int(dict_hash_fn hashfn, dict_equal_fn equalfn, int size) {
    Dict *r = malloc(sizeof(Dict));
    r->hashfn = hashfn;
    r->equalfn = equalfn;
    r->buckets = malloc(sizeof(Bucket) * size);
    r->nalloc = size;
    r->nelem = 0;
    for (int i = 0; i < size; i++)
        r->buckets[i].key = NULL;
    return r;
}

Dict *make_dict(dict_hash_fn *hashfn, dict_equal_fn *equalfn) {
    return make_dict_int(hashfn, equalfn, DICT_INITIAL_SIZE);
}

/*============================================================
 * Hash functions
 */

static u32 string_hash(void *e) {
    u32 hv = 0;
    String *str = (String *)e;
    char *ptr = STRING_BODY(str);
    for (int i = 0; i < STRING_LEN(str) && ptr[i]; i++) {
        hv = (hv << 5) - hv + (unsigned char)ptr[i];
    }
    return hv;
}

static u32 address_hash(void *ptr) {
    u32 hv = ((u32)(intptr_t)ptr) * 2654435761UL;
    return hv;
}

static bool dict_string_equal(void *a, void *b) {
    return string_equal(a, b);
}

static bool dict_address_equal(void *a, void *b) {
    return a == b;
}

Dict *make_string_dict(void) {
    return make_dict_int(string_hash, dict_string_equal, DICT_INITIAL_SIZE);
}

Dict *make_address_dict(void) {
    return make_dict_int(address_hash, dict_address_equal, DICT_INITIAL_SIZE);
}

/*============================================================
 * Rehashing
 */

static void rehash(Dict *dict) {
    Dict *newdict = make_dict_int(dict->hashfn, dict->equalfn, dict->nalloc * 2);
    for (int i = 0; i < dict->nalloc; i++) {
        Bucket *ent = &dict->buckets[i];
        if (BUCKET_EMPTY(ent))
            continue;
        store(newdict, ent->key, ent->hashval, ent->elem);
    }
    dict->buckets = newdict->buckets;
    dict->nalloc = newdict->nalloc;
}

/*============================================================
 * Accessors
 */

/*
 * Returns a pointer to a bucket to which a given key would be stored.
 */
static Bucket *find_bucket(Dict *dict, void *key, u32 hv, bool put) {
    int start = hv % dict->nalloc;
    Bucket *ent;
    for (int i = start; i < start + dict->nalloc; i++) {
        ent = &dict->buckets[i % dict->nalloc];
        if (put && ent->key == DELETED) return ent;
        if (!ent->key) return ent;
        if (ent->hashval != hv)
            continue;
        if (dict->equalfn(ent->key, key))
            return ent;
    }
    panic("no space found in dictionary");
}

/*
 * Puts a given object to a dictionary.  Returns true iff the given key was
 * already associated with a value.
 */
static bool store(Dict *dict, void *key, u32 hv, void *obj) {
    Bucket *ent = find_bucket(dict, key, hv, true);
    bool r = !BUCKET_EMPTY(ent);
    ent->hashval = hv;
    ent->key = key;
    ent->elem = obj;
    return r;
}

/*
 * Call rehash() if half buckets are already in use.  Otherwise, do
 * nothing.
 */
static void ensure_room(Dict *dict) {
    if (dict->nelem * 2 <= dict->nalloc)
        return;
    rehash(dict);
}

void dict_put(Dict *dict, void *key, void *obj) {
    ensure_room(dict);
    u32 hv = dict->hashfn(key);
    bool overwrite = store(dict, key, hv, obj);
    if (!overwrite) dict->nelem++;
}

bool dict_delete(Dict *dict, void *key) {
    Bucket *ent = find_bucket(dict, key, dict->hashfn(key), false);
    if (BUCKET_EMPTY(ent)) return false;
    ent->hashval = 0;
    ent->key = DELETED;
    ent->elem = NULL;
    dict->nelem--;
    return true;
}

void *dict_get(Dict *dict, void *key) {
    Bucket *ent = find_bucket(dict, key, dict->hashfn(key), false);
    if (BUCKET_EMPTY(ent)) return NULL;
    return ent->elem;
}

bool dict_has(Dict *dict, void *key) {
    Bucket *ent = find_bucket(dict, key, dict->hashfn(key), false);
    return !BUCKET_EMPTY(ent);
}

int dict_size(Dict *dict) {
    return dict->nelem;
}

/*============================================================
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
        if (!BUCKET_EMPTY(ent)) {
            void **r = malloc(sizeof(void *) * 2);
            r[0] = ent->key;
            r[1] = ent->elem;
            return r;
        }
    }
    return NULL;
}
