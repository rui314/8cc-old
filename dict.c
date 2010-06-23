#include "8cc.h"

static Dict *make_dict_int(int size) {
    Dict *r = malloc(sizeof(Dict));
    r->buckets = malloc(sizeof(Bucket) * size);
    r->nalloc = size;
    r->nelem = 0;
    for (int i = 0; i < size; i++)
        r->buckets[i].hashval = 0;
    return r;
}

Dict *make_dict(void) {
    return make_dict_int(DICT_INITIAL_SIZE);
}

static inline u32 string_hash(String *str) {
    u32 hv = 0;
    char *ptr = STRING_BODY(str);
    for (int i = 0; i < STRING_LEN(str); i++) {
        hv = (hv << 5) - hv + (unsigned char)*ptr++;
    }
    return hv == 0 ? 1 : hv;
}

static bool store(Dict *dict, String *key, u32 hv, void *obj) {
    int start = hv % dict->nalloc;
    Bucket *ent;
    int r = false;
    for (int i = start; i < dict->nalloc; i++) {
        ent = &dict->buckets[i];
        if (!ent->hashval) goto found1;
        if (ent->hashval == hv && string_equal(ent->key, key))
            goto found;
    }
    for (int i = 0; i < start; i++) {
        ent = &dict->buckets[i];
        if (!ent->hashval) goto found1;
        if (ent->hashval == hv && string_equal(ent->key, key))
            goto found;
    }
    error("[internal errror] no space found in dictionary");
 found:
    r = true;
 found1:
    ent->hashval = hv;
    ent->key = key;
    ent->elem = obj;
    return r;
}

static void rehash(Dict *dict) {
    Dict *newdict = make_dict_int(dict->nalloc * 2);
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

void dict_put(Dict *dict, String *key, void *obj) {
    ensure_room(dict);
    u32 hv = string_hash(key);
    bool overwrite = store(dict, key, hv, obj);
    if (!overwrite) dict->nelem++;
}

static Bucket *dict_get_int(Dict *dict, String *key) {
    u32 hv = string_hash(key);
    int start = hv % dict->nalloc;
    Bucket *ent;
    for (int i = start; i < dict->nalloc; i++) {
        ent = &dict->buckets[i];
        if (!ent->hashval) return NULL;
        if (string_equal(ent->key, key)) return ent;
    }
    for (int i = 0; i < start; i++) {
        ent = &dict->buckets[i];
        if (!ent->hashval) return NULL;
        if (string_equal(ent->key, key)) return ent;
    }
    error("[internal errror] dict is full.  Should not happen.");
    return NULL;
}

bool dict_delete(Dict *dict, String *key) {
    Bucket *ent = dict_get_int(dict, key);
    if (!ent) return false;
    ent->hashval = 0;
    ent->key = NULL;
    ent->elem = NULL;
    dict->nelem--;
    return true;
}

void *dict_get(Dict *dict, String *key) {
    Bucket *ent = dict_get_int(dict, key);
    if (!ent) return NULL;
    return ent->elem;
}
