/*
 * Copyright 2010 Rui Ueyama <rui314@gmail.com>.  All rights reserved.
 * This code is available under the simplified BSD license.  See LICENSE for details.
 */

#include "unittest.h"
#include "../dict.c"

/*
 * Dictionary
 */

TEST(dict) {
    Dict *dict = make_string_dict();
    EQ(0, dict->nelem);
    String *k = to_string("abc");
    dict_put(dict, k, (void *)-1);
    EQ(1, dict->nelem);
    EQ(-1, (long) dict_get(dict, k));
    EQ(NULL, dict_get(dict, to_string("nonexistent")));

    // test rehashing
    for (int i = 0; i < DICT_INITIAL_SIZE * 2; i++) {
        char buf[] = "0123456789";
        sprintf(buf, "key%d", i);
        dict_put(dict, to_string(buf), (void *)(long)i);
    }
    EQ(1 + DICT_INITIAL_SIZE * 2, dict->nelem);
    for (int i = 0; i < DICT_INITIAL_SIZE * 2; i++) {
        char buf[] = "0123456789";
        sprintf(buf, "key%d", i);
        EQ(i, (int)(long)dict_get(dict, to_string(buf)));
    }

    // Store duplicate key
    dict_put(dict, k, (void *)-2);
    EQ(-2, (long) dict_get(dict, k));
    EQ(1 + DICT_INITIAL_SIZE * 2, dict->nelem);

    // Removal
    bool existed = dict_delete(dict, k);
    EQ(DICT_INITIAL_SIZE * 2, dict->nelem);
    EQ(NULL, dict_get(dict, k));
    EQ(true, existed);
    existed = dict_delete(dict, k);
    EQ(false, existed);

    // Address dictionary
    dict = make_address_dict();
    k = to_string("abc");
    dict_put(dict, k, (void *)-1);
    EQ(-1, (long) dict_get(dict, k));
    EQ(NULL, dict_get(dict, to_string("abc")));
}

TEST(test_dict_iter) {
    Dict *dict = make_string_dict();
    DictIter *iter = make_dict_iter(dict);
    EQ(NULL, dict_iter_next(iter));

    dict_put(dict, to_string("key"), (void *)-1);
    iter = make_dict_iter(dict);
    void **p = dict_iter_next(iter);
    EQ_STR(STRING_BODY((String *)to_string("key")), STRING_BODY((String *)p[0]));
    EQ(-1, (intptr)p[1]);
    EQ(NULL, dict_iter_next(iter));
}


