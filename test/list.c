#include "unittest.h"
#include "../list.c"

/*
 * List
 */

TEST(list) {
    List *list = make_list();
    EQ(0, LIST_LEN(list));

    list_push(list, (void *)17);
    list_push(list, (void *)42);
    EQ(2, (intptr)LIST_LEN(list));
    EQ(17, (intptr)LIST_REF(list, 0));
    EQ(42, (intptr)LIST_REF(list, 1));
    EQ(42, (intptr)list_pop(list));
    EQ(17, (intptr)list_pop(list));
    EQ(0, LIST_LEN(list));

    list_push(list, (void *)17);
    list_push(list, (void *)42);
    EQ(17, (intptr)list_unshift(list));
    EQ(42, (intptr)list_unshift(list));

    // list_reverse()
    List *list1 = make_list();
    list_push(list1, (void *)17);
    list_push(list1, (void *)42);
    List *rev = list_reverse(list1);
    EQ(42, (intptr)LIST_REF(rev, 0));
    EQ(17, (intptr)LIST_REF(rev, 1));
}

TEST(list_as_set) {
    List *a = make_list();
    List *b = make_list();
    list_push(a, to_string("abc"));
    list_push(a, to_string("def"));
    list_push(b, to_string("abc"));
    list_push(b, to_string("XYZ"));

    List *uni = list_union(a, b);
    EQ(3, LIST_LEN(uni));
    EQ(true, list_in(uni, to_string("abc")));
    EQ(true, list_in(uni, to_string("def")));
    EQ(true, list_in(uni, to_string("XYZ")));

    List *uni1 = list_union(a, make_list());
    EQ(2, LIST_LEN(uni1));
    List *uni2 = list_union(make_list(), a);
    EQ(2, LIST_LEN(uni2));

    List *uni3 = list_union1(a, to_string("123"));
    EQ(3, LIST_LEN(uni3));
    EQ(true, list_in(uni3, to_string("123")));

    List *uni4 = list_union1(a, to_string("abc"));
    EQ(2, LIST_LEN(uni4));

    List *intersect = list_intersect(a, b);
    EQ(1, LIST_LEN(intersect));
    EQ(true, list_in(uni, to_string("abc")));

    List *intersect1 = list_intersect(a, make_list());
    EQ(0, LIST_LEN(intersect1));
    List *intersect2 = list_intersect(make_list(), a);
    EQ(0, LIST_LEN(intersect2));
}

