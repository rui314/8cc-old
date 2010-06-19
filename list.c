#include "8cc.h"

List *make_list(void) {
    List *obj = malloc(sizeof(List));
    obj->elems = malloc(sizeof(void*) * LIST_INITIAL_SIZE);
    obj->nalloc = LIST_INITIAL_SIZE;
    obj->len = 0;
    return obj;
}

static void ensure_room(List *list) {
    if (list->len < list->nalloc) return;
    int newsize = list->nalloc * 2;
    void *buf = malloc(sizeof(void*) * newsize);
    memcpy(buf, list->elems, sizeof(void*) * list->len);
    list->elems = buf;
    list->nalloc = newsize;
}

void list_push(List *list, void *e) {
    ensure_room(list);
    list->elems[list->len++] = e;
}
