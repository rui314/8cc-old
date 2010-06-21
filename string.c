#include "8cc.h"

String *make_string(void) {
    String *obj = malloc(sizeof(String));
    obj->buf = malloc(STRING_INITIAL_SIZE);
    obj->nalloc = STRING_INITIAL_SIZE;
    obj->len = 0;
    return obj;
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
