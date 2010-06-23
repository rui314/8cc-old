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
    return STRING_LEN(a) == STRING_LEN(b)
        && !memcmp(STRING_BODY(a), STRING_BODY(b), STRING_LEN(a));
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
