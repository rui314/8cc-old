#include "unittest.h"
#include "../string.c"

static int64_t qword(char *str) {
    int64_t r = 0;
    for (int i = strlen(str) - 1; i >= 0; i--)
        r = (r << 8) | str[i];
    return r;
}

TEST(string) {
    String *b = make_string();
    NOT_NULL(b);
    EQ(STRING_LEN(b), 0);

    o1(b, 'a');
    EQ(STRING_LEN(b), 1);
    EQ(STRING_BODY(b)[0], 'a');

    o8(b, qword("bcdefghi"));
    EQ(STRING_LEN(b), 9);
    EQ(strcmp(STRING_BODY(b), "abcdefghi"), 0);
}
