#include "8cc.h"

#define NOT_NULL(p) do { if (!(p)) error("Line %d: must not be null " #p, __LINE__); } while (0)
#define EQ(x, y) do { if ((x) != (y)) error("Line %d: must be the same", __LINE__); } while (0)

/*
 * String
 */

int64_t qword(char *str) {
    int64_t r = 0;
    for (int i = strlen(str) - 1; i >= 0; i--)
	r = (r << 8) | str[i];
    return r;
}

void testString(void) {
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

int main(int argc, char **argv) {
    testString();
    printf("OK\n");
    return 0;
}
