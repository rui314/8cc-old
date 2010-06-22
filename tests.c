#include "8cc.h"

#define NOT_NULL(p) do { if (!(p)) error("line %d: must not be null " #p, __LINE__); } while (0)
#define EQ(x, y) do { if ((x) != (y)) error("line %d: must be the same", __LINE__); } while (0)
#define EQ_CHAR(x, y) do { eq_char(__LINE__, (x), (y)); } while (0)
#define EQ_STR(x, y)  do { eq_str(__LINE__, (x), (y)); } while (0)

void eq_str(int line, char *expected, char *got) {
    if (strcmp(expected, got)) {
	error("line %d: \"%s\" expected, but got \"%s\"", line, expected, got);
    }
}

void eq_char(int line, int expected, int got) {
    if (expected != got) {
	error("line %d: '%d' expected, but got '%d'", line, expected, got);
    }
}

/*
 * String
 */

int64_t qword(char *str) {
    int64_t r = 0;
    for (int i = strlen(str) - 1; i >= 0; i--)
	r = (r << 8) | str[i];
    return r;
}

void test_string(void) {
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

static FILE *create_file(char *content) {
    char tmpl[] = "tmpXXXXXX";
    int fd = mkstemp(tmpl);
    if (fd < 0) {
	perror("fd: ");
	exit(-1);
    }
    unlink(tmpl);
    write(fd, content, strlen(content));
    lseek(fd, 0, SEEK_SET);
    return fdopen(fd, "r");
}

void test_file(void) {
    char *data = "ab\nc\r\r\nd\r";

    FILE *stream = create_file(data);
    File *file = make_file(stream, "pipe");
    EQ_STR(getfilename(file), "pipe");

    EQ(1, getfileline(file));
    EQ_CHAR('a', readc(file));
    EQ_CHAR('b', readc(file));
    EQ_CHAR('\n', readc(file));
    EQ(2, getfileline(file));
    EQ_CHAR('c', readc(file));
    EQ_CHAR('\n', readc(file));
    EQ(3, getfileline(file));
    EQ_CHAR('\n', readc(file));
    EQ(4, getfileline(file));
    EQ_CHAR('d', readc(file));
    EQ_CHAR('\n', readc(file));
    EQ(5, getfileline(file));
    EQ_CHAR(EOF, readc(file));
}

int main(int argc, char **argv) {
    test_string();
    test_file();
    printf("OK\n");
    return 0;
}
