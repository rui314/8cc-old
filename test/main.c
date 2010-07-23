#include "unittest.h"

List* test_funcs;

void eq_str(int line, char *expected, char *got) {
    if (strcmp(expected, got)) {
        error("line %d: \"%s\" expected, but got \"%s\"", line, expected, got);
    }
}

void eq_char(int line, int expected, int got) {
    if (expected != got) {
        error("line %d: '%c' expected, but got '%c'", line, expected, got);
    }
}

FILE *create_file(char *content) {
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

/*
 * Entry point
 */

RUN_TESTS()
