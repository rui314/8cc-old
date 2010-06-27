#include "8cc.h"

void error(char *format, ...) {
    va_list ap;
    va_start(ap, format);
    fprintf(stderr, "8cc: ERROR: ");
    vfprintf(stderr, format, ap);
    fprintf(stderr, "\n");
    exit(-1);
}

void warn(char *format, ...) {
    va_list ap;
    va_start(ap, format);
    fprintf(stderr, "8cc: ");
    vfprintf(stderr, format, ap);
    fprintf(stderr, "\n");
}
