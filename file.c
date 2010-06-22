#include "8cc.h"

void error(char *format, ...) {
    va_list ap;
    va_start(ap, format);
    fprintf(stderr, "8cc: ");
    vfprintf(stderr, format, ap);
    fprintf(stderr, "\n");
    exit(-1);
}

File *make_file(FILE *stream, char *filename) {
    File *r = malloc(sizeof(File));
    r->stream = stream;
    r->lineno = 1;
    r->filename = make_string();
    ostr(r->filename, filename);
    r->ungotten = EOF;
    return r;
}

File *open_file(char *path) {
    if (!strcmp(path, "-")) {
        return make_file(stdin, "-");
    }
    FILE *stream = fopen(path, "r");
    if (stream == NULL) {
        perror("fopen failed: ");
        exit(-1);
    }
    return make_file(stream, path);
}

static void unreadc_int(int c, File *file) {
    if (file->ungotten != EOF)
        ungetc(file->ungotten, file->stream);
    file->ungotten = c;
}

void unreadc(int c, File *file) {
    if (c == '\r' || c == '\n') file->lineno--;
    unreadc_int(c, file);
}

int readc(File *file) {
    int c;
    if (file->ungotten == EOF) {
        c = getc(file->stream);
    } else {
        c = file->ungotten;
        file->ungotten = EOF;
    }
    if (c == EOF || c == '\0')
        return EOF;
    if (c == '\r') {
        file->lineno++;
        c = getc(file->stream);
        if (c == '\n') return '\n';
        unreadc_int(c, file);
        return '\n';
    }
    if (c == '\n') file->lineno++;
    return c;
}
