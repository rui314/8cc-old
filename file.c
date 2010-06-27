/*
 * file.c - I/O implementation
 *
 *   Copyright 2010 Rui Ueyama <rui314@gmail.com>.  All rights reserved.
 *
 *   Redistribution and use in source and binary forms, with or without
 *   modification, are permitted provided that the following conditions are met:
 *
 *      1. Redistributions of source code must retain the above copyright
 *         notice, this list of conditions and the following disclaimer.
 *
 *      2. Redistributions in binary form must reproduce the above copyright
 *         notice, this list of conditions and the following disclaimer in the
 *         documentation and/or other materials provided with the distribution.
 *
 *   THIS SOFTWARE IS PROVIDED BY COPYRIGHT HOLDER ``AS IS'' AND ANY EXPRESS OR
 *   IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 *   OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN
 *   NO EVENT SHALL COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
 *   INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 *   (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 *   SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 *   CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 *   LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 *   OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
 *   DAMAGE.
 */

#include "8cc.h"

/*
 * A wrapper object for stdio's FILE.
 */

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

void unreadc(int c, File *file) {
    if (c == '\n') file->lineno--;
    if (file->ungotten != EOF)
        ungetc(file->ungotten, file->stream);
    file->ungotten = c;
}

/*
 * Abstracts end-of-line differences.  All sequences of "\r\n", "\n"
 * and "\r" are converted to '\n'.
 */
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
        unreadc(c, file);
        return '\n';
    }
    if (c == '\n') file->lineno++;
    return c;
}
