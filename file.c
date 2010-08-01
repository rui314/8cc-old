/*
 * file.c - I/O implementation
 *
 * Copyright 2010 Rui Ueyama <rui314@gmail.com>.  All rights reserved.
 * This code is available under the simplified BSD license.  See LICENSE for details.
 */

#include "8cc.h"

/*
 * A wrapper object for stdio's FILE.  You can pushback up to two characters to
 * a File.
 *
 * You can parse C source by peeking one character.  The only case I know of
 * where you have to peek two characters is %:%: (bigraph token ##).
 */

static void init_file(File *file, char *filename) {
    file->line = 1;
    file->column = 1;
    file->last_column = 0;
    file->filename = to_string(filename);
    file->ungotten[0] = EOF;
    file->ungotten[1] = EOF;
    file->eof_flag = false;
}

File *make_file(FILE *stream, char *filename) {
    File *r = malloc(sizeof(File));
    r->type = FILE_STDIO;
    r->stream = stream;
    init_file(r, filename);
    return r;
}

File *make_string_file(String *s) {
    ASSERT(STRING_BODY(s)[STRING_LEN(s)-1] == '\0');

    File *r = malloc(sizeof(File));
    r->type = FILE_STRING;
    r->buf = STRING_BODY(s);
    r->pos = 0;
    init_file(r, "-");
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

void close_file(File *file) {
    if (file->type == FILE_STDIO)
        fclose(file->stream);
}

void unreadc(int c, File *file) {
    if (c == '\0' || c == EOF)
        return;
    if (c == '\n') {
        file->line--;
        file->column = file->last_column;
    } else {
        file->column--;
    }
    if (file->ungotten[0] == EOF)
        file->ungotten[0] = c;
    else if (file->ungotten[1] == EOF)
        file->ungotten[1] = c;
    else
        panic("pushback buffer is full: '%c'", c);
    file->eof_flag = false;
}

/*
 * Returns the next character without consuming it.
 */
int peekc(File *file) {
    int c = readc(file);
    unreadc(c, file);
    return c;
}

/*
 * Consume next character iff the same as a given charcter.
 */
bool next_char_is(File *file, int c) {
    int c1 = readc(file);
    if (c == c1)
        return true;
    unreadc(c1, file);
    return false;
}

int readc_int(File *file) {
    if (file->eof_flag)
        return EOF;
    if (file->ungotten[1] != EOF) {
        int c = file->ungotten[1];
        file->ungotten[1] = EOF;
        return c;
    }
    if (file->ungotten[0] != EOF) {
        int c = file->ungotten[0];
        file->ungotten[0] = EOF;
        return c;
    }

    if (file->type == FILE_STDIO)
        return getc(file->stream);
    if (file->type == FILE_STRING)
        return file->buf[file->pos++];
    panic("unknown file type: %c", file->type);
}

static void next_line(File *file, int c) {
    file->line++;
    file->last_column = file->column;
    file->column = 1;
    if (c == '\r') {
        int c1 = readc_int(file);
        if (c1 != '\n')
            unreadc(c1, file);
    }
}

/*
 * Abstracts C source file.  This does two things:
 *
 *   - Converts "\r\n" or "\r" to "\n".
 **
 *   - Removes backslash and following end-of-line marker.  This needs
 *     to happen before preprocessing and before the lexical analysis
 *     of the C program.  (C:ARM p.13 2.1.2 Whitespace and Line
 *     Termination)
 */
int readc(File *file) {
    int c = readc_int(file);
    if (c == EOF || c == '\0') {
        file->eof_flag = true;
        return EOF;
    }
    if (c == '\\') {
        int c1 = readc_int(file);
        if (c1 == '\r' || c1 == '\n') {
            next_line(file, c1);
            return readc(file);
        }
        unreadc(c1, file);
        return c;
    }
    if (c == '\r' || c == '\n') {
        next_line(file, c);
        return '\n';
    }
    file->column++;
    return c;
}
