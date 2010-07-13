/*
 * lex.c - lexical analyzer
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

static Dict *reserved_word;

static Token *make_token(ReadContext *ctx) {
    Token *r = malloc(sizeof(Token));
    r->toktype = TOKTYPE_INVALID;
    r->line = ctx->file->line;
    r->column = ctx->file->column;
    return r;
}

static Token *make_token1(ReadContext *ctx, TokType toktype, TokenValue val) {
    Token *r = malloc(sizeof(Token));
    r->toktype = toktype;
    r->val = val;
    r->line = ctx->file->line;
    r->column = ctx->file->column;
    return r;
}

static Token *make_keyword(ReadContext *ctx, int k) {
    Token *r = malloc(sizeof(Token));
    r->toktype = TOKTYPE_KEYWORD;
    r->val.k = k;
    r->line = ctx->file->line;
    r->column = ctx->file->column;
    return r;
}

static void skip_comment(File *file) {
    int line = file->line;
    int column = file->column;
    int prev = '\0';
    for (;;) {
        int c = readc(file);
        if (c == EOF)
            error("Line %d:%d: premature end of input file in comment", line, column);
        if (c == '/' && prev == '*')
            return;
        prev = c;
    }
}

static void skip_line_comment(File *file) {
    for (;;) {
        int c = readc(file);
        if (c == EOF) return;
        if (c == '\n') {
            unreadc(c, file);
            return;
        }
    }
}

/*
 * integer-constant:
 *     decimal-constant integer-suffix?
 *     octal-constant integer-suffix?
 *     hexadecimal-constant integer-suffix?
 *
 * decimal-constant:
 *     [1-9] [0-9]*
 *
 * octal-constant:
 *     "0" [0-7]*
 *
 * hexadecimal-constant
 *     "0" [xX] [0-9a-fA-F]+
 *
 * integer-suffix:
 *     long-suffix unsigned-suffix?
 *     long-long-suffix unsigned-suffix?
 *     unsigned-suffix long-suffix?
 *     unsigned-suffix long--longsuffix?
 *
 * long-suffix;
 *     "l"
 *     "L"
 *
 * long-long-suffix:
 *     "ll"
 *     "LL"
 *
 * unsigned-suffix:
 *     "u"
 *     "U"
 *
 * floating-constant:
 *     decimal-floating-constant
 *     hexadecimal-floating-constant
 *
 * decimal-floating-constant:
 *     digit-sequence exponent? floating-suffix?
 *
 * hexadecimal-floating-constant:
 *     hex-prefix dotted-hex-digits binary-exponent floating-suffix?
 *     hex-prefix hex-digit-sequence binary-exponent floating-suffix?
 *
 * hex-prefix:
 *     0x
 *     0X
 *
 * dotted-hex-digits:
 *     hex-digit-sequence "."
 *     hex-digit-sequence "." hex-digit-sequence
 *     "." hex-digit-sequence
 *
 * exponent:
 *     [eE] [-+]? digit-sequence
 *
 * binary-exponent:
 *     [pP] [-+]? digit-sequence
 *
 * floating-suffix:
 *     [fFlL]
 *
 * digit-sequence:
 *     [0-9]+
 *
 * hex-digit-sequence:
 *     [0-9a-fA-F]+
 */
static Token *read_num(ReadContext *ctx) {
    Token *tok = make_token(ctx);
    String *buf = make_string();
    for (;;) {
        int c = readc(ctx->file);
        if (c == EOF) {
            goto ret_int;
        } else if ('0' <= c && c <= '9') {
            o1(buf, c);
        } else if (c == '.') {
            o1(buf, c);
            break;
        } else {
            unreadc(c, ctx->file);
            goto ret_int;
        }
    }
    for (;;) {
        int c = readc(ctx->file);
        if (c == EOF) {
            goto ret_float;
        } else if ('0' <= c && c <= '9') {
            o1(buf, c);
        } else {
            unreadc(c, ctx->file);
            goto ret_float;
        }
    }
 ret_int:
    tok->toktype = TOKTYPE_INT;
    tok->val.i = atoi(STRING_BODY(buf));
    return tok;
 ret_float:
    tok->toktype = TOKTYPE_FLOAT;
    tok->val.f = atof(STRING_BODY(buf));
    return tok;
}

static int hextodec(char c) {
    c = tolower(c);
    if ('0' <= c && c <= '9')
        return c - '0';
    return c - 'a' + 10;
}

/*
 * escape-character:
 *     "\" escape-code
 *     universal-character-name
 *
 * escape-code:
 *     character-escape-code
 *     octal-escape-code
 *     hex-escape-code
 *
 * character-escape-code:
 *     one of: n t b r f v \ ' " a ? e
 *     ('\e' is GNU extension)
 *
 * octal-escape-code:
 *     [0-7]{,3}
 *
 * hex-escape-code:
 *     "x" [0-9a-fA-F]+
 *
 * universal-character-name:
 *     "\u" [0-9a-fA-F]{4}
 *     "\U" [0-9a-fA-F]{8}
 */
static char read_escape_char(File *file) {
    int c = readc(file);
    int r;
    switch (c) {
    case EOF:
        error("line %d:%d: premature end of input file while reading a literal string or a character", file->line, file->column);
    case 'a': return '\a';
    case 'b': return '\b';
    case 'e': return '\033'; // GNU extension
    case 't': return '\t';
    case 'n': return '\n';
    case 'v': return '\v';
    case 'f': return '\f';
    case 'r': return '\r';
    case '0': case '1': case '2': case '3': case '4':
    case '5': case '6': case '7': case '8': case '9':
        r = c - '0';
        c = readc(file);
        if (isdigit(c)) {
            r = r * 8 + (c - '0');
            c = readc(file);
            if (isdigit(c)) {
                r = r * 8 + (c - '0');
            } else {
                unreadc(c, file);
            }
        } else {
            unreadc(c, file);
        }
        return r;
    case 'x':
        c = readc(file);
        if (!isxdigit(c))
            error("line %d:%d: hexdigit expected, but got '%c'", file->line, file->column, c);
        r = hextodec(c);
        c = readc(file);
        if (isxdigit(c)) {
            r = r * 16 + hextodec(c);
        } else {
            unreadc(c, file);
        }
        return r;
    default: return (char)c;
    }
}

/*
 * string-constant:
 *     '"' s-char* '"'
 *     'L"' s-char* '"'
 *
 * s-char:
 *    any source character except the double quote, backslash or newline
 *    escape-character
 */
static String *read_str(ReadContext *ctx) {
    String *b = make_string();
    for (;;) {
        int c = readc(ctx->file);
        switch (c) {
        case '"':
            o1(b, '\0');
            return b;
        case '\\':
            o1(b, read_escape_char(ctx->file));
            break;
        case EOF:
            error_ctx(ctx, "premature end of input file while reading a literal string");
        default:
            o1(b, c);
        }
    }
}

/*
 * character-constant:
 *     "'" c-char* "'"
 *     "L'" c-char* "'"
 *
 * c-char:
 *    any source character except the single quote, backslash or newline
 *    escape-character
 */
static char read_char(ReadContext *ctx) {
    int c = readc(ctx->file);
    switch (c) {
    case EOF:
        error_ctx(ctx, "premature end of input file while reading a literal character");
    case '\\': return read_escape_char(ctx->file);
    default: return (char)c;
    }
}

static String *read_word(File *file, char c0) {
    String *b = make_string();
    o1(b, c0);
    for (;;) {
        int c = readc(file);
        if (isalnum(c) || c == '_') {
            o1(b, c);
        } else if (c != EOF) {
            unreadc(c, file);
            return b;
        } else {
            return b;
        }
    }
}

Token *read_token(ReadContext *ctx) {
    if (!LIST_IS_EMPTY(ctx->ungotten))
        return list_pop(ctx->ungotten);

    String *str;
    for (;;) {
        int c = readc(ctx->file);
        int c1;
        switch (c) {
        case ' ': case '\t': case '\r': case '\n':
            continue;
        case '0': case '1': case '2': case '3': case '4':
        case '5': case '6': case '7': case '8': case '9':
            unreadc(c, ctx->file);
            return read_num(ctx);
        case '"':
            return make_token1(ctx, TOKTYPE_STRING, (TokenValue)read_str(ctx));
        case '\'': {
            Token *r = make_token1(ctx, TOKTYPE_CHAR, (TokenValue)read_char(ctx));
            c1 = read_char(ctx);
            if (c1 != '\'')
                error_ctx(ctx, "single quote expected, but got %c", c1);
            return r;
        }
        case 'a': case 'b': case 'c': case 'd': case 'e': case 'f': case 'g':
        case 'h': case 'i': case 'j': case 'k': case 'l': case 'm': case 'n':
        case 'o': case 'p': case 'q': case 'r': case 's': case 't': case 'u':
        case 'v': case 'w': case 'x': case 'y': case 'z': case 'A': case 'B':
        case 'C': case 'D': case 'E': case 'F': case 'G': case 'H': case 'I':
        case 'J': case 'K': case 'L': case 'M': case 'N': case 'O': case 'P':
        case 'Q': case 'R': case 'S': case 'T': case 'U': case 'V': case 'W':
        case 'X': case 'Y': case 'Z': case '_':
            str = read_word(ctx->file, c);
            int id = (intptr)dict_get(reserved_word, str);
            if (id)
                return make_keyword(ctx, id);
            return make_token1(ctx, TOKTYPE_IDENT, (TokenValue)str);
        case '=':
            if (next_char_is(ctx->file, '='))
                return make_keyword(ctx, KEYWORD_EQ);
            return make_keyword(ctx, '=');
        case '/':
            if (next_char_is(ctx->file, '*')) {
                skip_comment(ctx->file);
                return read_token(ctx);
            }
            if (next_char_is(ctx->file, '/')) {
                skip_line_comment(ctx->file);
                return read_token(ctx);
            }
            goto read_equal;
        case '<':
            if (next_char_is(ctx->file, '<'))
                c = KEYWORD_LSH;
            goto read_equal;
        case '>':
            if (next_char_is(ctx->file, '>'))
                c = KEYWORD_RSH;
            goto read_equal;
        case '+':
            if (next_char_is(ctx->file, '+'))
                return make_keyword(ctx, KEYWORD_INC);
            goto read_equal;
        case '-':
            if (next_char_is(ctx->file, '-'))
                return make_keyword(ctx, KEYWORD_DEC);
            goto read_equal;
        case '&':
            if (next_char_is(ctx->file, '&'))
                return make_keyword(ctx, KEYWORD_LOG_AND);
            goto read_equal;
        case '|':
            if (next_char_is(ctx->file, '|'))
                return make_keyword(ctx, KEYWORD_LOG_OR);
            goto read_equal;
        case '*': case '%': case '~': case '^': case '!':
        read_equal:
            if (next_char_is(ctx->file, '=')) {
                int k = c == '+' ? KEYWORD_A_ADD
                    : c == '-' ? KEYWORD_A_SUB
                    : c == '*' ? KEYWORD_A_MUL
                    : c == '/' ? KEYWORD_A_DIV
                    : c == '%' ? KEYWORD_A_MOD
                    : c == '&' ? KEYWORD_A_AND
                    : c == '|' ? KEYWORD_A_OR
                    : c == '^' ? KEYWORD_A_XOR
                    : c == '<' ? KEYWORD_LE
                    : c == '>' ? KEYWORD_GE
                    : c == '!' ? KEYWORD_NE
                    : c == KEYWORD_LSH ? KEYWORD_A_LSH
                    : c == KEYWORD_RSH ? KEYWORD_A_RSH
                    : panic("unknown op: %c", c);
                return make_keyword(ctx, k);
            }
            // FALL THROUGH
        case '(': case ')': case ',': case ';': case '[': case ']':
        case '{': case '}': case ':': case '?':
            return make_keyword(ctx, c);
        case EOF:
            return NULL;
        default:
            error_ctx(ctx, "unimplemented '%c'", c);
        }
    }
    return NULL;
}

/*============================================================
 * Initializer
 */

void lexer_init(void) {
    if (reserved_word != NULL)
        return;
    reserved_word = make_string_dict();
#define KEYWORD(id_, str_) \
    dict_put(reserved_word, to_string(str_), (void *)id_);
#define OP(_)
# include "keyword.h"
#undef OP
#undef KEYWORD
}
