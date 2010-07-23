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

/*
 * There are two classes of tokens during translation: "preprocessing tokens"
 * and "tokens".  C source files are parsed into preprocessing tokens first, C
 * preprocessor handles preprecessing directives and macro replacement, and then
 * the preprocessing tokens are converted to tokens.  Main C compiler sees only
 * the sequence of tokens.
 *
 * The functions in this file does parses C source file into sequence of
 * preprocessor token.  Preprocessor token is defined as the below (WG14/N1256
 * 6.4):
 *
 * preprocessing-token:
 *     header-name
 *     identifier
 *     pp-number
 *     character-constant
 *     string-literal
 *     punctuator
 *     each non-white-space character that cannot be one of the above
 */

#include "8cc.h"

/*==============================================================================
 * Functions to make CPP processing context.
 */

CppContext *make_cpp_context(File *file) {
    CppContext *r = malloc(sizeof(CppContext));
    r->file = file;
    r->at_bol = true;
    r->file_stack = make_list();
    r->defs = make_string_dict();
    r->ungotten = make_list();
    r->in_macro = false;
    r->incl = make_list();
    define_predefined_macros(r);
    return r;
}

void do_include(CppContext *ctx, String *path) {
    list_push(ctx->file_stack, ctx->file);
    ctx->file = open_file(STRING_BODY(path));
}

/*==============================================================================
 * Utility functions.
 */

static bool iswhitespace(int c) {
    return c == ' ' || c == '\t' || c == '\f' || c == '\v';
}

ATTRIBUTE((noreturn)) void error_cpp_ctx(CppContext *ctx, char *msg, ...) {
    va_list ap;
    va_start(ap, msg);
    print_parse_error(ctx->file->line, ctx->file->column, msg, ap);
}

void unget_cpp_token(CppContext *ctx, Token *tok) {
    if (tok)
        list_push(ctx->ungotten, tok);
}

Token *peek_cpp_token(CppContext *ctx) {
    Token *tok = read_cpp_token(ctx);
    unget_cpp_token(ctx, tok);
    return tok;
}

static bool next_two_chars(CppContext *ctx, char c0, char c1) {
    int v0 = readc(ctx->file);
    if (c0 != v0) {
        unreadc(v0, ctx->file);
        return false;
    }
    int v1 = readc(ctx->file);
    if (c1 != v1) {
        unreadc(v1, ctx->file);
        unreadc(v0, ctx->file);
        return false;
    }
    return true;
}

/*==============================================================================
 * Functions to make tokens.
 */

Token *copy_token(Token *tok) {
    Token *r = malloc(sizeof(Token));
    r->toktype = tok->toktype;
    r->val = tok->val;
    r->line = tok->line;
    r->column = tok->column;
    r->hideset = tok->hideset;
    r->space = tok->space;
    return r;
}

Token *make_token(CppContext *ctx, TokType type, TokenValue val) {
    Token *r = malloc(sizeof(Token));
    r->toktype = type;
    r->val = val;
    if (ctx->file) {
        r->line = ctx->file->line;
        r->column = ctx->file->column;
    } else {
        r->line = r->column = 0;
    }
    r->hideset = make_list();
    r->space = false;
    return r;
}

static Token *make_ident(CppContext *ctx, String *val) {
    return make_token(ctx, TOKTYPE_IDENT, (TokenValue)val);
}

Token *make_cppnum(CppContext *ctx, String *val) {
    return make_token(ctx, TOKTYPE_CPPNUM, (TokenValue)val);
}

static Token *make_punct(CppContext *ctx, int c) {
    return make_token(ctx, TOKTYPE_PUNCT, (TokenValue)c);
}

static Token *make_char_const(CppContext *ctx, char c) {
    return make_token(ctx, TOKTYPE_CHAR, (TokenValue)(int)c);
}

Token *make_str_literal(CppContext *ctx, String *val) {
    return make_token(ctx, TOKTYPE_STRING, (TokenValue)val);
}

static Token *make_cpp_token(CppContext *ctx, TokType type) {
    return make_token(ctx, type, (TokenValue)0);
}

/*==============================================================================
 * Functions to handle comments.  Comment will be treated as if it were one
 * whitespace character.  (See WG14/N1256 5.1.1.2 Translation phases, phase 4)
 */
static void skip_comment(CppContext *ctx) {
    int prev = '\0';
    for (;;) {
        int c = readc(ctx->file);
        if (c == EOF)
            error_cpp_ctx(ctx, "premature end of input file in comment");
        if (c == '/' && prev == '*')
            return;
        prev = c;
    }
}

static void skip_line_comment(CppContext *ctx) {
    for (;;) {
        int c = readc(ctx->file);
        if (c == EOF) return;
        if (c == '\n') {
            unreadc(c, ctx->file);
            return;
        }
    }
}

/*==============================================================================
 * Parser.  These methods reads source file and returns preprocessing tokens.
 */

/*
 * WG14/N1256 6.4.8 Preprocessing numbers
 *
 * pp-number
 *     digit
 *     . digit
 *     pp-number digit
 *     pp-number identifier-nondigit
 *     pp-number [eEpP] sign
 *     pp-number .
 *
 * (WG14/N1256 6.4.2 Identifiers)
 * identifier-nondigit:
 *     nondigit
 *     universal-character-name
 *     other implementation-defined characters
 * nondigit:
 *     [_a-zA-Z]
 */
static Token *read_cppnum(CppContext *ctx, int c) {
    // c must be [0-9] or '.'
    String *buf = make_string();
    o1(buf, c);
    if (c == '.') {
        int c1 = readc(ctx->file);
        if (!isdigit(c1))
            error_cpp_ctx(ctx, "number expected, but got '%c'", c1);
        o1(buf, c1);
    }
    for (;;) {
        c = readc(ctx->file);
        if (isalnum(c) || c == '_' || c == '.') {
            o1(buf, c);
        } else if (c == 'e' || c == 'E' || c == 'p' || c == 'P') {
            o1(buf, c);
            int c1 = readc(ctx->file);
            if (c1 != '+' && c1 != '-')
                error_cpp_ctx(ctx, "'+' or '-' expected, but got '%c'", c1);
            o1(buf, c1);
        } else {
            unreadc(c, ctx->file);
            return make_cppnum(ctx, buf);
        }
    }
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
        if (isxdigit(c))
            r = r * 16 + hextodec(c);
        else
            unreadc(c, file);
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
static String *read_str(CppContext *ctx) {
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
            error_cpp_ctx(ctx, "premature end of input file while reading a literal string");
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
static char read_char(CppContext *ctx) {
    int c = readc(ctx->file);
    if (c == EOF)
        error_cpp_ctx(ctx, "premature end of input file while reading a literal character");
    char r = (c != '\\') ? c : read_escape_char(ctx->file);
    c = readc(ctx->file);
    if (c != '\'')
        error_cpp_ctx(ctx, "'\'' (single quote) expected, but got '%c'", c);
    return r;
}

static String *read_ident(File *file, char c0) {
    String *b = make_string();
    o1(b, c0);
    for (;;) {
        int c1 = readc(file);
        if (isalnum(c1) || c1 == '_') {
            o1(b, c1);
        } else {
            unreadc(c1, file);
            return b;
        }
    }
}

static void skip_whitespace(CppContext *ctx) {
    int c = readc(ctx->file);
    while (iswhitespace(c))
        c = readc(ctx->file);
    unreadc(c, ctx->file);
}

/*
 * Reads operators such as += or *=.
 */
static Token *maybe_read_equal(CppContext *ctx, int t0, int t1) {
    if (next_char_is(ctx->file, '='))
        return make_punct(ctx, t1);
    return make_punct(ctx, t0);
}

/*
 * Reads operators such as ++ or --.
 */
static Token *maybe_read_rep0(CppContext *ctx, int t0, int t1, int t2) {
    if (next_char_is(ctx->file, t0))
        return make_punct(ctx, t2);
    return maybe_read_equal(ctx, t0, t1);
}

/*
 * Reads operators such as <<= or >>=.
 */
static Token *maybe_read_rep1(CppContext *ctx, int t0, int t1, int t2, int t3) {
    if (next_char_is(ctx->file, t0))
        return maybe_read_equal(ctx, t2, t3);
    return maybe_read_equal(ctx, t0, t1);
}

/*
 * Returns the next token.  This considers a squence of whitespace characters a
 * token, unlike read_cpp_token().
 */
static Token *read_cpp_token_int(CppContext *ctx) {
    if (!LIST_IS_EMPTY(ctx->ungotten))
        return list_pop(ctx->ungotten);
    if (ctx->in_macro)
        return NULL;

    for (;;) {
        int c = readc(ctx->file);
        switch (c) {
        case ' ': case '\t': case '\f': case '\v':
            skip_whitespace(ctx);
            return make_cpp_token(ctx, TOKTYPE_SPACE);
        case '\n':
            return make_cpp_token(ctx, TOKTYPE_NEWLINE);
        case '/':
            if (next_char_is(ctx->file, '*')) {
                skip_comment(ctx);
                return make_cpp_token(ctx, TOKTYPE_SPACE);
            }
            if (next_char_is(ctx->file, '/')) {
                skip_line_comment(ctx);
                return make_cpp_token(ctx, TOKTYPE_SPACE);
            }
            return maybe_read_equal(ctx, '/', KEYWORD_A_DIV);
        case '#':
            if (next_char_is(ctx->file, '#'))
                return make_punct(ctx, KEYWORD_TWOSHARPS);
            return make_punct(ctx, '#');
        case '.': {
            if (next_two_chars(ctx, '.', '.'))
                return make_punct(ctx, KEYWORD_THREEDOTS);
            if (isdigit(peekc(ctx->file)))
                return read_cppnum(ctx, c);
            return make_punct(ctx, '.');
        }
        case '0': case '1': case '2': case '3': case '4':
        case '5': case '6': case '7': case '8': case '9':
            return read_cppnum(ctx, c);
        case '"':
            return make_str_literal(ctx, read_str(ctx));
        case '\'':
            return make_char_const(ctx, read_char(ctx));

        case 'a': case 'b': case 'c': case 'd': case 'e': case 'f': case 'g':
        case 'h': case 'i': case 'j': case 'k': case 'l': case 'm': case 'n':
        case 'o': case 'p': case 'q': case 'r': case 's': case 't': case 'u':
        case 'v': case 'w': case 'x': case 'y': case 'z': case 'A': case 'B':
        case 'C': case 'D': case 'E': case 'F': case 'G': case 'H': case 'I':
        case 'J': case 'K': case 'L': case 'M': case 'N': case 'O': case 'P':
        case 'Q': case 'R': case 'S': case 'T': case 'U': case 'V': case 'W':
        case 'X': case 'Y': case 'Z': case '_':
            return make_ident(ctx, read_ident(ctx->file, c));

        case '=': return maybe_read_equal(ctx, '=', KEYWORD_EQ);
        case '*': return maybe_read_equal(ctx, '*', KEYWORD_A_MUL);
        case '^': return maybe_read_equal(ctx, '^', KEYWORD_A_XOR);
        case '!': return maybe_read_equal(ctx, '!', KEYWORD_NE);

        case '+': return maybe_read_rep0(ctx, '+', KEYWORD_A_ADD, KEYWORD_INC);
        case '&': return maybe_read_rep0(ctx, '&', KEYWORD_A_AND, KEYWORD_LOG_AND);
        case '|': return maybe_read_rep0(ctx, '|', KEYWORD_A_OR,  KEYWORD_LOG_OR);

        case '-':
            if (next_char_is(ctx->file, '>'))
                return make_punct(ctx, KEYWORD_ARROW);
            return maybe_read_rep0(ctx, '-', KEYWORD_A_SUB, KEYWORD_DEC);

            /*
             * The following six tokens (so-called "digraphs")
             *   <: :> <% %> %: %:%:
             * are equivalent to the following six tokens.
             *   [   ]  {  }  #  ##
             * (WG14/N1256 6.4.6 Punctuators, paragraph 3)
             */
        case '<':
            if (next_char_is(ctx->file, ':'))
                return make_punct(ctx, '[');
            if (next_char_is(ctx->file, '%'))
                return make_punct(ctx, '{');
            return maybe_read_rep1(ctx, '<', KEYWORD_LE, KEYWORD_LSH, KEYWORD_A_LSH);
        case ':':
            if (next_char_is(ctx->file, '>'))
                return make_punct(ctx, ']');
            return make_punct(ctx, ':');
        case '%':
            if (next_char_is(ctx->file, '>'))
                return make_punct(ctx, '}');
            if (next_char_is(ctx->file, ':')) {
                if (next_two_chars(ctx, '%', ':'))
                    return make_punct(ctx, KEYWORD_TWOSHARPS);
                return make_punct(ctx, '#');
            }
            return maybe_read_equal(ctx, '%', KEYWORD_A_MOD);

        case '>':
            return maybe_read_rep1(ctx, '>', KEYWORD_GE, KEYWORD_RSH, KEYWORD_A_RSH);

        case '(': case ')': case ',': case ';': case '[': case ']':
        case '{': case '}': case '?': case '~':
            return make_punct(ctx, c);
        case EOF:
            return NULL;
        default:
            error_cpp_ctx(ctx, "unimplemented '%c'", c);
        }
    }
}

/*==============================================================================
 * Preprocessor conditional inclusion.
 *
 * If a condition of #if, #ifdef, #ifndef or #elif is false, we don't actually
 * need to parse the subsequent tokens until #elif or #endif appears.
 * skip_cond_incl() skips parsing functions and directly reads characters from
 * file.  As the function toches the file rather than parsed tokens, this is
 * defeined in this file rather than in cpp.c.
 */

static void skip_char(CppContext *ctx) { read_char(ctx); }
static void skip_string(CppContext *ctx) { read_str(ctx); }

void skip_line(CppContext *ctx) {
    for (;;) {
        int c = readc(ctx->file);
        if (c == '\n')
            return;
        if (c == EOF)
            error_cpp_ctx(ctx, "unterminated conditional inclusion");
        if (c == '\'')
            skip_char(ctx);
        else if (c == '"')
            skip_string(ctx);
    }
}

CondInclType skip_cond_incl(CppContext *ctx) {
    assert(LIST_IS_EMPTY(ctx->ungotten));
    int nest = 0;
    for (;;) {
        skip_whitespace(ctx);
        if (!next_char_is(ctx->file, '#')) {
            skip_line(ctx);
            continue;
        }
        Token *tok = read_cpp_token(ctx);
        if (tok->toktype == TOKTYPE_NEWLINE)
            continue;
        if (tok->toktype != TOKTYPE_IDENT) {
            skip_line(ctx);
            continue;
        }
        if (!strcmp(STRING_BODY(tok->val.str), "if")
            || !strcmp(STRING_BODY(tok->val.str), "ifdef")
            || !strcmp(STRING_BODY(tok->val.str), "ifndef")) {
            nest++;
        } else if (!nest && !strcmp(STRING_BODY(tok->val.str), "else")) {
            expect_newline(ctx);
            return COND_ELSE;
        } else if (!nest && !strcmp(STRING_BODY(tok->val.str), "elif")) {
            return COND_ELIF;
        } else if (!strcmp(STRING_BODY(tok->val.str), "endif")) {
            if (nest) {
                nest--;
            } else {
                expect_newline(ctx);
                return COND_ENDIF;
            }
        }
        skip_line(ctx);
    }
}

/*==============================================================================
 * WG14/N1256 6.4.7 Header names
 *
 * #include directive needs special tokenize to read a token in <> or "".
 *
 * header-name:
 *     < h-char-sequence >
 *     " q-char-sequence "
 * h-char-sequence:
 *     [^>\n]+
 * q-char-sequence:
 *     [^"\n]+
 */

String *read_header_name(CppContext *ctx, bool *std) {
    skip_whitespace(ctx);
    char close;
    int c = readc(ctx->file);
    if (c == '"') {
        *std = false;
        close = '"';
    } else if (c == '<') {
        *std = true;
        close = '>';
    } else {
        unreadc(c, ctx->file);
        return NULL;
    }

    String *r = make_string();
    for (;;) {
        c = readc(ctx->file);
        if (c == EOF || c == '\n')
            error_cpp_ctx(ctx, "premature end of header name");
        if (c == close)
            break;
        o1(r, c);
    }
    if (STRING_LEN(r) == 0)
        error_cpp_ctx(ctx, "header name should not be empty");
    o1(r, '\0');
    return r;
}

/*==============================================================================
 * Public interfaces to be used by the preprocessor.
 */

/*
 * Returns iff the next token a next token is immediately preceded by
 * whitespace.  Token is not consumed.
 *
 * This function will be used only when the preprocessor reads #define
 * directive, which is the only place where the parser needs to be aware whether
 * whitespace exists between tokens or not.  For example, the following macro
 * FOO is function-like, that takes an argument named "x".
 *
 *   #define FOO(x) ...
 *
 * On the other hand, macro BAR shown below is not function-like and will be
 * expanded to "(x) ...".
 *
 *   #define BAR (x) ...
 */
bool is_next_space(CppContext *ctx) {
    Token *tok = read_cpp_token_int(ctx);
    unget_cpp_token(ctx, tok);
    return tok->toktype == TOKTYPE_SPACE;
}

/*
 * Returns the next token.
 */
Token *read_cpp_token(CppContext *ctx) {
    Token *tok = read_cpp_token_int(ctx);
    while (tok && tok->toktype == TOKTYPE_SPACE) {
        tok = read_cpp_token_int(ctx);
        tok->space = true;
    }
    if (!tok && !LIST_IS_EMPTY(ctx->file_stack)) {
        close_file(ctx->file);
        ctx->file = list_pop(ctx->file_stack);
        return read_cpp_token(ctx);
    }
    return tok;
}
