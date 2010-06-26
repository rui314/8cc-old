#include "8cc.h"

static Token *make_token(int toktype, int lineno) {
    Token *r = malloc(sizeof(Token));
    r->toktype = toktype;
    r->lineno = lineno;
    return r;
}

ReadContext *make_read_context(Elf *elf) {
    ReadContext *r = malloc(sizeof(ReadContext));
    r->elf = elf;
    r->scope = make_list();
    return r;
}

static Token *read_num(File *file, char first, int lineno) {
    Token *tok;
    String *buf = make_string();
    o1(buf, first);
    for (;;) {
        int c = readc(file);
        if (c == EOF) {
            goto ret_int;
        } else if ('0' <= c && c <= '9') {
            o1(buf, c);
        } else if (c == '.') {
            o1(buf, c);
            break;
        } else {
            unreadc(c, file);
            goto ret_int;
        }
    }
    for (;;) {
        int c = readc(file);
        if (c == EOF) {
            goto ret_float;
        } else if ('0' <= c && c <= '9') {
            o1(buf, c);
        } else {
            unreadc(c, file);
            goto ret_float;
        }
    }
 ret_int:
    tok = make_token(TOKTYPE_INT, lineno);
    tok->val.i = atoi(STRING_BODY(buf));
    return tok;
 ret_float:
    tok = make_token(TOKTYPE_FLOAT, lineno);
    tok->val.f = atof(STRING_BODY(buf));
    return tok;
}

static char read_escaped(File *file) {
    int c = readc(file);
    switch (c) {
    case EOF:
        error("line %d: premature end of input file while reading a literal string or a character", file->lineno);
    case 'a': return '\a';
    case 'b': return '\b';
    case 't': return '\t';
    case 'n': return '\n';
    case 'v': return '\v';
    case 'f': return '\f';
    case 'r': return '\r';
    default: return (char)c;
    }
}

static char *read_str(File *file) {
    String *b = make_string();
    for (;;) {
        int c = readc(file);
        switch (c) {
        case '"':
            return STRING_BODY(b);
        case '\\':
            o1(b, read_escaped(file));
            break;
        case EOF:
            error("line %d: premature end of input file while reading a literal string", file->lineno);
        default:
            o1(b, c);
        }
    }
}

static char read_char(File *file) {
    int c = readc(file);
    switch (c) {
    case EOF:
        error("line %d: premature end of input file while reading a literal character", file->lineno);
    case '\\': return read_escaped(file);
    default: return (char)c;
    }
}

static char *read_ident(File *file, char c0) {
    String *b = make_string();
    o1(b, c0);
    for (;;) {
        int c = readc(file);
        if (isalnum(c)) {
            o1(b, c);
        } else if (c != EOF) {
            unreadc(c, file);
            return STRING_BODY(b);
        } else {
            return STRING_BODY(b);
        }
    }
}

Token *read_token(File *file) {
    Token *r;
    char *str;
    for (;;) {
        int c = readc(file);
        switch (c) {
        case ' ': case '\t': case '\r': case '\n':
            continue;
        case '0': case '1': case '2': case '3': case '4':
        case '5': case '6': case '7': case '8': case '9':
            return read_num(file, c, file->lineno);
        case '"':
            r = make_token(TOKTYPE_STRING, file->lineno);
            r->val.str = read_str(file);
            return r;
        case '\'':
            r = make_token(TOKTYPE_CHAR, file->lineno);
            r->val.c = read_char(file);
            return r;
        case 'a': case 'b': case 'c': case 'd': case 'e': case 'f': case 'g':
        case 'h': case 'i': case 'j': case 'k': case 'l': case 'm': case 'n':
        case 'o': case 'p': case 'q': case 'r': case 's': case 't': case 'u':
        case 'v': case 'w': case 'x': case 'y': case 'z': case 'A': case 'B':
        case 'C': case 'D': case 'E': case 'F': case 'G': case 'H': case 'I':
        case 'J': case 'K': case 'L': case 'M': case 'N': case 'O': case 'P':
        case 'Q': case 'R': case 'S': case 'T': case 'U': case 'V': case 'W':
        case 'X': case 'Y': case 'Z':
            str = read_ident(file, c);
#define KEYWORD(type_, val_)                                    \
            if (!strcmp(str, (type_))) {                        \
                r = make_token(TOKTYPE_KEYWORD, file->lineno);  \
                r->val.k = (val_);                              \
                return r;                                       \
            }
            KEYWORD("int", KEYWORD_INT);
            KEYWORD("float", KEYWORD_FLOAT);
#undef KEYWORD
            r = make_token(TOKTYPE_IDENT, file->lineno);
            r->val.str = str;
            return r;
        case '{': case '}': case '(': case ')': case ';': case ',':
            r = make_token(TOKTYPE_KEYWORD, file->lineno);
            r->val.c = c;
            return r;
        case EOF:
            return NULL;
        default:
            error("line %d: unimplemented '%c'", file->lineno, c);
        }
    }
    return NULL;
}

static void expect(File *file, char expected) {
    for (;;) {
        int c = readc(file);
        if (c == expected) return;
        switch (c) {
        case ' ': case '\t': case '\r': case '\n':
            continue;
        case EOF:
            error("line %d: '%c' expected, but got EOF", file->lineno, expected);
        default:
            error("line %d: '%c' expected, but got '%c'", file->lineno, expected, c);
        }
    }
}

static void read_statement(File *file, Section *data, Token *fntok, List *lis) {
    List *argtoks = make_list();
    expect(file, '(');
    for (;;) {
        Token *arg = read_token(file);
        list_push(argtoks, arg);
        Token *sep = read_token(file);
        if (sep->toktype != TOKTYPE_KEYWORD)
            error("line %d: expected ',', or ')', but got '%c'", sep->lineno, sep->val.c);
        if (sep->val.c == ')')
            break;
        if (sep->val.c != ',')
            error("line %d: expected ',', but got '%c'", sep->lineno, sep->val.c);
    }
    expect(file, ';');

    List *args = make_list();
    int i;
    for (i = 0; i < LIST_LEN(argtoks); i++) {
        Token *arg = LIST_ELEM(Token, argtoks, i);
        switch (arg->toktype) {
        case TOKTYPE_INT:
            list_push(args, make_imm(arg->val.i));
            break;
        case TOKTYPE_FLOAT:
            list_push(args, make_immf(arg->val.f));
            break;
        case TOKTYPE_IDENT:
        case TOKTYPE_CHAR:
            error("identifier or char is not supported here");
        case TOKTYPE_STRING:
            list_push(args, make_global("", add_string(data, to_string(arg->val.str))));
        }
    }
    Var *fn = make_extern(fntok->val.str);
    list_push(lis, make_func_call(fn, args));
}

static List *read_func(File *file, ReadContext *ctx) {
    Section *text = find_section(ctx->elf, ".text");
    Section *data = find_section(ctx->elf, ".data");
    List *r = make_list();
    Token *fname = read_token(file);
    if (fname->toktype != TOKTYPE_IDENT)
        error("line %d: identifier expected", fname->lineno);
    expect(file, '(');
    expect(file, ')');
    expect(file, '{');
    for (;;) {
        Token *tok = read_token(file);
        if (tok == NULL)
            error("premature end of input");
        if (tok->val.c == '}')
            break;
        read_statement(file, data, tok, r);
    }
    dict_put(ctx->elf->syms, to_string(fname->val.str), make_symbol(fname->val.str, text, 0, STB_GLOBAL, STT_NOTYPE, 1));
    return r;
}

List *parse(File *file, Elf *elf) {
    ReadContext *ctx = make_read_context(elf);
    return read_func(file, ctx);
}
