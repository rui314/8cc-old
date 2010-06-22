#include "8cc.h"

static Token *make_token(char c, int lineno) {
    Token *r = malloc(sizeof(Token));
    r->val = c;
    r->lineno = lineno;
    return r;
}

static int read_num(File *file, int num) {
    for (;;) {
        int c = readc(file);
        if (c == EOF) {
            return num;
        } else if ('0' <= c && c <= '9') {
            num = num * 10 + (c - '0');
        } else {
            unreadc(c, file);
            return num;
        }
    }
}

static char *read_str(File *file) {
    String *b = make_string();
    for (;;) {
        int c = readc(file);
        switch (c) {
        case '"':
            return STRING_BODY(b);
        case '\\': {
            int c2 = readc(file);
            switch (c2) {
            case 'n':
                o1(b, '\n');
                break;
            default:
                o1(b, c2);
                break;
            }
            break;
        }
        case EOF:
            error("line %d: premature end of input file", file->lineno);
        default:
            o1(b, c);
        }
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

static Token *read_token(File *file) {
    Token *r;
    for (;;) {
        int c = readc(file);
        switch (c) {
        case ' ': case '\t': case '\r': case '\n':
            continue;
        case '0': case '1': case '2': case '3': case '4': 
        case '5': case '6': case '7': case '8': case '9': 
            r = make_token(TOK_NUM, file->lineno);
            r->num = read_num(file, c - '0');
            return r;
        case '"':
            r = make_token(TOK_STR, file->lineno);
            r->str = read_str(file);
            return r;
        case 'a': case 'b': case 'c': case 'd': case 'e': case 'f': case 'g':
        case 'h': case 'i': case 'j': case 'k': case 'l': case 'm': case 'n':
        case 'o': case 'p': case 'q': case 'r': case 's': case 't': case 'u':
        case 'v': case 'w': case 'x': case 'y': case 'z': case 'A': case 'B':
        case 'C': case 'D': case 'E': case 'F': case 'G': case 'H': case 'I':
        case 'J': case 'K': case 'L': case 'M': case 'N': case 'O': case 'P':
        case 'Q': case 'R': case 'S': case 'T': case 'U': case 'V': case 'W':
        case 'X': case 'Y': case 'Z':
            r = make_token(TOK_IDENT, file->lineno);
            r->str = read_ident(file, c);
            return r;
        case '{': case '}': case '(': case ')': case ';': case ',':
            return make_token(c, file->lineno);
        case EOF:
            return NULL;
        default:
            error("line %d: unimplemented '%c'", file->lineno, c);
        }
    }
    return make_token('(', file->lineno);
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

static void read_statement(File *file, Section *text, Section *data, List *lis) {
    Token *fntok = read_token(file);
    List *argtoks = make_list();
    expect(file, '(');
    for (;;) {
        Token *arg = read_token(file);
        list_push(argtoks, arg);
        Token *sep = read_token(file);
        if (sep->val == ')')
            break;
        if (sep->val != ',')
            error("line %d: expected ',', but got '%c'", sep->lineno, sep->val);
    }
    expect(file, ';');
    
    Var **args = malloc(sizeof(Var *) * (LIST_LEN(argtoks) + 1));
    int i;
    for (i = 0; i < LIST_LEN(argtoks); i++) {
        Token *arg = LIST_ELEM(Token, argtoks, i);
        switch (arg->val) {
        case TOK_NUM:
            args[i] = make_imm(arg->num);
            break;
        case TOK_IDENT:
        case TOK_CHAR:
            error("identifier or char is not supported here");
        case TOK_STR:
            args[i] = make_global("", add_string(data, arg->str));
        }
    }
    args[i] = NULL;
    Var *fn = make_extern(fntok->str, text);
    list_push(lis, make_func_call(fn, args));
}

static List *read_func(File *file, Section *text, Section *data) {
    List *r = make_list();
    Token *tok = read_token(file);
    if (tok->val != TOK_IDENT)
        error("line %d: identifier expected", tok->lineno);
    expect(file, '(');
    expect(file, ')');
    expect(file, '{');
    read_statement(file, text, data, r);
    expect(file, '}');
    return r;
}

List *parse(File *file, Section *text, Section *data) {
    return read_func(file, text, data);
}
