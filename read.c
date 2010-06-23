#include "8cc.h"

static Token *make_token(char c, int lineno) {
    Token *r = malloc(sizeof(Token));
    r->val = c;
    r->lineno = lineno;
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
    tok = make_token(TOK_NUM, lineno);
    tok->num = atoi(STRING_BODY(buf));
    return tok;
 ret_float:
    tok = make_token(TOK_FLOAT, lineno);
    tok->flo = atof(STRING_BODY(buf));
    return tok;
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
            error("line %d: premature end of input file while reading a literal string", file->lineno);
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

Token *read_token(File *file) {
    Token *r;
    for (;;) {
        int c = readc(file);
        switch (c) {
        case ' ': case '\t': case '\r': case '\n':
            continue;
        case '0': case '1': case '2': case '3': case '4': 
        case '5': case '6': case '7': case '8': case '9': 
            return read_num(file, c, file->lineno);
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

static void read_statement(File *file, Section *data, List *lis) {
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
    
    List *args = make_list();
    int i;
    for (i = 0; i < LIST_LEN(argtoks); i++) {
        Token *arg = LIST_ELEM(Token, argtoks, i);
        switch (arg->val) {
        case TOK_NUM:
            list_push(args, make_imm(arg->num));
            break;
        case TOK_FLOAT:
            list_push(args, make_immf(arg->flo));
            break;
        case TOK_IDENT:
        case TOK_CHAR:
            error("identifier or char is not supported here");
        case TOK_STR:
            list_push(args, make_global("", add_string(data, arg->str)));
        }
    }
    Var *fn = make_extern(fntok->str);
    list_push(lis, make_func_call(fn, args));
}

static List *read_func(File *file, Section *data) {
    List *r = make_list();
    Token *tok = read_token(file);
    if (tok->val != TOK_IDENT)
        error("line %d: identifier expected", tok->lineno);
    expect(file, '(');
    expect(file, ')');
    expect(file, '{');
    read_statement(file, data, r);
    expect(file, '}');
    return r;
}

List *parse(File *file, Section *data) {
    return read_func(file, data);
}
