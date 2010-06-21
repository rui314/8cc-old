#include "8cc.h"

static Token *make_token(char c) {
    Token *r = malloc(sizeof(Token));
    r->val = c;
    return r;
}

static int read_num(FILE *file, int num) {
    for (;;) {
	int c = getc(file);
	if (c == EOF) {
	    return num;
	} else if ('0' <= c && c <= '9') {
	    num = num * 10 + (c - '0');
	} else {
	    ungetc(c, file);
	    return num;
	}
    }
}

static char *read_str(FILE *file) {
    StringBuilder *b = make_sbuilder();
    for (;;) {
	int c = getc(file);
	switch (c) {
	case '"':
	    return SBUILDER_BODY(b);
	case '\\': {
	    int c2 = getc(file);
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
	    error("premature end of input file");
	default:
	    o1(b, c);
	}
    }
}

static char *read_ident(FILE *file, char c0) {
    StringBuilder *b = make_sbuilder();
    o1(b, c0);
    for (;;) {
	int c = getc(file);
	if (isalnum(c)) {
	    o1(b, c);
	} else if (c != EOF) {
	    ungetc(c, file);
	    return SBUILDER_BODY(b);
	} else {
	    return SBUILDER_BODY(b);
	}
    }
}

static Token *readtok(FILE *file) {
    Token *r;
    for (;;) {
	int c = getc(file);
	switch (c) {
	case ' ': case '\t': case '\r': case '\n':
	    continue;
	case '0': case '1': case '2': case '3': case '4': 
	case '5': case '6': case '7': case '8': case '9': 
	    r = make_token(TOK_NUM);
	    r->num = read_num(file, c - '0');
	    return r;
	case '"':
	    r = make_token(TOK_STR);
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
	    r = make_token(TOK_IDENT);
	    r->str = read_ident(file, c);
	    return r;
	case '{': case '}': case '(': case ')': case ';': case ',':
	    return make_token(c);
	case EOF:
	    return NULL;
	default:
	    error("unimplemented '%c'", c);
	}
    }
    return make_token('(');
}

static void expect(FILE *file, char expected) {
    for (;;) {
	int c = getc(file);
	if (c == expected) return;
	switch (c) {
	case ' ': case '\t': case '\r': case '\n':
	    continue;
	case EOF:
	    error("%c expected, but got EOF", expected);
	default:
	    error("%c expected, but got %c", expected, c);
	}
    }
}

static void read_statement(FILE *file, Section *text, Section *data, List *lis) {
    Token *fntok = readtok(file);
    List *argtoks = make_list();
    expect(file, '(');
    for (;;) {
	Token *arg = readtok(file);
	list_push(argtoks, arg);
	Token *sep = readtok(file);
	if (sep->val == ')')
	    break;
	if (sep->val != ',')
	    error("expected ',', but got %c", sep->val);
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

static List *read_func(FILE *file, Section *text, Section *data) {
    List *r = make_list();
    Token *tok = readtok(file);
    if (tok->val != TOK_IDENT)
	error("identifier expected");
    expect(file, '(');
    expect(file, ')');
    expect(file, '{');
    read_statement(file, text, data, r);
    expect(file, '}');
    return r;
}

List *parse(FILE *file, Section *text, Section *data) {
    return read_func(file, text, data);
}
