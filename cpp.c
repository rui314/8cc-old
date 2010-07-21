/*
 * cpp.c - C preprocessor
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

/*==============================================================================
 * Utility functions for token handling.
 */

static Token *read_if(CppContext *ctx, char *str) {
    Token *tok = read_cpp_token(ctx);
    if (tok && tok->toktype == TOKTYPE_IDENT && !strcmp(STRING_BODY(tok->val.str), str))
        return tok;
    unget_cpp_token(ctx, tok);
    return NULL;
}

static bool is_punct(Token *tok, int v) {
    return tok->toktype == TOKTYPE_PUNCT && tok->val.i == v;
}

void expect_newline(CppContext *ctx) {
    Token *tok = read_cpp_token(ctx);
    if (!tok || tok->toktype != TOKTYPE_NEWLINE)
        error_token(tok, "newline expected, but got '%s'", token_to_string(tok));

}

/*==============================================================================
 * Data structure representing a macro
 */

typedef Token *special_macro_handler(CppContext *ctx, Token *tok);

// Object-like macro, function-like macro and special macro
// (e.g. __FILE__ or __LINE__).
#define MACRO_OBJ     1
#define MACRO_FUNC    2
#define MACRO_SPECIAL 3

typedef struct Macro {
    int type;
    // For object-like and function-like
    int nargs;
    List *body;
    bool is_varg;
    // For special macros
    special_macro_handler *fn;
} Macro;

static Macro *make_obj_macro(List *body) {
    Macro *r = malloc(sizeof(Macro));
    r->type = MACRO_OBJ;
    r->body = body;
    r->is_varg = false;
    return r;
}

static Macro *make_func_macro(List *body, int nargs, bool is_varg) {
    Macro *r = malloc(sizeof(Macro));
    r->type = MACRO_FUNC;
    r->nargs = nargs;
    r->body = body;
    r->is_varg = is_varg;
    return r;
}

static Macro *make_special_macro(special_macro_handler *fn) {
    Macro *r = malloc(sizeof(Macro));
    r->type = MACRO_SPECIAL;
    r->fn = fn;
    return r;
}

/*==============================================================================
 * Keyword recognizer and utility functions.
 */

static Dict *keyword_dict(void) {
    static Dict *dict;
    if (dict != NULL)
        return dict;
    dict = make_string_dict();
#define KEYWORD(id_, str_) \
    dict_put(dict, to_string(str_), (void *)id_);
#define PUNCT(id_, str_)
# include "keyword.h"
#undef PUNCT
#undef KEYWORD
    return dict;
}

static Dict *punct_dict(void) {
    static Dict *dict;
    if (dict) return dict;
    dict = make_string_dict();
#define KEYWORD(k, s)
#define PUNCT(k, s) \
    dict_put(dict, to_string(s), (void *)k);
# include "keyword.h"
#undef PUNCT
#undef KEYWORD
    return dict;
}

static Token *to_keyword_maybe(Token *tok) {
    assert(tok->toktype == TOKTYPE_IDENT);
    int id = (intptr)dict_get(keyword_dict(), tok->val.str);
    if (id) {
        Token *r = copy_token(tok);
        r->toktype = TOKTYPE_KEYWORD;
        r->val.i = id;
        return r;
    }
    return tok;
}

static Token *cppnum_to_num(Token *tok) {
    Token *r = copy_token(tok);
    char *p = STRING_BODY(tok->val.str);
    if (strchr(p, '.')) {
        r->toktype = TOKTYPE_FLOAT;
        r->val.f = atof(p);
    } else {
        r->toktype = TOKTYPE_INT;
        r->val.i = atoi(p);
    }
    return r;
}

static Token *cpp_token_to_token(Token *tok) {
    tok->hideset = NULL;
    if (tok->toktype == TOKTYPE_IDENT)
        return to_keyword_maybe(tok);
    if (tok->toktype == TOKTYPE_CPPNUM)
        return cppnum_to_num(tok);
    if (tok->toktype == TOKTYPE_PUNCT) {
        Token *r = copy_token(tok);
        r->toktype = TOKTYPE_KEYWORD;
        return r;
    }
    assert(tok->toktype == TOKTYPE_CHAR || tok->toktype == TOKTYPE_STRING);
    return tok;
}

/*==============================================================================
 * WG14/N1256 6.10.8 Predefined macro names.
 */

static void def_obj_macro(CppContext *ctx, char *name, Token *tok) {
    List *list = make_list();
    list_push(list, tok);
    dict_put(ctx->defs, to_string(name), make_obj_macro(list));
}

static void def_special_macro(CppContext *ctx, char *name, special_macro_handler *fn) {
    dict_put(ctx->defs, to_string(name), make_special_macro(fn));
}

static Token *macro_date(CppContext *ctx, struct tm *now) {
    char *month[] = { "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec" };
    String *s = make_string_printf("%s %02d %04d", month[now->tm_mon], now->tm_mday, 1900 + now->tm_year);
    return make_str_literal(ctx, s);
}

static Token *macro_time(CppContext *ctx, struct tm *now) {
    String *s = make_string_printf("%02d:%02d:%02d", now->tm_hour, now->tm_min, now->tm_sec);
    return make_str_literal(ctx, s);
}

static Token *handle_file_macro(CppContext *ctx, Token *tok) {
    Token *r = copy_token(tok);
    r->toktype = TOKTYPE_STRING;
    r->val.str = ctx->file->filename;
    return r;
}

static Token *handle_line_macro(CppContext *ctx, Token *tok) {
    Token *r = copy_token(tok);
    r->toktype = TOKTYPE_CPPNUM;
    r->val.str = make_string_printf("%d", ctx->file->line);
    return r;
}

void define_predefined_macros(CppContext *ctx) {
    time_t timet = time(NULL);
    struct tm now;
    localtime_r(&timet, &now);

    def_obj_macro(ctx, "__8CC__", make_cppnum(ctx, to_string("1")));
    def_obj_macro(ctx, "__STDC__", make_cppnum(ctx, to_string("1")));
    def_obj_macro(ctx, "__STDC_HOSTED__", make_cppnum(ctx, to_string("1")));
    def_obj_macro(ctx, "__STDC_VERSION__", make_cppnum(ctx, to_string("199901L")));
    def_obj_macro(ctx, "__DATE__", macro_date(ctx, &now));
    def_obj_macro(ctx, "__TIME__", macro_time(ctx, &now));
    def_special_macro(ctx, "__FILE__", handle_file_macro);
    def_special_macro(ctx, "__LINE__", handle_line_macro);
}

/*
 * The following macros are not defined for now.
 *  __STDC_MB_MIGHT_NEQ_WC__
 *  __STDC_IEC_559__
 *  __STDC_IEC_559_COMPLEX__
 *  __STDC_ISO_10646__
 */

/*==============================================================================
 * Macro expansion.
 *
 * Preprocessor macro expansion is woefully underspecified in the C standard.
 * An insufficient implementation could not fully expand all recursive macros,
 * while too aggressive expansion could go into infinite replacement loop.
 *
 * In order to prevent infinite expansion, the macro expander here maintains
 * "hide set" for each token.  If a token is placed as a result of macro X's
 * expansion, the name "X" is added to the hide set of the token.  The token
 * will not trigger execution of macro X because of the hide set, that prevents
 * infinite loop.
 *
 * There are many phases in macro expansion.  In the first phase, a macro
 * arguments are parsed as a sequence of tokens separated by comma.  The macro's
 * replacement list is read next, to see if there are any macro argument used as
 * the operand of # or ## operator.  If so, the arguments are converted to
 * string or concatenated, respectively.  All the other arguments are fully
 * macro-expanded, and then the macro parameters will be replaced by the
 * expanded arguments.  The resulting sequence of tokens are read again by the
 * macro expander, until the first token is not a macro.
 *
 * A implementation that process macro expansion in a different order could
 * produce fully-expanded but wrong results of a macro.
 *
 * The implementation here is based on Dave Prosser's expansion algorithm, that
 * is said the ANSI C committee used as a basis for the standard's wording,
 * described in the following PDF.  I believe the algorithm and my implemention
 * is correct, as it correctly expands all example macros in the C
 * specification, while I'm not 100% sure.
 *
 * Blog dds: Dave Prosser's C Preprocessing Algorithm
 * http://www.spinellis.gr/blog/20060626/
 */

static Token *expand(CppContext *ctx);

CppContext *make_virt_cpp_context(CppContext *ctx, List *ts) {
    CppContext *r = make_cpp_context(NULL);
    r->at_bol = false;
    r->defs = ctx->defs;
    r->ungotten = ts;
    r->in_macro = true;
    return r;
}

/*
 * Expands all macros in a given token list, as if they consisted the rest of
 * the source file.  A new preprocessing contests are created, and the tokens
 * are pushed to the context so that subsequent read_cpp_token() will get them,
 * as if these were the content of a file.
 *
 * expand() and expand_all() calls each other to get the fully expanded form of
 * a macro.
 */
static List *expand_all(CppContext *ctx, List *ts) {
    List *r = make_list();
    CppContext *virt = make_virt_cpp_context(ctx, list_reverse(ts));
    Token *tok;
    while ((tok = expand(virt)) != NULL)
        list_push(r, tok);
    return r;
}

static void pushback(CppContext *ctx, List *ts) {
    for (int i = LIST_LEN(ts) - 1; i >= 0; i--)
        unget_cpp_token(ctx, (Token *)LIST_REF(ts, i));
}

/*
 * Reads comma-separated arguments of function-like macro invocation.  Comma
 * characters in matching parentheses are not considered as separator.
 *
 * If there is no terminating ')', we'll pushback all tokens and returns NULL,
 * so that macro expander continue processing the tokens without expanding the
 * macro.
 *
 * (WG14/N1256 6.10.3 Macro replacement, sentence 10)
 */
static List *read_args_int(CppContext *ctx, Macro *macro) {
    List *r = make_list();
    List *arg = make_list();
    List *buf = make_list();
    int depth = 0;

    Token *tok = peek_cpp_token(ctx);
    if (!tok || !is_punct(tok, '('))
        return NULL;
    read_cpp_token(ctx);
    list_push(buf, tok);

    for (tok = read_cpp_token(ctx); ; tok = read_cpp_token(ctx)) {
        if (!tok) {
            pushback(ctx, buf);
            return NULL;
        }
        list_push(buf, tok);
        if (tok->toktype == TOKTYPE_NEWLINE)
            continue;
        if (depth) {
            if (is_punct(tok, ')'))
                depth--;
            list_push(arg, tok);
            continue;
        }
        if (is_punct(tok, '('))
            depth++;
        if (is_punct(tok, ')')) {
            unget_cpp_token(ctx, tok);
            list_push(r, arg);
            return r;
        }
        bool in_threedots = macro->is_varg && LIST_LEN(r) + 1 == macro->nargs;
        if (is_punct(tok, ',') && !in_threedots) {
            list_push(r, arg);
            arg = make_list();
            continue;
        }
        list_push(arg, tok);
    }
}

static List *read_args(CppContext *ctx, Macro *macro) {
    List *args = read_args_int(ctx, macro);
    if (!args) return NULL;

    /*
     * In CPP syntax, we cannot distinguish zero-argument macro invocation from
     * one argument macro invocation, because macro argument can be blank.  For
     * example, the following macro invocation
     *
     *   FOO()
     *
     * is valid for both definitions shown below.
     *
     *   #define FOO()  1
     *   #define FOO(x) x
     *
     * In the latter case, macro parameter "x" become an empty sequence of
     * identifiers, thus FOO() will be replaced with the empty.
     *
     * The argument list is set to empty here if macro takes no parameters and
     * the argument is empty.
     */
    if (macro->nargs == 0 && LIST_LEN(args) == 1 && LIST_LEN((List *)LIST_REF(args, 0)) == 0)
        list_pop(args);

    if ((macro->is_varg && LIST_LEN(args) < macro->nargs)
        || (!macro->is_varg && LIST_LEN(args) != macro->nargs))
        error_cpp_ctx(ctx, "Macro argument number does not match");
    return args;
}

static List *add_hide_set(List *ts, List *hideset) {
    List *r = make_list();
    for (int i = 0; i < LIST_LEN(ts); i++) {
        Token *t = copy_token((Token *)LIST_REF(ts, i));
        t->hideset = list_union(t->hideset, hideset);
        list_push(r, t);
    }
    return r;
}

void stringize_char(String *b, char c, char quote) {
    if (!isascii(c))
        string_printf(b, "\\x%02x", (u8)c);
    else if (c == '\\' || c == quote)
        string_printf(b, "\\%c", c);
    else
        string_printf(b, "%c", c);
}

void paste(String *b, Token *tok) {
    switch (tok->toktype) {
    case TOKTYPE_IDENT:
    case TOKTYPE_CPPNUM:
        string_append(b, STRING_BODY(tok->val.str));
        return;
    case TOKTYPE_PUNCT:
        string_append(b, token_to_string(tok));
        return;
    default:
        error_token(tok, "pasting invalid token: '%s'", token_to_string(tok));
    }
}

/*
 * Joins given two tokens and returns it.  Used by "##" operator.
 */
Token *glue_tokens(Token *t0, Token *t1) {
    String *b = make_string();
    paste(b, t0);
    paste(b, t1);
    Token *r = copy_token(t0);
    if (isdigit(STRING_BODY(b)[0])) {
        r->toktype = TOKTYPE_CPPNUM;
        r->val.str = b;
        return r;
    }
    int punct = (intptr)dict_get(punct_dict(), b);
    if (punct) {
        r->toktype = TOKTYPE_PUNCT;
        r->val.i = punct;
        return r;
    }
    r->toktype = TOKTYPE_IDENT;
    r->val.str = b;
    return r;
}

void glue_push(List *ls, Token *tok) {
    assert(!LIST_IS_EMPTY(ls));
    Token *last = list_pop(ls);
    list_push(ls, glue_tokens(last, tok));
}

/*
 * Write a string representation of a given token to a buffer.  Used by "#"
 * operator.
 */
Token *stringize(Token *tmpl, List *arg) {
    String *s = make_string();
    for (int i = 0; i < LIST_LEN(arg); i++) {
        Token *tok = LIST_REF(arg, i);
        if (STRING_LEN(s) && tok->space)
            o1(s, ' ');
        switch (tok->toktype) {
        case TOKTYPE_IDENT:
        case TOKTYPE_CPPNUM:
            string_append(s, STRING_BODY(tok->val.str));
            break;
        case TOKTYPE_PUNCT:
            string_append(s, token_to_string(tok));
            break;
        case TOKTYPE_CHAR: {
            // TODO: retain original spelling
            o1(s, '\'');
            stringize_char(s, tok->val.i, '\'');
            string_append(s, "\'");
            break;
        }
        case TOKTYPE_STRING: {
            o1(s, '"');
            for (char *p = STRING_BODY(tok->val.str); *p; p++)
                stringize_char(s, *p, '\"');
            string_append(s, "\"");
            break;
        }
        default:
            panic("invalid token type: %d", tok->toktype);
        }
    }
    Token *r = copy_token(tmpl);
    r->toktype = TOKTYPE_STRING;
    r->val.str = s;
    return r;
}

/*
 * Substitutes parameters in macro replacement list with actual arguments.
 */
static List *subst(CppContext *ctx, Macro *macro, List *args, List *hideset) {
    List *r = make_list();
    for (int i = 0; i < LIST_LEN(macro->body); i++) {
        bool islast = (i == LIST_LEN(macro->body) - 1);
        Token *t0 = LIST_REF(macro->body, i);
        Token *t1 = islast ? NULL : LIST_REF(macro->body, i + 1);
        bool t0_param = t0->toktype == TOKTYPE_MACRO_PARAM;
        bool t1_param = !islast && t1->toktype == TOKTYPE_MACRO_PARAM;

        if (is_punct(t0, '#') && t1_param) {
            list_push(r, stringize(t0, LIST_REF(args, t1->val.i)));
            i++;
            continue;
        }
        if (is_punct(t0, KEYWORD_TWOSHARPS) && t1_param) {
            List *arg = LIST_REF(args, t1->val.i);
            if (!LIST_IS_EMPTY(arg)) {
                glue_push(r, (Token *)LIST_REF(arg, 0));
                List *tmp = make_list();
                for (int i = 1; i < LIST_LEN(arg); i++)
                    list_push(tmp, LIST_REF(arg, i));
                list_append(r, expand_all(ctx, tmp));
            }
            i++;
            continue;
        }
        if (is_punct(t0, KEYWORD_TWOSHARPS) && !islast) {
            hideset = t1->hideset; // wrong?
            glue_push(r, t1);
            i++;
            continue;
        }
        if (t0_param && !islast && is_punct(t1, KEYWORD_TWOSHARPS)) {
            hideset = t1->hideset; // wrong?
            List *arg = LIST_REF(args, t0->val.i);
            if (LIST_IS_EMPTY(arg))
                i++;
            else
                list_append(r, arg);
            continue;
        }
        if (t0_param) {
            List *arg = LIST_REF(args, t0->val.i);
            list_append(r, expand_all(ctx, arg));
            continue;
        }
        list_push(r, t0);
    }
    return add_hide_set(r, hideset);
}

/*
 * Reads a token from a given preprocessing context, expands it if macro, and
 * returns it.
 */
static Token *expand(CppContext *ctx) {
    Token *tok = read_cpp_token(ctx);
    if (!tok) return NULL;
    if (tok->toktype != TOKTYPE_IDENT)
        return tok;
    String *name = tok->val.str;
    if (list_in(tok->hideset, name))
        return tok;
    Macro *macro = dict_get(ctx->defs, name);
    if (!macro)
        return tok;

    switch (macro->type) {
    case MACRO_OBJ: {
        List *ts = subst(ctx, macro, make_list(), list_union1(tok->hideset, name));
        pushback(ctx, ts);
        return expand(ctx);
    }
    case MACRO_FUNC: {
        List *args = read_args(ctx, macro);
        if (!args)
            return tok;
        Token *rparen = read_cpp_token(ctx);
        List *hideset = list_union1(list_intersect(tok->hideset, rparen->hideset), name);
        List *ts = subst(ctx, macro, args, hideset);
        pushback(ctx, ts);
        return expand(ctx);
    }
    case MACRO_SPECIAL:
        return macro->fn(ctx, tok);
    }
    panic("should not reach here");
}

/*==============================================================================
 * Preprocessor directives.
 */

static int is_defined(CppContext *ctx, Token *tok) {
    if (!tok || tok->toktype != TOKTYPE_IDENT)
        error_token(tok, "identifier expected, but got '%s'", token_to_string(tok));
    return dict_has(ctx->defs, tok->val.str) ? 1 : 0;
}

/*
 * Reads "defined" unary operator of the form "defined <identifier>" or
 * "defined(<identifier>)".  The token "defined" is already read when the
 * function is called.
 *
 * (WG14/N1256 6.10.1 Conditional inclusion, paragraph 1)
 */
static int read_defined(CppContext *ctx) {
    Token *tok = read_cpp_token(ctx);
    if (is_punct(tok, '(')) {
        tok = read_cpp_token(ctx);
        Token *tok1 = read_cpp_token(ctx);
        if (!tok1 || !is_punct(tok1, ')'))
            error_token(tok1, "')' expected, but got '%s'", token_to_string(tok1));
    }
    return is_defined(ctx, tok);
}

static int read_constant_expr(CppContext *ctx) {
    if (read_if(ctx, "defined"))
        return read_defined(ctx);
    panic("only defined() is implemented");
}

static int read_ifdef(CppContext *ctx) {
    int r = is_defined(ctx, read_cpp_token(ctx));
    expect_newline(ctx);
    return r;
}

/*
 * Handles #if, #elif, #ifdef and #ifndef.  If condition does not meet, the
 * function calls skip_cond_include(), defined in lex.c, to skip all tokens
 * untilt the next #elif, #else of #endif.
 *
 * (WG14/N1256 6.10.1 Conditional inclusion)
 */
static void handle_cond_incl(CppContext *ctx, CondInclType type) {
    bool cond;
    switch (type) {
    case COND_IF:
        cond = read_constant_expr(ctx);
        break;
    case COND_IFDEF:
        cond = read_ifdef(ctx);
        break;
    case COND_IFNDEF:
        cond = !read_ifdef(ctx);
        break;
    case COND_ELIF:
        error_cpp_ctx(ctx, "stray #elif");
    case COND_ELSE:
        expect_newline(ctx);
        if (LIST_IS_EMPTY(ctx->incl))
            error_cpp_ctx(ctx, "stray #else");
        bool in_else = (intptr)list_pop(ctx->incl);
        if (in_else)
            error_cpp_ctx(ctx, "#else appears twice");
        CondInclType type1 = skip_cond_incl(ctx);
        if (type1 == COND_ELIF)
            error_cpp_ctx(ctx, "stray #elif");
        if (type1 == COND_ELSE)
            error_cpp_ctx(ctx, "stray #else");
        return;
    case COND_ENDIF:
        expect_newline(ctx);
        if (LIST_IS_EMPTY(ctx->incl))
            error_cpp_ctx(ctx, "stray #endif");
        list_pop(ctx->incl);
        return;
    }
    if (cond) {
        list_push(ctx->incl, (void *)false);
        return;
    }
    // skip_cond_incl() returns one of COND_ELIF, COND_ELSE or COND_ENDIF.
    CondInclType type1 = skip_cond_incl(ctx);
    if (type1 == COND_ELIF)
        handle_cond_incl(ctx, COND_IF);
    else if (type1 == COND_ELSE)
        list_push(ctx->incl, (void *)true);
}

/*
 * Reads function-like macro arguments.  Returns true if the argument list ends
 * with "...".  Otherwise false.
 */
static bool read_funclike_define_args(CppContext *ctx, Dict *param) {
    for (;;) {
        Token *tok = read_cpp_token(ctx);
        if (is_punct(tok, ')'))
            return false;
        if (dict_size(param)) {
            if (!is_punct(tok, ','))
                error_token(tok, "',' expected, but got '%s'", token_to_string(tok));
            tok = read_cpp_token(ctx);
        }
        if (!tok || tok->toktype == TOKTYPE_NEWLINE)
            error_token(tok, "missing ')' in macro parameter list");
        if (is_punct(tok, KEYWORD_THREEDOTS)) {
            Token *subst = make_token(ctx, TOKTYPE_MACRO_PARAM, (TokenValue)dict_size(param));
            dict_put(param, to_string("__VA_ARGS__"), subst);
            Token *tok1 = read_cpp_token(ctx);
            if (!is_punct(tok1, ')'))
                error_token(tok1, "')' expected, but got '%s'", token_to_string(tok1));
            return true;
        }
        if (tok->toktype != TOKTYPE_IDENT)
            error_token(tok, "identifier expected, but got '%s'", token_to_string(tok));
        Token *subst = make_token(ctx, TOKTYPE_MACRO_PARAM, (TokenValue)dict_size(param));
        dict_put(param, tok->val.str, subst);
    }
}

static List *read_funclike_define_body(CppContext *ctx, Dict *param) {
    List *body = make_list();
    // Read macro body list
    for (;;) {
        Token *tok = read_cpp_token(ctx);
        if (!tok || tok->toktype == TOKTYPE_NEWLINE)
            return body;
        if (tok->toktype == TOKTYPE_IDENT) {
            Token *subst = dict_get(param, tok->val.str);
            if (subst) {
                list_push(body, subst);
                continue;
            }
        }
        list_push(body, tok);
    }
    return body;
}

/*
 * Reads function-like macro definition.
 */
static void read_funclike_define(CppContext *ctx, String *name) {
    Dict *param = make_string_dict();
    bool is_varg = read_funclike_define_args(ctx, param);
    List *body = read_funclike_define_body(ctx, param);
    dict_put(ctx->defs, name, make_func_macro(body, dict_size(param), is_varg));
}

/*
 * #define
 * (WG14/N1256 6.10.3 Macro replacement)
 */
static void read_define(CppContext *ctx) {
    Token *name = read_cpp_token(ctx);
    if (name->toktype != TOKTYPE_IDENT)
        error_cpp_ctx(ctx, "macro name must be an identifier, but got '%s'", token_to_string(name));

    bool is_space = is_next_space(ctx);
    Token *tok = read_cpp_token(ctx);

    if (!is_space && tok && is_punct(tok, '(')) {
        read_funclike_define(ctx, name->val.str);
        return;
    }

    List *body = make_list();
    while (tok && tok->toktype != TOKTYPE_NEWLINE) {
        list_push(body, tok);
        tok = read_cpp_token(ctx);
    }
    dict_put(ctx->defs, name->val.str, make_obj_macro(body));
}

/*
 * #undef
 * (WG14/N1256 6.10.5 Scope of macro definisions, paragraph 2)
 */
static void read_undef(CppContext *ctx) {
    Token *name = read_cpp_token(ctx);
    if (!name || name->toktype != TOKTYPE_IDENT)
        error_token(name, "undef works only to an identifier, but got '%s'", token_to_string(name));
    expect_newline(ctx);
    dict_delete(ctx->defs, name->val.str);
}

/*
 * #error
 * (WG14/N1256 6.10.5 Error directive)
 */
static void read_error_dir(CppContext *ctx, Token *define) {
    String *buf = make_string();
    Token *tok = read_cpp_token(ctx);
    while(tok && tok->toktype != TOKTYPE_NEWLINE) {
        o1(buf, ' ');
        string_append(buf, token_to_string(tok));
        tok = read_cpp_token(ctx);
    }
    error_token(define, "error: #error:%s", STRING_BODY(buf));
}

static void read_directive(CppContext *ctx) {
    Token *tok;
    if (read_if(ctx, "define"))      read_define(ctx);
    else if (read_if(ctx, "undef"))  read_undef(ctx);
    else if (read_if(ctx, "if"))     handle_cond_incl(ctx, COND_IF);
    else if (read_if(ctx, "elif"))   handle_cond_incl(ctx, COND_ELIF);
    else if (read_if(ctx, "else"))   handle_cond_incl(ctx, COND_ELSE);
    else if (read_if(ctx, "ifdef"))  handle_cond_incl(ctx, COND_IFDEF);
    else if (read_if(ctx, "ifndef")) handle_cond_incl(ctx, COND_IFNDEF);
    else if (read_if(ctx, "endif"))  handle_cond_incl(ctx, COND_ENDIF);
    else if ( (tok = read_if(ctx, "error")) ) {
        read_error_dir(ctx, tok);
    } else {
        Token *tok = read_cpp_token(ctx);
        error_token(tok, "unsupported preprocessor directive: '%s'", token_to_string(tok));
    }
}

/*==============================================================================
 * Entry function for the main C compiler.
 *
 * read_token() reads preprocessing tokens by calling read_cpp_token(), which is
 * defined in lex.c.  There are six types of tokens can be returned from
 * read_cpp_token().
 *
 *     identifier
 *     pp-number
 *     character-constant
 *     string-literal
 *     punctuator
 *     newline
 *
 * read_token() evaluates preprocessing directives (such as "#define") appearing
 * in the sequence of preprocessing tokens, as well as expanding macros.  The
 * resulting tokens are then converted to ordinary tokens before returning to
 * the main compiler.
 *
 * Preprocessing numbers will be converted to integer or float numbers.
 * Punctuators to keywords.  Identifiers to keywords (if reserved words) or keep
 * as is.  Newline tokens removed.
 */

Token *read_token(ReadContext *readctx) {
    if (!LIST_IS_EMPTY(readctx->ungotten))
        return list_pop(readctx->ungotten);

    CppContext *ctx = readctx->cppctx;
    for (;;) {
        Token *tok = read_cpp_token(ctx);
        if (!tok)
            return NULL;
        if (tok->toktype == TOKTYPE_NEWLINE) {
            ctx->at_bol = true;
            continue;
        }
        if (ctx->at_bol && is_punct(tok, '#')) {
            read_directive(ctx);
            ctx->at_bol = true;
            continue;
        }
        ctx->at_bol = false;
        unget_cpp_token(ctx, tok);
        return cpp_token_to_token(expand(ctx));
    }
}
