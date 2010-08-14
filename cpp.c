/*
 * cpp.c - C preprocessor
 *
 * Copyright 2010 Rui Ueyama <rui314@gmail.com>.  All rights reserved.
 * This code is available under the simplified BSD license.  See LICENSE for details.
 */

#include "8cc.h"

/*
 * References:
 * C99 spec: http://www.open-std.org/jtc1/sc22/WG14/www/docs/n1256.pdf
 */


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
    if (tok && tok->toktype != TOKTYPE_NEWLINE)
        error_token(tok, "newline expected, but got '%s'", token_to_string(tok));

}

/*==============================================================================
 * Data structure representing a macro
 */

typedef void special_macro_handler(CppContext *ctx, Token *tok);

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
    /*
     * If "dict" is already initialized, returns it.  Otherwise, we'll add all
     * keywords to a newly created dictionary.  Temporary variable is needed as
     * we don't want to export the dictionary being built to the other threads.
     */
    static Dict *dict;
    if (dict) return dict;
    Dict *tmp = make_string_dict();
#define KEYWORD(id_, str_) \
    dict_put(tmp, to_string(str_), (void *)id_);
#define PUNCT(id_, str_)
# include "keyword.h"
#undef PUNCT
#undef KEYWORD
    dict = tmp;
    return dict;
}

static Dict *punct_dict(void) {
    static Dict *dict;
    if (dict) return dict;
    Dict *tmp = make_string_dict();
#define KEYWORD(k, s)
#define PUNCT(k, s) \
    dict_put(tmp, to_string(s), (void *)k);
# include "keyword.h"
#undef PUNCT
#undef KEYWORD
    dict = tmp;
    return dict;
}

static Token *to_keyword_maybe(Token *tok) {
    ASSERT(tok->toktype == TOKTYPE_IDENT);
    int id = (intptr)dict_get(keyword_dict(), tok->val.str);
    if (id) {
        Token *r = copy_token(tok);
        r->toktype = TOKTYPE_KEYWORD;
        r->val.i = id;
        return r;
    }
    return tok;
}

static Token *cppnum_to_float(Token *tok) {
    Token *r = copy_token(tok);
    r->toktype = TOKTYPE_FLOAT;
    r->val.f = atof(STRING_BODY(tok->val.str));
    return r;
}

static Token *cppnum_to_int(Token *tok) {
    char *p = STRING_BODY(tok->val.str);
    int base = 10;
    int val = 0;
    // Read prefix such as "0" or "0x".
    if (*p == '0') {
        p++;
        if (*p == 'x' || *p == 'X') {
            base = 16;
            p++;
        } else if (*p == 'b' || *p == 'B') {
            // Binary constant using '0b' prefix is GNU extension
            base = 2;
            p++;
        } else {
            base = 8;
        }
    }
    // Read numbers until non-number character.
    for (; *p; p++) {
        int v;
        if ('0' <= *p && *p <= '9')
            v = *p - '0';
        else if ('a' <= *p && *p <= 'f')
            v = *p - 'a' + 10;
        else if ('A' <= *p && *p <= 'F')
            v = *p - 'A' + 10;
        else
            break;
        if (v >= base)
            error_token(tok, "invalid digit '%c' in base %d number", *p, base);
        val *= base;
        val += v;
    }
    // Ignore all suffixes for now
    while (*p == 'U' || *p == 'u' || *p == 'L' || *p == 'l')
        p++;
    if (*p)
        error_token(tok, "invalid char '%c' in a number '%s'", *p, STRING_BODY(tok->val.str));

    Token *r = copy_token(tok);
    r->toktype = TOKTYPE_INT;
    r->val.i = val;
    return r;
}

static Token *cppnum_to_num(Token *tok) {
    return strchr(STRING_BODY(tok->val.str), '.')
        ? cppnum_to_float(tok)
        : cppnum_to_int(tok);
}

static Token *cpp_token_to_token(Token *tok) {
    if (!tok)
        return NULL;
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
    ASSERT(tok->toktype == TOKTYPE_CHAR || tok->toktype == TOKTYPE_STRING);
    return tok;
}

/*==============================================================================
 * C99 6.10.8 Predefined macro names.
 */

static void handle_pragma(CppContext *ctx);

static void def_obj_macro(CppContext *ctx, char *name, Token *tok) {
    List *list = make_list1(tok);
    dict_put(ctx->defs, to_string(name), make_obj_macro(list));
}

static void def_special_macro(CppContext *ctx, char *name, special_macro_handler *fn) {
    dict_put(ctx->defs, to_string(name), make_special_macro(fn));
}

/*
 * Returns a struct tm representing now.  The result is cached in the context.
 */
static struct tm *get_tm(CppContext *ctx) {
    if (ctx->tm)
        return ctx->tm;
    time_t timet = time(NULL);
    struct tm *now = malloc(sizeof(struct tm));
    localtime_r(&timet, now);
    ctx->tm = now;
    return now;
}

static void handle_date_macro(CppContext *ctx, Token *tmpl) {
    Token *tok = copy_token(tmpl);
    tok->toktype = TOKTYPE_STRING;
    char *month[] = { "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec" };
    struct tm *now = get_tm(ctx);
    tok->val.str = make_string_printf("%s %02d %04d", month[now->tm_mon], now->tm_mday, 1900 + now->tm_year);
    unget_cpp_token(ctx, tok);
}

static void handle_time_macro(CppContext *ctx, Token *tmpl) {
    Token *tok = copy_token(tmpl);
    tok->toktype = TOKTYPE_STRING;
    struct tm *now = get_tm(ctx);
    tok->val.str = make_string_printf("%02d:%02d:%02d", now->tm_hour, now->tm_min, now->tm_sec);
    unget_cpp_token(ctx, tok);
}

static void handle_file_macro(CppContext *ctx, Token *tmpl) {
    Token *tok = copy_token(tmpl);
    tok->toktype = TOKTYPE_STRING;
    tok->val.str = ctx->file->filename;
    unget_cpp_token(ctx, tok);
}

static void handle_line_macro(CppContext *ctx, Token *tmpl) {
    Token *tok = copy_token(tmpl);
    tok->toktype = TOKTYPE_CPPNUM;
    tok->val.str = make_string_printf("%d", ctx->file->line);
    unget_cpp_token(ctx, tok);
}

/*
 * C99 6.10.9 Pragma operator.
 *
 * _Pragma("tokens ...") is equivalent to #pragma tokens ....
 */
static void handle_pragma_macro(CppContext *ctx, Token *ignore) {
    Token *tok = read_cpp_token(ctx);
    if (!is_punct(tok, '('))
        error_token(tok, "'(' expected, but got '%s'", token_to_string(tok));
    Token *body = read_cpp_token(ctx);
    if (body->toktype != TOKTYPE_STRING)
        error_token(body, "string expected, but got '%s'", token_to_string(body));
    tok = read_cpp_token(ctx);
    if (!is_punct(tok, ')'))
        error_token(tok, "')' expected, but got '%s'", token_to_string(tok));

    File *file = make_string_file(body->val.str);
    do_include(ctx, file);
    handle_pragma(ctx);
}

void define_predefined_macros(CppContext *ctx) {
    def_obj_macro(ctx, "__8CC__", make_cppnum(ctx, to_string("1")));
    def_obj_macro(ctx, "__STDC__", make_cppnum(ctx, to_string("1")));
    def_obj_macro(ctx, "__STDC_HOSTED__", make_cppnum(ctx, to_string("1")));
    def_obj_macro(ctx, "__STDC_VERSION__", make_cppnum(ctx, to_string("199901L")));
    def_special_macro(ctx, "__DATE__", handle_date_macro);
    def_special_macro(ctx, "__TIME__", handle_time_macro);
    def_special_macro(ctx, "__FILE__", handle_file_macro);
    def_special_macro(ctx, "__LINE__", handle_line_macro);
    def_special_macro(ctx, "_Pragma", handle_pragma_macro);
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

static Token *expand_one(CppContext *ctx);

static CppContext *make_virt_cpp_context(CppContext *ctx, List *tokens) {
    CppContext *r = make_cpp_context(NULL);
    r->at_bol = false;
    r->defs = ctx->defs;
    r->ungotten = list_reverse(tokens);
    r->in_macro = true;
    return r;
}

/*
 * Expands all macros in a given token list, as if they consisted the rest of
 * the source file.  A new preprocessing contexts are created, and the tokens
 * are pushed to the context so that subsequent read_cpp_token() will get them,
 * as if these were the content of a file.
 *
 * expand_one() and expand_all() calls each other to get the fully expanded form
 * of the given tokens.
 */
static List *expand_all(CppContext *ctx, List *ts) {
    List *r = make_list();
    CppContext *virt = make_virt_cpp_context(ctx, ts);
    Token *tok;
    while ((tok = expand_one(virt)) != NULL)
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
 * (C99 6.10.3 Macro replacement, sentence 10)
 */
static List *read_args_int(CppContext *ctx, Macro *macro) {
    List *r = make_list();
    List *arg = make_list();
    int depth = 0;

    Token *tok = peek_cpp_token(ctx);
    if (!tok || !is_punct(tok, '('))
        return NULL;
    read_cpp_token(ctx);

    for (Token *tok1 = read_cpp_token(ctx); ; tok1 = read_cpp_token(ctx)) {
        if (!tok1)
            error_token(tok, "unterminated macro argument list");
        if (tok1->toktype == TOKTYPE_NEWLINE)
            continue;
        if (depth) {
            if (is_punct(tok1, ')'))
                depth--;
            list_push(arg, tok1);
            continue;
        }
        if (is_punct(tok1, '('))
            depth++;
        if (is_punct(tok1, ')')) {
            unget_cpp_token(ctx, tok1);
            list_push(r, arg);
            return r;
        }
        bool in_threedots = macro->is_varg && LIST_LEN(r) + 1 == macro->nargs;
        if (is_punct(tok1, ',') && !in_threedots) {
            list_push(r, arg);
            arg = make_list();
            continue;
        }
        list_push(arg, tok1);
    }
}

/*
 * Reads macro arguments.  If the number of macro arguments does not match with
 * the number of parameters, it will raise an error.
 */
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

/*
 * Added a given hide set to tokens in a given list.
 */
static List *add_hide_set(List *tokens, List *hideset) {
    List *r = make_list();
    for (int i = 0; i < LIST_LEN(tokens); i++) {
        Token *t = copy_token((Token *)LIST_REF(tokens, i));
        t->hideset = list_union(t->hideset, hideset);
        list_push(r, t);
    }
    return r;
}

/*
 * Writes a string representation of a given character to a buffer.  If the
 * character is quote, it'll be esacaped with backslash.
 */
static void stringize_char(String *b, char c, char quote) {
    if (!isascii(c))
        string_printf(b, "\\x%02x", (u8)c);
    else if (c == '\\' || c == quote)
        string_printf(b, "\\%c", c);
    else
        string_printf(b, "%c", c);
}

static void paste(String *b, Token *tok) {
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
static Token *glue_tokens(Token *t0, Token *t1) {
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

/*
 * Joins a given token with the last token of a list.
 */
static void glue_push(List *ls, Token *tok) {
    ASSERT(!LIST_IS_EMPTY(ls));
    Token *last = list_pop(ls);
    list_push(ls, glue_tokens(last, tok));
}

/*
 * Join tokens in a given list.  If sep is true, a space is put
 * between tokens.  If false, no separator.
 */
static String *join_tokens(List *arg, bool sep) {
    String *s = make_string();
    for (int i = 0; i < LIST_LEN(arg); i++) {
        Token *tok = LIST_REF(arg, i);
        if (sep && STRING_LEN(s) && tok->space)
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
    return s;
}

/*
 * Write a string representation of a given token sequence.  Used by
 * # operator.
 */
static Token *stringize(Token *tmpl, List *arg) {
    Token *r = copy_token(tmpl);
    r->toktype = TOKTYPE_STRING;
    r->val.str = join_tokens(arg, true);
    return r;
}

/*
 * Substitutes parameters in macro definition body with actual arguments.
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
static Token *expand_one(CppContext *ctx) {
    Token *tok = read_cpp_token(ctx);
    if (!tok) return NULL;
    if (tok->toktype != TOKTYPE_IDENT)
        return tok;
    String *name = tok->val.str;
    Macro *macro = dict_get(ctx->defs, name);
    if (!macro)
        return tok;
    if (list_in(tok->hideset, name))
        return tok;

    switch (macro->type) {
    case MACRO_OBJ: {
        List *ts = subst(ctx, macro, make_list(), list_union1(tok->hideset, name));
        pushback(ctx, ts);
        return expand_one(ctx);
    }
    case MACRO_FUNC: {
        List *args = read_args(ctx, macro);
        Token *rparen = read_cpp_token(ctx);
        List *hideset = list_union1(list_intersect(tok->hideset, rparen->hideset), name);
        List *ts = subst(ctx, macro, args, hideset);
        pushback(ctx, ts);
        return expand_one(ctx);
    }
    case MACRO_SPECIAL:
        macro->fn(ctx, tok);
        return expand_one(ctx);
    }
    panic("should not reach here");
}

/*==============================================================================
 * Preprocessor directives.
 */

static bool is_defined(CppContext *ctx, Token *tok) {
    if (!tok || tok->toktype != TOKTYPE_IDENT)
        error_token(tok, "identifier expected, but got '%s'", token_to_string(tok));
    return dict_has(ctx->defs, tok->val.str);
}

/*
 * Reads "defined" unary operator of the form "defined <identifier>" or
 * "defined(<identifier>)".  The token "defined" is already read when the
 * function is called.
 *
 * (C99 6.10.1 Conditional inclusion, paragraph 1)
 */
static Token *read_defined(CppContext *ctx) {
    Token *tok = read_cpp_token(ctx);
    if (is_punct(tok, '(')) {
        tok = read_cpp_token(ctx);
        Token *tok1 = read_cpp_token(ctx);
        if (!tok1 || !is_punct(tok1, ')'))
            error_token(tok1, "')' expected, but got '%s'", token_to_string(tok1));
    }
    Token *r = copy_token(tok);
    r->toktype = TOKTYPE_CPPNUM;
    r->val.i = is_defined(ctx, tok);
    return r;
}

/*
 * Evaluate a given tokens as an integer constant expression and returns the
 * result.
 */
static int eval_const_expr(CppContext *cppctx, List *tokens) {
    if (LIST_LEN(tokens) == 1 && ((Token *)LIST_REF(tokens, 0))->toktype == TOKTYPE_CPPNUM)
        return ((Token *)LIST_REF(tokens, 0))->val.i;

    CppContext *virt = make_virt_cpp_context(cppctx, tokens);
    ReadContext *readctx = make_read_context(cppctx->file, NULL, virt);
    Var *var = read_comma_expr(readctx);

    Token *tok = read_token(readctx);
    if (tok)
        error_token(tok, "newline expected, but got '%s'", token_to_string(tok));

    ASSERT(var->stype == VAR_IMM);
    if (!ctype_equal(var->ctype, CTYPE_INT))
        error_cpp_ctx(cppctx, "integer expected");
    return var->val.i;
}

/*
 * Reads an constant expression for #if directive.  In preprocessor constant
 * expression, all undefined identifiers are replaced with 0.
 *
 * (C99 6.10.1 Conditional inclusion, paragraph 4)
 */
static int read_constant_expr(CppContext *ctx) {
    List *tokens = make_list();
    for (;;) {
        Token *tok = expand_one(ctx);
        if (!tok || tok->toktype == TOKTYPE_NEWLINE)
            break;
        if (tok->toktype == TOKTYPE_IDENT && !strcmp("defined", STRING_BODY(tok->val.str)))
            tok = read_defined(ctx);
        list_push(tokens, tok);
    }
    return eval_const_expr(ctx, tokens);
}

/*
 * #ifdef
 * (C99 6.10.1 Conditional inclusion, paragraph 5)
 */
static int read_ifdef(CppContext *ctx) {
    int r = is_defined(ctx, read_cpp_token(ctx));
    expect_newline(ctx);
    return r;
}

/*
 * Handles #if, #elif, #ifdef and #ifndef.  If condition does not meet, the
 * function calls skip_cond_include(), defined in lex.c, to skip all tokens
 * until the next #elif, #else of #endif.
 *
 * (C99 6.10.1 Conditional inclusion)
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

/*
 * Reads function-like macro body.  Macro body is a sequence of tokens ends with
 * a newline.
 *
 * Macro parameters in the body will be replaced with a special token whose type
 * is TOKTYPE_MACRO_PARAM.  When macro is executed, these tokens will then be
 * replaced with macro arguments.
 */
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
 * Stores a given macro to a CppContex.
 *
 * TODO: Print warning message if a macro is redefined.  Redefinition is valid
 * only when the a one is the same as the old one. (C99 6.10.3p2)
 */

static void store_macro(CppContext *ctx, String *name, Macro *macro) {
    dict_put(ctx->defs, name, macro);
}

/*
 * Reads function-like macro definition.
 */
static void read_funclike_define(CppContext *ctx, String *name) {
    Dict *param = make_string_dict();
    bool is_varg = read_funclike_define_args(ctx, param);
    List *body = read_funclike_define_body(ctx, param);
    store_macro(ctx, name, make_func_macro(body, dict_size(param), is_varg));
}

/*
 * #define
 * (C99 6.10.3 Macro replacement)
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
    store_macro(ctx, name->val.str, make_obj_macro(body));
}

/*
 * #undef
 * (C99 6.10.5 Scope of macro definisions, paragraph 2)
 */
static void read_undef(CppContext *ctx) {
    Token *name = read_cpp_token(ctx);
    if (!name || name->toktype != TOKTYPE_IDENT)
        error_token(name, "undef works only to an identifier, but got '%s'", token_to_string(name));
    expect_newline(ctx);
    dict_delete(ctx->defs, name->val.str);
}


/*
 * Reads a file name of #include directive.  If the file name is quoted with <>,
 * "std" will set to true.  If quoted with doublequote, set to false.  We use
 * expand_one() rather than read_cpp_token(), because macros are allowed to be
 * used in #include.
 * (C99 6.10.2 Source file inclusion)
 */
static void read_cpp_header_name(CppContext *ctx, String **name, bool *std) {
    if (LIST_IS_EMPTY(ctx->ungotten)) {
        *name = read_header_name(ctx, std);
        if (name)
            return;
    }

    Token *tok = expand_one(ctx);
    if (!tok || tok->toktype == TOKTYPE_NEWLINE)
        error_token(tok, "expected file name, but got '%s'", token_to_string(tok));
    if (tok->toktype == TOKTYPE_STRING) {
        *name = tok->val.str;
        *std = false;
        return;
    }
    List *tokens = make_list();
    if (is_punct(tok, '<')) {
        for (;;) {
            Token *tok = expand_one(ctx);
            if (!tok || tok->toktype == TOKTYPE_NEWLINE)
                error_token(tok, "premature end of header name");
            if (is_punct(tok, '>'))
                break;
            list_push(tokens, tok);
        }
        *name = join_tokens(tokens, false);
        *std = true;
        return;
    }
    error_token(tok, "'<' expected, but got '%s'", token_to_string(tok));
}

/*
 * Constructs a file path by joining path0 and path1.
 */
static String *construct_path(String *path0, String *path1) {
    char *s0 = STRING_BODY(path0);
    char *s1 = STRING_BODY(path1);
    if (!*s0)
        return path1;
    return make_string_printf("%s/%s", s0, s1);
}

/*
 * Find a header file for a given header name.  If header was quoted
 * with <>, list "path" would include "/usr/include" and
 * "/usr/local/include".  Otherwise just "".
 */

static File *open_header(CppContext *ctx, String *name, List *paths) {
    for (int i = 0; i < LIST_LEN(paths); i++) {
        String *path = construct_path((String *)LIST_REF(paths, i), name);
        FILE *stream = fopen(STRING_BODY(path), "r");
        if (!stream)
            continue;
        return make_file(stream, STRING_BODY(path));
    }
    error_cpp_ctx(ctx, "Cannot find header: '%s'", STRING_BODY(name));
}

/*
 * #include
 * (C99 6.10.2 Source file inclusion)
 */
static void handle_include(CppContext *ctx) {
    String *name;
    bool std;
    read_cpp_header_name(ctx, &name, &std);
    expect_newline(ctx);

    List *include_path = std
        ? ctx->include_path
        : make_list1(to_string(""));
    File *file = open_header(ctx, name, include_path);
    do_include(ctx, file);
}

/*
 * #line
 * (C99 6.10.4 Line control)
 *
 * Line directive must be one of the following form in macro-expanded form:
 *
 *     #line digit-sequence
 *     #line digit-sequence "s-char-sequenceopt"
 */
static void handle_line_directive(CppContext *ctx) {
    Token *tok = expand_one(ctx);
    if (!tok || tok->toktype != TOKTYPE_CPPNUM)
        error_token(tok, "number expected, but got '%s'", token_to_string(tok));
    int line = cppnum_to_num(tok)->val.i;

    tok = expand_one(ctx);
    if (tok && tok->toktype == TOKTYPE_NEWLINE) {
        ctx->file->line = line;
        return;
    }
    if (tok && tok->toktype == TOKTYPE_STRING) {
        expect_newline(ctx);
        ctx->file->line = line;
        ctx->file->filename = tok->val.str;
        return;
    }
    error_token(tok, "filename expected, but got '%s'", token_to_string(tok));
}

/*
 * #pragma
 * (C99 6.10.5 6.10.6 Pragma directive)
 *
 * No pragmas including standard C's are not supported for now.
 */
static void handle_pragma(CppContext *ctx) {
    error_cpp_ctx(ctx, "No pragmas supported");
}

/*
 * #error
 * (C99 6.10.5 Error directive)
 */
static void read_error_directive(CppContext *ctx, Token *define) {
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
    if (read_if(ctx, "define"))       read_define(ctx);
    else if (read_if(ctx, "undef"))   read_undef(ctx);
    else if (read_if(ctx, "if"))      handle_cond_incl(ctx, COND_IF);
    else if (read_if(ctx, "elif"))    handle_cond_incl(ctx, COND_ELIF);
    else if (read_if(ctx, "else"))    handle_cond_incl(ctx, COND_ELSE);
    else if (read_if(ctx, "ifdef"))   handle_cond_incl(ctx, COND_IFDEF);
    else if (read_if(ctx, "ifndef"))  handle_cond_incl(ctx, COND_IFNDEF);
    else if (read_if(ctx, "endif"))   handle_cond_incl(ctx, COND_ENDIF);
    else if (read_if(ctx, "include")) handle_include(ctx);
    else if (read_if(ctx, "line"))    handle_line_directive(ctx);
    else if (read_if(ctx, "pragma"))  handle_pragma(ctx);
    else if ( (tok = read_if(ctx, "error")) ) {
        read_error_directive(ctx, tok);
    } else {
        tok = read_cpp_token(ctx);
        if (tok && tok->toktype == TOKTYPE_NEWLINE)
            // 6.10.7 NULL directive.  Do nothing.
            return;
        error_token(tok, "unsupported preprocessor directive: '%s'", token_to_string(tok));
    }
}

/*==============================================================================
 * -E option.
 *
 * write_cpp() writes preprocessed tokens to a given file.  This is useful for
 * debugging.
 */

static Token *read_token_int(CppContext *ctx) {
    for (;;) {
        Token *tok = read_cpp_token(ctx);
        if (!tok)
            return NULL;
        if (tok->toktype == TOKTYPE_NEWLINE) {
            ctx->at_bol = true;
            return tok;
        }
        if (ctx->at_bol && is_punct(tok, '#')) {
            read_directive(ctx);
            ctx->at_bol = true;
            continue;
        }
        ctx->at_bol = false;
        unget_cpp_token(ctx, tok);
        return expand_one(ctx);
    }
}

static void write_cpp_token(FILE *out, Token *tok, bool is_bol) {
    if (!is_bol && tok->space)
        fprintf(out, " ");
    switch (tok->toktype) {
    case TOKTYPE_IDENT:
    case TOKTYPE_CPPNUM:
        fprintf(out, "%s", STRING_BODY(tok->val.str));
        break;
    case TOKTYPE_PUNCT:
        fprintf(out, "%s", token_to_string(tok));
        break;
    case TOKTYPE_CHAR: {
        String *b = make_string();
        stringize_char(b, tok->val.i, '\'');
        fprintf(out, "'%s'", STRING_BODY(b));
        break;
    }
    case TOKTYPE_STRING: {
        fprintf(out, "\"");
        for (char *p = STRING_BODY(tok->val.str); *p; p++) {
            String *b = make_string();
            stringize_char(b, *p, '\"');
            fprintf(out, "'%s'", STRING_BODY(b));
        }
        fprintf(out, "\"");
        break;
    }
    default:
        panic("invalid token type: %d", tok->toktype);
    }
}

void cpp_write(CppContext *ctx, FILE *out) {
    bool is_bol = true;
    for (;;) {
        Token *tok = read_token_int(ctx);
        if (!tok) return;
        if (tok->toktype == TOKTYPE_NEWLINE) {
            fprintf(out, "\n");
            is_bol = true;
            continue;
        }
        write_cpp_token(out, tok, is_bol);
        is_bol = false;
    }
}

/*==============================================================================
 * Entry function of the preprocessor.
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

    for (;;) {
        Token *tok = read_token_int(readctx->cppctx);
        if (!tok) return NULL;
        if (tok->toktype == TOKTYPE_NEWLINE)
            continue;
        return cpp_token_to_token(tok);
    }
}
