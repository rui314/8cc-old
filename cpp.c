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
    if (tok->toktype == TOKTYPE_IDENT && !strcmp(STRING_BODY(tok->val.str), str))
        return tok;
    unget_cpp_token(ctx, tok);
    return NULL;
}

static bool read_punct_if(CppContext *ctx, int v) {
    Token *tok = read_cpp_token(ctx);
    if (tok->toktype == TOKTYPE_PUNCT && tok->val.i == v)
        return true;
    unget_cpp_token(ctx, tok);
    return false;
}

static bool is_punct(Token *tok, int v) {
    return tok->toktype == TOKTYPE_PUNCT && tok->val.i == v;
}

/*==============================================================================
 * Data structure representing a macro
 */

typedef Token *special_macro_handler(CppContext *ctx);

// Object-like macro, function-like macro and special macro
// (e.g. __FILE__ or __LINE__).
#define MACRO_OBJ     1
#define MACRO_FUNC    2
#define MACRO_SPECIAL 3

typedef struct Macro {
    int type;
    // For object-like and function-like
    int nargs;
    List *repl;
    // For special macros
    special_macro_handler *fn;
} Macro;

static Macro *make_obj_macro(List *repl) {
    Macro *r = malloc(sizeof(Macro));
    r->type = MACRO_OBJ;
    r->repl = repl;
    return r;
}

static Macro *make_func_macro(int nargs, List *repl) {
    Macro *r = malloc(sizeof(Macro));
    r->type = MACRO_FUNC;
    r->nargs = nargs;
    r->repl = repl;
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
#define OP(_)
# include "keyword.h"
#undef OP
#undef KEYWORD
    return dict;
}

static Token *ident_to_keyword(Token *tok) {
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
    if (tok->toktype == TOKTYPE_IDENT)
        return ident_to_keyword(tok);
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
    char buf[20];
    sprintf(buf, "%s %02d %04d", month[now->tm_mon], now->tm_mday, 1900 + now->tm_year);
    return make_str_literal(ctx, to_string(buf));
}

static Token *macro_time(CppContext *ctx, struct tm *now) {
    char buf[10];
    sprintf(buf, "%02d:%02d:%02d", now->tm_hour, now->tm_min, now->tm_sec);
    return make_str_literal(ctx, to_string(buf));
}

static Token *handle_file_macro(CppContext *cpp) {
    return make_str_literal(cpp, cpp->file->filename);
}

static Token *handle_line_macro(CppContext *cpp) {
    char buf[10];
    sprintf(buf, "%d", cpp->file->line);
    return cppnum_to_num(make_cppnum(cpp, to_string(buf)));
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
 * C preprocessor.
 */

/*
 * Reads function-like macro definition.
 */
static void read_function_like_define(CppContext *ctx, String *name) {
    Dict *param = make_string_dict();
    List *repl = make_list();

    // Read macro pameters
    for (Token *tok = read_cpp_token(ctx);
         tok;
         tok = read_cpp_token(ctx)) {
        if (is_punct(tok, ')'))
            break;
        if (dict_size(param)) {
            if (!is_punct(tok, ','))
                error_token(tok, "',' expected, but got '%s'", token_to_string(tok));
            tok = read_cpp_token(ctx);
        }
        if (!tok || tok->toktype == TOKTYPE_NEWLINE)
            error_token(tok, "missing ')' in macro parameter list");
        if (is_punct(tok, KEYWORD_THREEDOTS)) {
            Token *subst = make_token(ctx, TOKTYPE_MACRO_PARAM, (TokenValue)dict_size(param));
            dict_put(param, tok->val.str, subst);
            Token *tok1 = read_cpp_token(ctx);
            if (!is_punct(tok1, ')'))
                error_token(tok1, "')' expected, but got '%s'", token_to_string(tok1));
            break;
        }
        if (tok->toktype != TOKTYPE_IDENT)
            error_token(tok, "identifier expected, but got '%s'", token_to_string(tok));
        Token *subst = make_token(ctx, TOKTYPE_MACRO_PARAM, (TokenValue)dict_size(param));
        dict_put(param, tok->val.str, subst);
    }

    // Read macro replacement list
    for (Token *tok = read_cpp_token(ctx);
         tok && tok->toktype != TOKTYPE_NEWLINE;
         tok = read_cpp_token(ctx)) {
        if (tok->toktype == TOKTYPE_IDENT) {
            Token *subst = dict_get(param, tok->val.str);
            if (subst) {
                list_push(repl, subst);
                continue;
            }
        }
        list_push(repl, tok);
    }
    dict_put(ctx->defs, name, make_func_macro(dict_size(param), repl));
}

/*
 * Reads arguments of function-like macro invocation.
 * (WG14/N1256 6.10.3 Macro replacement, sentence 10)
 */
static List *read_args(CppContext *ctx) {
    List *r = make_list();
    List *arg = make_list();
    int depth = 0;
    for (Token *tok = read_cpp_token(ctx); ; tok = read_cpp_token(ctx)) {
        if (!tok)
            error_cpp_ctx(ctx, "EOF while reading arguments of macro");
        if (tok->toktype == TOKTYPE_NEWLINE)
            continue;
        if (depth) {
            if (is_punct(tok, ')'))
                depth--;
            list_push(arg, tok);
            continue;
        }
        if (is_punct(tok, ')')) {
            list_push(r, arg);
            break;
        }
        if (is_punct(tok, ',')) {
            list_push(r, arg);
            arg = make_list();
            continue;
        }
        list_push(arg, tok);
    }
    return r;
}

/*
 * Expands a function-like macro.
 * (WG14/N1256 6.10.3 Macro replacement)
 */
static List *subst_args(Macro *macro, Token *tok, List *args) {
    assert(macro->type == MACRO_FUNC);
    if (macro->nargs != LIST_LEN(args))
        error_token(tok, "Parameter number does not match argument list");

    List *r = make_list();
    for (int i = 0; i < LIST_LEN(macro->repl); i++) {
        Token *subst = LIST_REF(macro->repl, i);
        if (subst->toktype == TOKTYPE_MACRO_PARAM)
            list_append(r, LIST_REF(args, subst->val.i));
        else
            list_push(r, subst);
    }

    for (int i = 0; i < LIST_LEN(r); i++)
        LIST_REF(r, i) = cpp_token_to_token(LIST_REF(r, i));
    return r;
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
        read_function_like_define(ctx, name->val.str);
        return;
    }

    List *repl = make_list();
    while (tok && tok->toktype != TOKTYPE_NEWLINE) {
        list_push(repl, tok);
        tok = read_cpp_token(ctx);
    }
    dict_put(ctx->defs, name->val.str, make_obj_macro(repl));
}

/*
 * #error
 * (WG14/N1256 6.10.5 Error directive)
 */
static void read_error_dir(CppContext *ctx, Token *define) {
    String *buf = make_string();
    Token *tok = read_cpp_token(ctx);
    while(tok &&  tok->toktype != TOKTYPE_NEWLINE) {
        o1(buf, ' ');
        string_append(buf, token_to_string(tok));
        tok = read_cpp_token(ctx);
    }
    error_token(define, "error: #error:%s", STRING_BODY(buf));
}

static void read_directive(CppContext *ctx) {
    Token *tok;
    if (read_if(ctx, "define"))
        read_define(ctx);
    else if ( (tok = read_if(ctx, "error")) )
        read_error_dir(ctx, tok);
    else
        error_cpp_ctx(ctx, "'#' must be followed by 'define' for now");
}

Token *read_token(ReadContext *ctx) {
    if (!LIST_IS_EMPTY(ctx->ungotten))
        return list_pop(ctx->ungotten);

    CppContext *cppctx = ctx->cppctx;
    for (;;) {
        Token *tok = read_cpp_token(cppctx);
        if (!tok)
            return NULL;
        if (tok->toktype == TOKTYPE_NEWLINE) {
            cppctx->at_bol = true;
            continue;
        }
        if (cppctx->at_bol && is_punct(tok, '#')) {
            read_directive(cppctx);
            cppctx->at_bol = true;
            continue;
        }
        cppctx->at_bol = false;
        if (tok->toktype == TOKTYPE_IDENT) {
            Macro *macro = dict_get(cppctx->defs, tok->val.str);
            if (!macro)
                return cpp_token_to_token(tok);
            switch (macro->type) {
            case MACRO_OBJ:
                for (int i = LIST_LEN(macro->repl) - 1; i >= 0; i--)
                    unget_cpp_token(cppctx, (Token *)LIST_REF(macro->repl, i));
                break;
            case MACRO_FUNC: {
                if (!read_punct_if(cppctx, '('))
                    return cpp_token_to_token(tok);
                List *repl = subst_args(macro, tok, read_args(cppctx));
                for (int i = LIST_LEN(repl) - 1; i >= 0; i--)
                    unget_token(ctx, LIST_REF(repl, i));
                return read_token(ctx);
            }
            case MACRO_SPECIAL:
                return macro->fn(cppctx);
            }
            continue;
        }
        return cpp_token_to_token(tok);
    }
}
