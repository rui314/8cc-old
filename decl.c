/*
 * decl.c - C declaration parser
 *
 * Copyright 2010 Rui Ueyama <rui314@gmail.com>.  All rights reserved.
 * This code is available under the simplified BSD license.  See LICENSE for details.
 */

#include "8cc.h"

/*==============================================================================
 * Functions to detect declaration types.
 *
 * Declaration grammer in C is not easy to parse.  One reason why this is so
 * complex is because declaration of varaible, function declaration and function
 * definition shares prefix.  Until you see '{' or '(', you cannot say you're
 * parsing variable, function declaration or definition.
 *
 * We parse declarations in two pass.  In the first pass, guess_decl_type() is
 * called to determine the type of the declaration.  In the second pass, the
 * actual parsing function is called to parse the declaration according to the
 * type.  Separating the parsing process in two phase makes parsing function
 * simple.
 */

typedef enum DeclType {
    DECL_NONE,
    DECL_VAR,
    DECL_VARABST,
    DECL_ARRAY,
    DECL_ARRAYABST,
    DECL_FUNCDECL,
    DECL_FUNCDEF,
    DECL_FUNCABST,
    DECL_OLD_FUNCDECL,
    DECL_OLD_FUNCDEF,
    DECL_OLD_FUNCABST,
} DeclType;

static bool skip_pointer_opt(ReadContext *ctx) {
    Token *tok = read_token_nonnull(ctx);
    bool r = false;
    while (is_keyword(tok, KEYWORD_CONST)
           || is_keyword(tok, KEYWORD_RESTRICT)
           || is_keyword(tok, KEYWORD_VOLATILE)
           || is_keyword(tok, KEYWORD_INLINE)
           || is_keyword(tok, '*')){
        tok = read_token_nonnull(ctx);
        r = true;
    }
    unget_token(ctx, tok);
    return r;
}

static bool is_open_paren(Token *tok) {
    return is_keyword(tok, '(')
        || is_keyword(tok, '[')
        || is_keyword(tok, '{');
}

static bool is_close_paren(Token *tok) {
    return is_keyword(tok, ')')
        || is_keyword(tok, ']')
        || is_keyword(tok, '}');
}

static bool guess_param(ReadContext *ctx) {
    int depth = 0;
    bool ansi = false;
    for (;;) {
        Token *tok = read_token_nonnull(ctx);
        if (is_close_paren(tok)) {
            depth--;
            if (depth < 0)
                return ansi;
        } else if (is_open_paren(tok)) {
            depth++;
            continue;
        } else if (tok->toktype == TOKTYPE_IDENT) {
            Token *tok1 = read_token_nonnull(ctx);
            if (is_keyword(tok1, ','))
                continue;
            if (is_keyword(tok1, ')')) {
                unget_token(ctx, tok1);
                continue;
            }
        }
        ansi = true;
    }
}

static DeclType guess_func_type(ReadContext *ctx, bool isabst) {
    bool ansi = guess_param(ctx);
    Token *tok = read_token_nonnull(ctx);
    if (is_keyword(tok, '{'))
        return ansi ? DECL_FUNCDEF : DECL_OLD_FUNCDEF;
    if (!ansi && is_type_keyword(tok))
        return DECL_OLD_FUNCDEF;
    return ansi
        ? (isabst ? DECL_FUNCABST : DECL_FUNCDECL)
        : (isabst ? DECL_OLD_FUNCABST : DECL_OLD_FUNCDECL);
}

static DeclType guess_decl_type1(ReadContext *ctx, bool isabst) {
    Token *tok = read_token_nonnull(ctx);
    if (is_keyword(tok, '['))
        return isabst ? DECL_ARRAYABST : DECL_ARRAY;
    if (is_keyword(tok, '('))
        return guess_func_type(ctx, isabst);
    return isabst ? DECL_VARABST : DECL_VAR;
}

static DeclType guess_decl_type(ReadContext *ctx) {
    bool isptr = skip_pointer_opt(ctx);

    // Skip parenthesized declarator or identifier.
    Token *tok = read_token_nonnull(ctx);
    if (tok->toktype == TOKTYPE_IDENT)
        return guess_decl_type1(ctx, false);
    if (is_keyword(tok, '(')) {
        DeclType type = guess_decl_type(ctx);
        parse_expect(ctx, ')');
        switch (type) {
        case DECL_NONE:
            error_ctx(ctx, "malformed declaration");
        case DECL_VAR:
            return guess_decl_type1(ctx, false);
        case DECL_VARABST:
            return guess_decl_type1(ctx, true);
        default:
            return type;
        }
    }
    if (is_keyword(tok, '['))
        return DECL_ARRAYABST;
    unget_token(ctx, tok);
    return isptr ? DECL_VARABST : DECL_NONE;
}

/*==============================================================================
 * Declaration parser context
 */

typedef struct DeclCtx {
    Type *basetype;
    Token **ident;
    Type *(*array)(ReadContext *, Type *);
    Type *(*func)(ReadContext *, Type *);
} DeclCtx;

DeclCtx *make_decl_ctx(Type *base, Token **ident) {
    DeclCtx *r = malloc(sizeof(DeclCtx));
    r->basetype = base;
    r->ident = ident;
    r->array = NULL;
    r->func = NULL;
    return r;
}

/*==============================================================================
 * Variable declaration
 */

static int read_type_qual(ReadContext *ctx) {
    int mask = 0;
    for (;;) {
        Token *tok = read_token_nonnull(ctx);
        if (is_keyword(tok, KEYWORD_CONST)) {
            mask |= QCONST;
        } else if (is_keyword(tok, KEYWORD_RESTRICT)) {
            mask |= QRESTRICT;
        } else if (is_keyword(tok, KEYWORD_VOLATILE)) {
            mask |= QVOLATILE;
        } else {
            unget_token(ctx, tok);
            return mask;
        }
    }
}

static Type *read_decl(ReadContext *ctx, DeclCtx *declctx) {
    if (next_token_is(ctx, '*')) {
        int mask = read_type_qual(ctx);
        Type *ctype0 = read_decl(ctx, declctx);
        Type *ctype1 = make_ptr_type(ctype0);
        return mask ? make_qual_type(ctype1, mask) : ctype1;
    }
    Type *basetype = declctx->basetype;
    if (next_token_is(ctx, '(')) {
        basetype = read_decl(ctx, declctx);
        parse_expect(ctx, ')');
    } else if (declctx->ident) {
        Token *tok = read_token_nonnull(ctx);
        if (tok->toktype != TOKTYPE_IDENT)
            error_token(tok, "identifier expected, but got %s", token_to_string(tok));
        *declctx->ident = tok;
    }
    if (declctx->array && next_token_is(ctx, '['))
        return declctx->array(ctx, basetype);
    if (declctx->func && next_token_is(ctx, '('))
        return declctx->func(ctx, basetype);
    return basetype;
}

static Type *read_var_decl(ReadContext *ctx, Type *base, Token **ident) {
    DeclCtx *declctx = make_decl_ctx(base, ident);
    return read_decl(ctx, declctx);
}

/*==============================================================================
 * Array declaration
 */

static Type *read_array_dimensions(ReadContext *ctx, Type *base) {
    parse_expect(ctx, ']');
    if (next_token_is(ctx, '[')) {
        Type *type = read_array_dimensions(ctx, base);
        return make_array_type(type, NULL);
    }
    return make_array_type(base, NULL);
}

struct Type *read_array_decl(ReadContext *ctx, Type *base, Token **ident) {
    DeclCtx *declctx = make_decl_ctx(base, ident);
    declctx->array = read_array_dimensions;
    return read_decl(ctx, declctx);
}

/*==============================================================================
 * Function declaration
 */

static Type *read_decl_spec(ReadContext *ctx) {
    Token *tok = read_token_nonnull(ctx);
    if (!is_keyword(tok, KEYWORD_INT))
        panic("only 'int' is allowed for now");
    return make_int_type(IINT);
}

static List *fix_param_type(List *param) {
    if (LIST_LEN(param) != 1)
        return param;
    Type *type = LIST_REF(param, 0);
    return type->type == TVOID ? make_list() : param;
}

static Type *read_func_params(ReadContext *ctx, Type *rettype) {
    List *paramtype = make_list();
    for (;;) {
        Type *basetype = read_decl_spec(ctx);
        Type *type = read_var_decl(ctx, basetype, NULL);
        list_push(paramtype, type);
        if (next_token_is(ctx, ')'))
            return make_func_type(rettype, fix_param_type(paramtype));
        parse_expect(ctx, ',');
    }
}

static Type *read_func_decl(ReadContext *ctx, Type *base, Token **ident) {
    DeclCtx *declctx = make_decl_ctx(base, ident);
    declctx->func = read_func_params;
    return read_decl(ctx, declctx);
}
