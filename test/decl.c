/*
 * Copyright 2010 Rui Ueyama <rui314@gmail.com>.  All rights reserved.
 * This code is available under the simplified BSD license.  See LICENSE for details.
 */

#include "unittest.h"
#include "../decl.c"

#define TEST_GUESS(type, input) \
    EQ(type, guess_decl_type(mkctx(input)))

TEST(guess_decl_type) {
    TEST_GUESS(DECL_NONE, ";");

    TEST_GUESS(DECL_VAR, "a;");
    TEST_GUESS(DECL_VARABST, "*;");
    TEST_GUESS(DECL_VARABST, "* const volatile restrict * const * volatile * restrict;");
    TEST_GUESS(DECL_VARABST, "(*);");

    TEST_GUESS(DECL_ARRAY, "a[];");
    TEST_GUESS(DECL_ARRAY, "*a[];");
    TEST_GUESS(DECL_ARRAYABST, "[];");
    TEST_GUESS(DECL_ARRAYABST, "*[];");

    TEST_GUESS(DECL_FUNCDECL, "a(void);");
    TEST_GUESS(DECL_FUNCDECL, "a(int a);");
    TEST_GUESS(DECL_FUNCDECL, "a(int, float);");
    TEST_GUESS(DECL_FUNCDECL, "a(a{[]()}, float);");
    TEST_GUESS(DECL_FUNCDEF, "a(void) {");
    TEST_GUESS(DECL_FUNCABST, "(*)(void);");

    TEST_GUESS(DECL_OLD_FUNCDECL, "a();");
    TEST_GUESS(DECL_OLD_FUNCDECL, "a(p);");
    TEST_GUESS(DECL_OLD_FUNCDECL, "a(p,q,r);");
    TEST_GUESS(DECL_OLD_FUNCDEF, "a(a) {");
    TEST_GUESS(DECL_OLD_FUNCDEF, "a(a) int a; {");
    TEST_GUESS(DECL_OLD_FUNCABST, "(*)();");
}

#define TEST_VAR_DECL(type, input) \
    EQ_STRING(type, pp_type(read_var_decl(mkctx(input), make_int_type(IINT), &token)))

TEST(read_var_decl) {
    Token *token;
    TEST_VAR_DECL("int", "a");
    TEST_VAR_DECL("int*", "*b");
    TEST_VAR_DECL("int*", "(*c)");
    TEST_VAR_DECL("int*", "(*(d))");
}

#define TEST_VAR_ABST_DECL(type, input) \
    EQ_STRING(type, pp_type(read_var_abst_decl(mkctx(input), make_int_type(IINT))))

TEST(read_var_abst_decl) {
    TEST_VAR_ABST_DECL("int", ";");
    TEST_VAR_ABST_DECL("int*", "*;");
    TEST_VAR_ABST_DECL("int*", "(*);");
    TEST_VAR_ABST_DECL("int**", "(*(*));");
}

