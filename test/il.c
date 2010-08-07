/*
 * Copyright 2010 Rui Ueyama <rui314@gmail.com>.  All rights reserved.
 * This code is available under the simplified BSD license.  See LICENSE for details.
 */

#include "unittest.h"
#include "../il.c"

#define A(type) make_array_type(type, NULL)
#define P(type) make_ptr_type(type)
#define F(type) make_func_type(type, make_list())
#define F1(type, arg0) make_func_type(type, make_list1(arg0))
#define F2(type, arg0, arg1) make_func_type(type, make_list2(arg0, arg1))

#define INT get_int_type(SINT)
#define VOID get_void_type()

TEST(pp_type) {
    EQ_STRING("int", pp_type(INT));
    EQ_STRING("unsigned int", pp_type(get_int_type(UINT)));

    EQ_STRING("float", pp_type(get_float_type(FLOAT)));
    EQ_STRING("double", pp_type(get_float_type(DOUBLE)));

    EQ_STRING("void", pp_type(VOID));

    EQ_STRING("int*", pp_type(P(INT)));
    EQ_STRING("float*", pp_type(P(get_float_type(FLOAT))));
    EQ_STRING("void**", pp_type(P(P(get_void_type()))));
}

#define TEST_VAR(expect, type)                                          \
    EQ_STRING((expect), pp_var(make_nvar(LOCAL, (type), to_string("x"))))

TEST(pp_var) {
    TEST_VAR("int x", INT);
    TEST_VAR("int*x", P(INT));
    TEST_VAR("int x[]", A(INT));
    TEST_VAR("int(*x)[]", P(A(INT)));
    TEST_VAR("int(*x[])(void)", A(P(F(INT))));
    TEST_VAR("int x(void)", F(INT));
    TEST_VAR("int(*x)(void)", P(F(INT)));
    TEST_VAR("int*x(void)", F(P(INT)));
    TEST_VAR("int x(int)", F1(INT, INT));
    TEST_VAR("int x(int,int)", F2(INT, INT, INT));
    TEST_VAR("int x(int(*)(void))", F1(INT, P(F(INT))));
    TEST_VAR("void(*x(void))(int)", F(P(F1(VOID, INT))));
    TEST_VAR("void(*x(int,void(*)(int)))(int)",
             F2(P(F1(VOID, INT)), INT, P(F1(VOID, INT))));
}

#define ONE    make_const_int(INT, 1)
#define TWO    make_const_int(INT, 2)
#define THREE  make_const_int(INT, 3)
#define VARX   make_nvar(LOCAL, INT, to_string("x"))

#define BIN(op, e0, e1)                         \
    make_binop_exp(INT, (op), (e0), (e1))
#define ADDR(type, exp)                         \
    make_addrof_exp((type), (LvalExp *)(exp))
#define LVALVAR(type, var)                              \
    make_lval_exp(type, (LvalBase){ LVAL_VAR, var })
#define LVALMEM(type, var)                              \
    make_lval_exp(type, (LvalBase){ LVAL_MEM, var })

#define TEST_EXP(expect, exp)                   \
    EQ_STRING((expect), pp_exp(exp))

TEST(pp_exp) {
    TEST_EXP("1", ONE);
    TEST_EXP("1+2", BIN('+', ONE, TWO));
    TEST_EXP("(1+2)*3", BIN('*', BIN('+', ONE, TWO), THREE));
    TEST_EXP("&x", ADDR(P(INT), LVALVAR(INT, VARX)));
}
