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

/*
 * C types
 */

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

/*
 * Variables
 */

#define TEST_VAR(expect, type)                                          \
    EQ_STRING((expect), pp_var(make_local_var((type), to_string("x"))))

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

/*
 * Expressions
 */

#define ZERO   make_const_int(INT, 0)
#define ONE    make_const_int(INT, 1)
#define TWO    make_const_int(INT, 2)
#define THREE  make_const_int(INT, 3)
#define VARX   make_local_var(INT, to_string("x"))
#define VARY   make_local_var(INT, to_string("y"))

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

    TEST_EXP("2.000000", make_const_float(get_float_type(DOUBLE), 2));

    LvalExp *lval = LVAL_EXP(LVALVAR(INT, VARX));
    lval->off = make_lval_off(INDEX, THREE);
    TEST_EXP("x[3]", EXP(lval));

    TEST_EXP("x", make_startof_exp(P(INT), LVAL_EXP(LVALVAR(A(INT), VARX))));
    TEST_EXP("sizeof(int*)", make_sizeoftype_exp(INT, P(INT)));
    TEST_EXP("(int*)x", make_cast_exp(P(INT), LVALVAR(INT, VARX)));
}

/*
 * Instructions
 */

TEST(pp_instr) {
    LvalExp *x = LVAL_EXP(LVALVAR(INT, VARX));
    LvalExp *y = LVAL_EXP(LVALVAR(INT, VARY));

    EQ_STRING("x=3;", pp_set_instr(SET_INSTR(make_set_instr(x, THREE))));
    EQ_STRING("x=y(3);", pp_call_instr(CALL_INSTR(make_call_instr(x, y, make_list1(THREE)))));
}

/*
 * Statements
 */

static List *make_stmt_list(Exp *val) {
    LvalExp *x = LVAL_EXP(LVALVAR(INT, VARX));
    Node *instr0 = make_set_instr(x, val);
    return make_list1(make_instr_stmt(make_list1(instr0)));
}

TEST(pp_goto_stmt) {
    LvalExp *x = LVAL_EXP(LVALVAR(INT, VARX));
    Node *stmt0 = make_instr_stmt(make_list1(make_set_instr(x, THREE)));

    List *stmts = make_list2(stmt0, make_goto_stmt(STMT(stmt0)));
    EQ_STRING("L0:x=3;goto L0;", pp_stmt_list(stmts));
}

TEST(pp_if_stmt) {
    List *then = make_stmt_list(ONE);
    List *els = make_stmt_list(TWO);
    EQ_STRING("if(1){x=1;}", pp_stmt(make_if_stmt(EXP(ONE), then, NULL)));
    EQ_STRING("if(1){x=1;}else{x=2;}", pp_stmt(make_if_stmt(EXP(ONE), then, els)));
}

TEST(pp_loop_stmt) {
    List *body = make_stmt_list(ONE);
    EQ_STRING("while(1){}", pp_stmt(make_loop_stmt(make_list())));
    EQ_STRING("while(1){x=1;}", pp_stmt(make_loop_stmt(body)));
}

TEST(pp_return_stmt) {
    EQ_STRING("return;", pp_stmt(make_return_stmt(NULL)));
    EQ_STRING("return 1;", pp_stmt(make_return_stmt(ONE)));
}

/*
 * Function
 */

TEST(pp_nfunction) {
    LvalExp *x = LVAL_EXP(LVALVAR(INT, VARX));
    LvalExp *y = LVAL_EXP(LVALVAR(INT, VARY));
    List *param = make_list2(x, y);
    List *var = make_list();
    List *stmt = make_list();
    NFunction *fn = make_nfunction(to_string("f"), INT, param, var, stmt);
    EQ_STRING("int f(x,y){}", pp_nfunction(fn));

    list_push(var, VARX);
    list_push(stmt, make_instr_stmt(make_list1(make_set_instr(x, THREE))));
    fn = make_nfunction(to_string("f"), INT, param, var, stmt);
    EQ_STRING("int f(x,y){int x;x=3;}", pp_nfunction(fn));
}
