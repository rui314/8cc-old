/*
 * Copyright 2010 Rui Ueyama <rui314@gmail.com>.  All rights reserved.
 * This code is available under the simplified BSD license.  See LICENSE for details.
 */

#include "unittest.h"
#include "../il.c"

TEST(pp_type) {
    EQ_STRING("int", pp_type(get_int_type(INT)));
    EQ_STRING("unsigned int", pp_type(get_int_type(UINT)));

    EQ_STRING("float", pp_type(get_float_type(FLOAT)));
    EQ_STRING("double", pp_type(get_float_type(DOUBLE)));

    EQ_STRING("void", pp_type(get_void_type()));

    EQ_STRING("int*", pp_type(make_ptr_type(get_int_type(INT))));
    EQ_STRING("float*", pp_type(make_ptr_type(get_float_type(FLOAT))));
    EQ_STRING("void**", pp_type(make_ptr_type(make_ptr_type(get_void_type()))));
}
