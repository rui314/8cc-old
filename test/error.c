/*
 * Copyright 2010 Rui Ueyama <rui314@gmail.com>.  All rights reserved.
 * This code is available under the simplified BSD license.  See LICENSE for details.
 */

#include "unittest.h"
#include "../error.c"

TEST(error) {
    Exception *e = make_exception();
    if (CATCH_ERROR(e)) {
        EQ_STR("ERROR: foo", STRING_BODY(e->msg));
        return;
    }
    error("foo");
}
