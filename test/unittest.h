/*
 * unittest.c - unit test utilities
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

/*
 * License of all the other files in this directory is the same as above.
 */

#include "../8cc.h"

#define CONSTRUCTOR __attribute__((constructor))

#define NOT_NULL(p) do { if (!(p)) error("Line %d: must not be null " #p, __LINE__); } while (0)
#define EQ(x, y) do { if ((x) != (y)) error("Line %d: must be the same: '%s' and '%s'", __LINE__, #x, #y); } while (0)
#define EQ_CHAR(x, y) do { eq_char(__LINE__, (x), (y)); } while (0)
#define EQ_STR(x, y)  do { eq_str(__LINE__, (x), (y)); } while (0)

extern List* test_funcs;

#define TEST(name)                                     \
    static void TEST_##name(void);                     \
    CONSTRUCTOR static void name##_TEST_INIT(void) {   \
        if (!test_funcs)                               \
            test_funcs = make_list();                  \
        list_push(test_funcs, TEST_##name);            \
        list_push(test_funcs, #name);                  \
    }                                                  \
    static void TEST_##name(void)

#define RUN_TESTS()                                         \
    int main(int argc, char **argv) {                       \
        printf("Running unit tests ...\n");                 \
        while (LIST_LEN(test_funcs) > 0) {                  \
            char *name = (char *)list_pop(test_funcs);      \
            printf("  %s\n", name);                         \
            void (*fn)(void) = list_pop(test_funcs);        \
            fn();                                           \
        }                                                   \
        printf("done\n");                                   \
    }

extern void eq_str(int line, char *expected, char *got);
extern void eq_char(int line, int expected, int got);
extern FILE *create_file(char *content);
