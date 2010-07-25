#define _GNU_SOURCE 1
#include "unittest.h"
#include "../run.c"

TEST(run_string) {
    EQ(1, run_string("main() { return 1; }"));
}
