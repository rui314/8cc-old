#include <stdarg.h>

#include "8cc.h"

#define NOT_NULL(p) do { if (!(p)) error("Line %d: must not be null " #p "\n", __LINE__); } while (0)
#define EQ(x, y) do { if ((x) != (y)) error("Line %d: must be the same\n", __LINE__); } while (0)

static void error(char *format, ...) {
  va_list ap;
  va_start(ap, format);
  vfprintf(stderr, format, ap);
  exit(-1);
}

/*
 * StringBuilder
 */

int64_t qword(char *str) {
  int64_t r = 0;
  for (int i = strlen(str) - 1; i >= 0; i--)
    r = (r << 8) | str[i];
  return r;
}

void testStringBuilder(void) {
  StringBuilder *b = make_sbuilder();
  NOT_NULL(b);
  EQ(SBUILDER_LEN(b), 0);

  o1(b, 'a');
  EQ(SBUILDER_LEN(b), 1);
  EQ(SBUILDER_BODY(b)[0], 'a');

  o8(b, qword("bcdefghi"));
  EQ(SBUILDER_LEN(b), 9);
  EQ(strcmp(SBUILDER_BODY(b), "abcdefghi"), 0);
}

int main(int argc, char **argv) {
  testStringBuilder();
  printf("OK\n");
  return 0;
}
