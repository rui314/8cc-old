#include "8cc.h"

#define NOT_NULL(p) do { if (!(p)) error("Line %d: must not be null " #p "\n", __LINE__); } while (0)
#define EQ(x, y) do { if ((x) != (y)) error("Line %d: must be the same\n", __LINE__); } while (0)

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

static char golden_bin[] = {
  0x55, 0x48, 0x89, 0xe5, 0x48, 0xbf, 0x00, 0x00,
  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x31, 0xc0,
  0xe8, 0x00, 0x00, 0x00, 0x00, 0xc9, 0xc3,
};

void testAsemble(void) {
}

int main(int argc, char **argv) {
  testStringBuilder();
  testAsemble();
  printf("OK\n");
  return 0;
}
