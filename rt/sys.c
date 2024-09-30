#include <stdio.h>
#include <stdint.h>
#include <inttypes.h>
#include <stdlib.h>

typedef uint64_t u64;
const u64 BOOL_TAG = 0x0000000000000001;
const u64 BOOL_TRUE = 0x8000000000000001;
const u64 BOOL_FALSE = 0x0000000000000001;
const int ERR_NOT_NUMBER = 1;
const int ERR_NOT_BOOLEAN = 2;

extern u64 our_code_starts_here() asm("our_code_starts_here");

void error(int errCode, u64 val) {
  if (errCode == ERR_NOT_NUMBER) {
    if (val == BOOL_TRUE) {
      fprintf(stderr, "Type error: Expected integer but got true");
    } else if (val == BOOL_FALSE) {
      fprintf(stderr, "Type error: Expected integer but got false");
    }
  } else if (errCode == ERR_NOT_BOOLEAN) {
    fprintf(stderr, "Type error: Expected boolean but got %" PRId64 "\n", (int64_t)val >> 1);
  }

  exit(errCode);
}

int main(int argc, char** argv) {
  u64 result = our_code_starts_here();
  if (result == BOOL_TRUE) {
    printf("true");
  } else if (result == BOOL_FALSE) {
    printf("false");
  } else {
    printf("%" PRId64 "\n", (int64_t)result >> 1); // Shift represented num to actual value
  }
  return 0;
}
