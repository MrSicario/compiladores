#include <stdio.h>
#include <stdint.h>
#include <inttypes.h>
#include <stdlib.h>

typedef uint64_t u64;
typedef int64_t i64;
const u64 BOOL_TAG = 0x0000000000000001;
const u64 BOOL_TRUE = 0x8000000000000001;
const u64 BOOL_FALSE = 0x0000000000000001;
const int ERR_NOT_NUMBER = 1;
const int ERR_NOT_BOOLEAN = 2;
const int ERR_RUNTIME = 3;

extern u64 our_code_starts_here() asm("our_code_starts_here");

void error(int errCode, u64 val) {
  if (errCode == ERR_NOT_NUMBER) {
    if (val == BOOL_TRUE) {
      fprintf(stderr, "Type error: Expected integer but got true");
    } else if (val == BOOL_FALSE) {
      fprintf(stderr, "Type error: Expected integer but got false");
    }
  } else if (errCode == ERR_NOT_BOOLEAN) {
    fprintf(stderr, "Type error: Expected boolean but got %" PRId64 "\n", (i64)val >> 1);
  } else if (errCode == ERR_RUNTIME) {
    fprintf(stderr, "Runtime error: Invalid value 0x%llx", val);
  }

  exit(errCode);
}

// print any -> any
u64 print(u64 val) {
  if ((val & (BOOL_TAG)) == 0) {
    printf("> %lld\n", (i64)val >> 1);
  } else if (val == BOOL_TRUE) {
    printf("> true\n");
  } else if (val == BOOL_FALSE) {
    printf("> false\n");
  } else {
    error(ERR_RUNTIME, val);
  }
  return val;
}

i64 max(i64 a, i64 b) {
  return (a>b) ? a : b;
}

// int_pow int int -> int
i64 int_pow(i64 base, i64 exp) {
  unsigned result = 1;
  while (exp) {
    if (exp % 2)
      result *= base;
      exp /= 2;
      base *= base;
  }
  return result;
}

// mul_eight_nums int8x -> int
i64 mul_eight_nums(i64 a1, i64 a2, i64 a3, i64 a4, i64 a5, i64 a6, i64 a7, i64 a8) {
  return a1*a2*a3*a4*a5*a6*a7*a8;
}

int main(int argc, char** argv) {
  u64 result = our_code_starts_here();
  if (result == BOOL_TRUE) {
    printf("true");
  } else if (result == BOOL_FALSE) {
    printf("false");
  } else {
    printf("%" PRId64 "\n", (i64)result >> 1); // Shift represented num to actual value
  }
  return 0;
}
