#include <stdio.h>
#include <stdint.h>
#include <inttypes.h>
#include <stdlib.h>

typedef uint64_t u64;
typedef int64_t i64;
const u64 BOOL_TAG = 0x0000000000000001;
const u64 BOOL_TRUE = 0x8000000000000001;
const u64 BOOL_FALSE = 0x0000000000000001;
const u64 POINTER_MASK = 0b111;
const u64 TUPLE_TAG = 0b011;

typedef enum {
  NOT_NUMBER = 1,
  NOT_BOOLEAN = 2,
  NOT_TUPLE = 3,
  INDEX = 10,
  RUNTIME = 100
} ErrCode;

extern u64 our_code_starts_here(u64 *heap) asm("our_code_starts_here");

void error(ErrCode err, u64 val, u64 extra);

void runtime_error(u64 val) {
  fprintf(stderr, "Runtime error: Invalid value 0x%llx", val);
  exit(RUNTIME);
}

char *val_to_string(u64 val) {
  char *s = NULL;
  if (val == BOOL_TRUE) {
    asprintf(&s, "true");
  } else if (val == BOOL_FALSE) {
    asprintf(&s, "false");
  } else if ((val & POINTER_MASK) == TUPLE_TAG) {
    asprintf(&s, "(tup");
    u64 *tup = (u64 *)(val - TUPLE_TAG);
    u64 tup_size = tup[0];
    for (u64 i=0; i<tup_size; i++) {
      char *elem = val_to_string(tup[i+1]);
      asprintf(&s, "%s %s", s, elem);
      free(elem);
    }
    asprintf(&s, "%s)", s);
  } else if ((val & (BOOL_TAG)) == 0) {
    asprintf(&s, "%lld", (i64)val >> 1);
  } else {
    runtime_error(val);
  }
  return s;
}

void error(ErrCode err, u64 val, u64 extra) {
  char *vs = val_to_string(val);
  char *xs = val_to_string(extra);
  switch (err) {
    case NOT_NUMBER:
      fprintf(stderr, "Type error: Expected integer but got %s", vs);
      break;
    case NOT_BOOLEAN:
      fprintf(stderr, "Type error: Expected boolean but got %s", vs);
      break;
    case NOT_TUPLE:
      fprintf(stderr, "Type error: Expected tuple but got %s", vs);
      break;
    case INDEX:
      fprintf(stderr, "Index out of bounds: Tried to access index %s of %s", xs, vs);
      break;
    case RUNTIME:
      runtime_error(val);
  }
  free(vs);
  free(xs);
  exit(err);
}

// print any -> any
u64 print(u64 val) {
  char *s = val_to_string(val);
  printf("> %s\n", s);
  free(s);
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
  u64 *heap = calloc(1024, sizeof(u64));
  u64 result = our_code_starts_here(heap);
  char *rs = val_to_string(result);
  printf("%s", rs);
  free(rs);
  free(heap);
  return 0;
}
