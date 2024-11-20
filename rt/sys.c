#define _GNU_SOURCE

#include <stdio.h>
#include <stdint.h>
#include <inttypes.h>
#include <stdlib.h>

typedef uint64_t u64;
typedef int64_t i64;
const u64 BOOL_TRUE = 0x8000000000000001;
const u64 BOOL_FALSE = 0x0000000000000001;
// Tags
const u64 BOOL_TAG = 0b001;
const u64 TUPLE_TAG = 0b011;
const u64 CLOSURE_TAG = 0b101;
const u64 POINTER_MASK = 0b111;

typedef enum {
  NOT_NUMBER = 1,
  NOT_BOOLEAN = 2,
  NOT_TUPLE = 3,
  NOT_CLOSURE = 4,
  WRONG_ARITY = 5,
  INDEX_ERROR = 10,
  RT_ERROR = 100
} ErrCode;

extern u64 our_code_starts_here(u64 *heap) asm("our_code_starts_here");

char *val_to_string(u64 val) {
  char *s = NULL;
  u64 tag = val & POINTER_MASK;
  switch (tag) {
    case BOOL_TAG:
      if (val == BOOL_TRUE) asprintf(&s, "true");
      else asprintf(&s, "false");
      break;
    case TUPLE_TAG:
      asprintf(&s, "(tup");
      u64 *tup = (u64 *)(val - TUPLE_TAG);
      u64 tup_size = tup[0];
      for (u64 i=0; i<tup_size; i++) {
        char *elem = val_to_string(tup[i+1]);
        asprintf(&s, "%s %s", s, elem);
        free(elem);
      }
      asprintf(&s, "%s)", s);
      break;
    case CLOSURE_TAG:
      asprintf(&s, "<clos:");
      u64 *clos = (u64 *)(val - CLOSURE_TAG);
      u64 arity = clos[0];
      asprintf(&s, "%s%" PRId64, s, (i64)arity);
      asprintf(&s, "%s>", s);
      break;
    default:
      asprintf(&s, "%" PRId64, (i64)val >> 1);
      break;
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
    case NOT_CLOSURE:
      fprintf(stderr, "Type error: Expected closure but got %s", vs);
      break;
    case WRONG_ARITY:
      fprintf(stderr, "Arity mismatch: closure expected %s arguments but got %s", vs, xs);
      break;
    case INDEX_ERROR:
      fprintf(stderr, "Index out of bounds: Tried to access index %s of %s", xs, vs);
      break;
    case RT_ERROR:
      fprintf(stderr, "Runtime error: Invalid value 0x%" PRId64 "\n", val);
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
