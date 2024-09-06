#include <stdio.h>
#include <stdint.h>
#include <inttypes.h>

typedef uint64_t u64;
const u64 BOOL_TAG = 0x0000000000000001;
const u64 BOOL_TRUE = 0x8000000000000001;
const u64 BOOL_FALSE = 0x0000000000000001;

extern u64 our_code_starts_here() asm("our_code_starts_here");

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
