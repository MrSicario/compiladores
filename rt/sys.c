#define _GNU_SOURCE

#include <inttypes.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>

typedef uint64_t u64;
typedef int64_t i64;

extern u64 our_code_starts_here(u64 *heap) asm("our_code_starts_here");

// Values & tags
const u64 BOOL_TRUE = 0x8000000000000001;
const u64 BOOL_FALSE = 0x0000000000000001;
const u64 BOOL_TAG = 0b001;
const u64 TUPLE_TAG = 0b011;
const u64 CLOSURE_TAG = 0b101;
const u64 FORWARD_TAG = 0b111;
const u64 POINTER_MASK = 0b111;

/// Memory management & heap state
u64 STACK_SIZE = 0x800000;
u64 HEAP_SIZE = 16;
int USE_GC = 1;
u64 *stack_root;
u64 *heap_alloc_start;
u64 *heap_start;
u64 *heap_mid;
u64 *heap_end;
u64 *heap_from_space;
u64 *heap_to_space;
u64 *alloc_ptr;
u64 *scan_ptr;

// Errors
typedef enum {
	NOT_NUMBER = 1,
	NOT_BOOLEAN = 2,
	NOT_TUPLE = 3,
	NOT_CLOSURE = 4,
	WRONG_ARITY = 5,
	INDEX_ERROR = 10,
	RT_ERROR = 100,
	MEMORY_ERROR = 101
} ErrCode;

// Types
typedef enum {
	Int = 1,
	Bool = 2,
	Tuple = 3,
	Closure = 4,
	Forward = 5
} Type;

void error(ErrCode err, u64 val, u64 extra);
char *val_to_string(u64 val);
void print_heaps();

// Error handler
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
		break;
	case MEMORY_ERROR:
		fprintf(stderr, "MemoryError: Not enough memory for allocation");
		break;
	}
	free(vs);
	free(xs);
	exit(err);
}

// Type and tagging funs

// Get the type of a value by rading its tag, will always return a type if correct or crash.
Type get_type(u64 val) {
	if ((val & 0b1) == 0) return Int;
	u64 tag = val & POINTER_MASK;
	switch (tag) {
		case BOOL_TAG: return Bool;
		case TUPLE_TAG: return Tuple;
		case CLOSURE_TAG: return Closure;
		case FORWARD_TAG: return Forward;
		default: error(RT_ERROR, val, 0);
	}
	exit(-1);
}

// Get the type of a value, or 0 if not a well formed value.
Type get_type_unsafe(u64 val) {
	if ((val & 0b1) == 0) return Int;
	u64 tag = val & POINTER_MASK;
	switch (tag) {
		case BOOL_TAG: return Bool;
		case TUPLE_TAG: return Tuple;
		case CLOSURE_TAG: return Closure;
		case FORWARD_TAG: return Forward;
		default: return 0;
	}
	return 0;
}

u64 tag(u64 val, Type type) {
	switch (type) {
		case Int: return val << 1;
		case Bool: return (val << 63) + 1;
		case Tuple: return val + TUPLE_TAG;
		case Closure: return val + CLOSURE_TAG;
		case Forward: return val + FORWARD_TAG;
	}
	exit(-1);
}

u64 untag(u64 val) {
	switch (get_type(val)) {
		case Int: return (i64)val >> 1;
		case Bool: return val >> 63;
		case Tuple: return val - TUPLE_TAG;
		case Closure: return val - CLOSURE_TAG;
		case Forward: return val - FORWARD_TAG;
	}
	exit(-1);
}

// Strings and printing
char *val_to_string(u64 val) {
	char *s = NULL;
	switch (get_type(val)) {
		case Int: asprintf(&s, "%" PRId64, (i64)val >> 1); return s;
		case Bool:
			if (val == BOOL_TRUE) {
				asprintf(&s, "true");
			} else {
				asprintf(&s, "false");
			}
			return s;
		case Tuple:
			asprintf(&s, "(tup");
			u64 *tup = (u64 *)(val - TUPLE_TAG);
			u64 tup_size = tup[0];
			for (u64 i=0; i<tup_size; i++) {
				char *elem = val_to_string(tup[i+1]);
				asprintf(&s, "%s %s", s, elem);
				free(elem);
			}
			asprintf(&s, "%s)", s);
			return s;
		case Closure:
			asprintf(&s, "<clos:");
			u64 *clos = (u64 *)(val - CLOSURE_TAG);
			u64 arity = clos[0];
			asprintf(&s, "%s%" PRId64, s, (i64)arity);
			asprintf(&s, "%s>", s);
			return s;
		default:
			error(RT_ERROR, val, 0);
	}
	exit(-1);
}

u64 print(u64 val) {
	char *s = val_to_string(val);
	printf("> %s\n", s);
	free(s);
	return val;
}

// Memory management funs
void set_stack_root(u64* rsp) {
	stack_root = rsp;
};

void heap_init() {
	heap_alloc_start = calloc((HEAP_SIZE * 2) + 15, sizeof(u64));
	heap_start = (u64*)(((u64)(heap_alloc_start + 15)) & ~0xf);
	heap_mid = heap_start + HEAP_SIZE;
	heap_end = heap_start + 2 * HEAP_SIZE;
	heap_from_space = heap_start;
	heap_to_space = heap_mid;
}

void heap_free() {
	free(heap_alloc_start);
}

bool is_heap_ptr(u64 *addr) {
	return addr < heap_end && addr >= heap_start;
}

void print_heap(u64 *p_heap_start, u64 *p_heap_end) {
	printf("| Heap from %p to %p\n", p_heap_start, p_heap_end);
	for (u64 i = 0; i <= (u64)(p_heap_end - p_heap_start); i++) {
		printf("|  %lld/%p: %p \n", i, (p_heap_start + i), (u64*)*(p_heap_start + i));
	}
}

void print_heaps() {
	printf("|\n|=======HEAP 1==========\n");
	print_heap(heap_start, (heap_mid)-1);
	printf("|=======HEAP 2==========\n");
	print_heap(heap_mid, heap_end);
	printf("|=================\n\n");
}

void print_stack(u64* rbp, u64* rsp) {
	printf("|------- frame %p to %p  ------\n", rsp, rbp);
	for (u64* cur_word = rsp+6; cur_word < rbp; cur_word++) {
		u64 val = (u64)*cur_word;
		printf("| %p: %p", cur_word, (u64*)*cur_word);
		if (is_heap_ptr((u64*)val)) {
			if (get_type(val) == Tuple){ printf(" (tuple)"); }
			else if (get_type(val) == Closure){ printf(" (closure)"); }
		}
		printf("\n");
	}
	if (rbp < stack_root) {
		print_stack((u64*)*rbp, rbp + 2);
	} else {
		printf("|------- bottom %p  ------\n\n", stack_root);
	}
}

// returns the length of an object in slots
size_t memsize(u64 val) {
	switch (get_type(val)) {
		case Int: return 1;
		case Bool: return 1;
		case Tuple: return 1 + ((u64*)untag(val))[0]; // 1 + tuple size (slot 1)
		case Closure: return 3 + ((u64*)untag(val))[2]; // 3 + n_free_vars (slot 2)
		default: exit(-1);
	}
	exit(-1);
}

Type get_heap_object_type(u64 *heap_object) {
	// how do we know the type of an object if we're standing directly on the data and not on a tagged value pointing to it?
	// i dont have the time to add a new data slot indicating the type
	// depressing hack for these bleak times: check slot 1 (code address on closure, first element on tuple)
	// the address will always end on 0b000, so we can distinguish between the two (unless its a tuple with a factor of 8 on the first slot, or an empty tuple)
	if ((heap_object[1] & POINTER_MASK) == 0b000 && is_heap_ptr((u64*)heap_object[1])) return Closure;
	return Tuple;
}

// given a value_slot (pointer to stack or heap), if the value is an object (tuple or closure):
// copy and forward the object if elegible, then set the value to match the new address.
void try_copy_forward(u64 *value_slot) {
	u64 *new_addr;
	u64 val = *value_slot;
	Type value_type = get_type_unsafe(val);
	if (value_type == Tuple || value_type == Closure) {
		u64 *object = (u64*)untag(val); // since it's a tuple/closure, untagging gives the address of the object
		if (is_heap_ptr(object)) {
			// now check the first element of the object data
			// it can be (1): a Forward value to an already copied object -> modify the original value so it points to the new adress instead of here
			// or (2): literally anything else -> copy forward this and also set the original value
			if (get_type_unsafe(object[0]) == Forward) {
				new_addr = (u64*)untag(object[0]); // get the forward adress actually containing the object
			} else {
				new_addr = alloc_ptr;
				size_t object_len = memsize(val);
				memcpy(new_addr, object, object_len * sizeof(u64)); // copy the object to the new address (memcpy copies n bytes!!)
				object[0] = tag((u64)new_addr, Forward); // on the (now old) object, replace the first slot with a Forward value
				alloc_ptr += object_len; // push the allocation pointer by object_len slots
			}

			// finally change the original value regardless of what happened (since either way the object is not where the value says anymore)
			*value_slot = tag((u64)new_addr, value_type);
		}
	}
}


u64 *collect(u64* cur_frame, u64* cur_sp) {
	// swap from-space to-space
	u64 *tmp = heap_from_space;
	heap_from_space = heap_to_space;
	heap_to_space = tmp;

	u64 *tuple_elems_start;
	u64 *closure_values_start;

	// init spaces
	alloc_ptr = heap_from_space;
	scan_ptr = heap_from_space;
	
	u64 msize;
	// scan stack (todo: repeat this until stack root)
	for (u64 *cur_slot = cur_sp+6; cur_slot < stack_root || cur_slot < cur_frame; cur_slot++) {
		try_copy_forward(cur_slot);
	}

	// scan objects in the new heap to bring over what's necessary
	while (scan_ptr < alloc_ptr) {
		try_copy_forward(scan_ptr);
		scan_ptr++;
	}

	// clean up old heap
	for (u64 *slot = heap_to_space; slot < heap_to_space + HEAP_SIZE; slot++) {
		*slot = 0ull;
	}

	return alloc_ptr;
}

/* trigger GC if enabled and needed, out-of-memory error if insufficient */
u64 *try_gc(u64* alloc_ptr, u64 words_needed, u64* cur_frame, u64* cur_sp) {
	if (USE_GC==1 && alloc_ptr + words_needed > heap_from_space + HEAP_SIZE) {
		printf("| need memory: GC!\n");
		alloc_ptr = collect(cur_frame, cur_sp);
	}
	if (alloc_ptr + words_needed > heap_from_space + HEAP_SIZE) {
		error(MEMORY_ERROR, 0, 0);
	}
	return alloc_ptr;
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
	/* stack size config */
	char* stack_size_envvar = getenv("STACK_SIZE");
	if (stack_size_envvar) STACK_SIZE = strtoull(stack_size_envvar, NULL, 0);
	printf("| Setting stack size to %" PRId64 " .\n", STACK_SIZE);
	struct rlimit limit;
	getrlimit(RLIMIT_STACK, &limit);
	limit.rlim_cur = STACK_SIZE < limit.rlim_max ? STACK_SIZE : limit.rlim_max;
	int res = setrlimit(RLIMIT_STACK, &limit);
	if (res != 0) { printf("| Setting rlimit failed...\n") ; }

	 /* GC config */
	char* use_gc_envvar = getenv("USE_GC");
	if (use_gc_envvar) USE_GC = strtoull(use_gc_envvar, NULL, 0);
	printf("| Use GC: %d\n", USE_GC);
	
	/* heap size config */
	char* heap_size_envvar = getenv("HEAP_SIZE");
	if (heap_size_envvar) HEAP_SIZE = strtoull(heap_size_envvar, NULL, 0);
	printf("| Heap size: %" PRId64 " .\n", HEAP_SIZE);

	// Initialize two-space heap
	heap_init();

	// Run the program
	u64 result = our_code_starts_here(heap_start);
	char *r_str = val_to_string(result);
	printf("%s", r_str);
	free(r_str);

	heap_free();
	return 0;
}
