//#define GC_DEBUG
//#define GC_DUMP_REGULARLY
//#define GC_PRINT_STATS
//#define GC_PRINT_VERBOSE_STATS

#include <bdwgc/include/gc.h>
#include <stdio.h>
#include <stdlib.h>

void *alloc(size_t size) {
    return GC_MALLOC(size);
}

void print_unit() {
    printf("unit\n");
}

void print_nil() {
    printf("nil\n");
}

void print_int(int i) {
    printf("%d\n", i);
}

void print_bool(int b) {
    printf("%s\n", b ? "true" : "false");
}

void print_pointer(void *p) {
    if (p == NULL) {
        print_nil();
    } else {
        printf("%p\n", p);
    }
}
