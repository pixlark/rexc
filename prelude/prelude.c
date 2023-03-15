#include "bdwgc/include/gc.h"
#include <stdio.h>
#include <stdlib.h>

void *alloc(size_t size) {
    return GC_malloc(size);
}

void print_unit() {
    printf("unit\n");
}

void print_int(int i) {
    printf("%d\n", i);
}

void print_bool(int b) {
    printf("%s\n", b ? "true" : "false");
}

void print_pointer(void *p) {
    printf("%p\n", p);
}
