#include <stdlib.h>

struct desc_t {
    struct desc_t *parent;
    size_t size;
    void (*copy_routine)(void *, void *);
    void (*final_routine)(void *);
    void *(*vtable[])(void *, ...);
};

void *allocate(const struct desc_t *type) {
    return malloc(type->size);
}

void allocatable_copy(const struct desc_t *type, void **dst, void *src) {
    *dst = allocate(type);
    type->copy_routine(*dst, src);
}

void allocatable_final(const struct desc_t *type, void *value) {
    type->final_routine(value);
    free(value);
}

void copy_int(void *dst, void *src) {
    int *dst_int = dst;
    int *src_int = src;
    *dst_int = *src_int;
}

void final_int(void *val) {}

struct desc_t desc_int = {.parent=NULL, .size=sizeof(int), .copy_routine=copy_int, .final_routine=final_int, .vtable={}};

struct b {
    int *a;
};

void copy_b(void *dst, void *src) {
    struct b *dst_b = dst;
    struct b *src_b = src;
    allocatable_copy(&desc_int, (void **)&dst_b->a, src_b->a);
}

void final_b(void *value) {
    struct b *value_b = value;
    allocatable_final(&desc_int, value_b->a);
}

struct desc_t desc_b = {.parent=NULL, .size=sizeof(int *), .copy_routine=copy_b, .final_routine=final_b, .vtable={}};

// Inherits from b
struct c {
    struct b b;
};

void copy_c(void *dst, void *src) {
    struct c *dst_c = dst;
    struct c *src_c = src;
    desc_b.copy_routine(&dst_c->b, &src_c->b);
}

void final_c(void *value) {
    struct c *value_c;
    desc_b.final_routine(&value_c->b);
}

struct desc_t desc_c = {.parent=&desc_b, .size=sizeof(int *), .copy_routine=copy_c, .final_routine=final_c, .vtable={}};