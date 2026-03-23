
#include "vtable.h"

volatile uint32_t vtable;

void set_vtable_ptr(uint32_t vtable_ptr) {
    vtable = vtable_ptr;
}

uint32_t get_vtable_ptr() {
    return vtable;
}