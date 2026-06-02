#ifndef VTABLE_H
#define VTABLE_H

#include <stdint.h>

extern volatile uint32_t vtable;

void set_vtable_ptr(uint32_t vtable_ptr);

uint32_t get_vtable_ptr();

#endif