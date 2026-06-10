#ifndef MICROHASTEE_EXTI_TRAMPOLINE_H
#define MICROHASTEE_EXTI_TRAMPOLINE_H

#include <stdint.h>

#include "drivers/exti.h"

void set_exti_callback_table_ptr(uint32_t table_ptr);
uint32_t get_exti_callback_table_ptr(void);

void h_exti_register_callback(exti_dev_t *dev, int line);

#endif // MICROHASTEE_EXTI_TRAMPOLINE_H
