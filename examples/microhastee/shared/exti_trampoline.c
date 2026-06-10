#include "exti_trampoline.h"

#include <stdint.h>

extern void h_exti_dispatch(intptr_t line, intptr_t edge);

static volatile uint32_t exti_callback_table;

void set_exti_callback_table_ptr(uint32_t table_ptr) {
    exti_callback_table = table_ptr;
}

uint32_t get_exti_callback_table_ptr(void) {
    return exti_callback_table;
}

#define H_EXTI_CALLBACK(n)                         \
    static void h_exti##n##_callback(exti_edge_t edge) { \
        h_exti_dispatch(n, (intptr_t)edge);        \
    }

H_EXTI_CALLBACK(0)
H_EXTI_CALLBACK(1)
H_EXTI_CALLBACK(2)
H_EXTI_CALLBACK(3)
H_EXTI_CALLBACK(4)
H_EXTI_CALLBACK(5)
H_EXTI_CALLBACK(6)
H_EXTI_CALLBACK(7)
H_EXTI_CALLBACK(8)
H_EXTI_CALLBACK(9)
H_EXTI_CALLBACK(10)
H_EXTI_CALLBACK(11)
H_EXTI_CALLBACK(12)
H_EXTI_CALLBACK(13)
H_EXTI_CALLBACK(14)
H_EXTI_CALLBACK(15)

void h_exti_register_callback(exti_dev_t *dev, int line) {
    switch (line) {
        case 0:  exti_register_callback(dev, h_exti0_callback);  break;
        case 1:  exti_register_callback(dev, h_exti1_callback);  break;
        case 2:  exti_register_callback(dev, h_exti2_callback);  break;
        case 3:  exti_register_callback(dev, h_exti3_callback);  break;
        case 4:  exti_register_callback(dev, h_exti4_callback);  break;
        case 5:  exti_register_callback(dev, h_exti5_callback);  break;
        case 6:  exti_register_callback(dev, h_exti6_callback);  break;
        case 7:  exti_register_callback(dev, h_exti7_callback);  break;
        case 8:  exti_register_callback(dev, h_exti8_callback);  break;
        case 9:  exti_register_callback(dev, h_exti9_callback);  break;
        case 10: exti_register_callback(dev, h_exti10_callback); break;
        case 11: exti_register_callback(dev, h_exti11_callback); break;
        case 12: exti_register_callback(dev, h_exti12_callback); break;
        case 13: exti_register_callback(dev, h_exti13_callback); break;
        case 14: exti_register_callback(dev, h_exti14_callback); break;
        case 15: exti_register_callback(dev, h_exti15_callback); break;
        default: break;
    }
}
