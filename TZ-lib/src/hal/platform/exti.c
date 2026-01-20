
#include "hal/platform/domain.h"
#include "hal/platform/cmsis_select.h"
#include "hal/core/exti.h"
#include "hal/platform/exti.h"
#include "stm32l5xx.h"

static exti_callback_t exti_callbacks[16] = {0};

void platform_exti_register_callback(exti_line_t exti, exti_callback_t cb) {
    if (exti.pin < 16) {
        exti_callbacks[exti.pin] = cb;
    }
}

#define EXTI_HANDLER(n)                          \
void exti##n##_handler(void) {                   \
    if(EXTIx->FPR1 & EXTI_FPR1_FPIF##n) {        \
        EXTIx->FPR1 |= EXTI_FPR1_FPIF##n;        \
        if(exti_callbacks[n] != 0) {             \
            exti_callbacks[n](EXTI_EDGE_FALLING);\
        }                                        \
    }                                            \
    if(EXTIx->RPR1 & EXTI_RPR1_RPIF##n) {        \
        EXTIx->RPR1 |= EXTI_RPR1_RPIF##n;        \
        if(exti_callbacks[n] != 0) {             \
            exti_callbacks[n](EXTI_EDGE_RISING); \
        }                                        \
    }                                            \
}

EXTI_HANDLER(0)
EXTI_HANDLER(1)
EXTI_HANDLER(2)
EXTI_HANDLER(3)
EXTI_HANDLER(4)
EXTI_HANDLER(5)
EXTI_HANDLER(6)
EXTI_HANDLER(7)
EXTI_HANDLER(8)
EXTI_HANDLER(9)
EXTI_HANDLER(10)
EXTI_HANDLER(11)
EXTI_HANDLER(12)
EXTI_HANDLER(13)