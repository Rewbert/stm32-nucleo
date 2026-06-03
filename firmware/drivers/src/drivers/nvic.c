#include "domain/cmsis_select.h"

#include "drivers/nvic.h"

void nvic_set_priority(int irqn, uint8_t priority) {
    NVIC_SetPriority((IRQn_Type)irqn, priority);
}

void nvic_enable_irq(int irqn) {
    NVIC_EnableIRQ((IRQn_Type)irqn);
}

void nvic_set_target_nonsecure(int irqn) {
#if HAL_SECURE
    NVIC_SetTargetState((IRQn_Type)irqn);
#else
    (void)irqn;
#endif
}
