#ifndef DRIVERS_NVIC_H
#define DRIVERS_NVIC_H

#include <stdint.h>

void nvic_set_priority(int irqn, uint8_t priority);
void nvic_enable_irq(int irqn);
void nvic_set_target_nonsecure(int irqn);

#endif // DRIVERS_NVIC_H
