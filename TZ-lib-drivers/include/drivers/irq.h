#ifndef DRIVERS_IRQ_H
#define DRIVERS_IRQ_H

#include "domain/cmsis_select.h"

static inline void irq_enable(void)  { __enable_irq();  }
static inline void irq_disable(void) { __disable_irq(); }

#endif // DRIVERS_IRQ_H
