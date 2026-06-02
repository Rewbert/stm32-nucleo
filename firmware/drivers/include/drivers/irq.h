#ifndef DRIVERS_IRQ_H
#define DRIVERS_IRQ_H

/* cpsie i / cpsid i — Cortex-M exception mask instructions */
static inline void irq_enable(void)  { __asm volatile ("cpsie i" : : : "memory"); }
static inline void irq_disable(void) { __asm volatile ("cpsid i" : : : "memory"); }

#endif // DRIVERS_IRQ_H
