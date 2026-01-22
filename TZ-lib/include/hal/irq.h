#ifndef HAL_IRQ_H
#define HAL_IRQ_H

/**
 * @brief Enable interrupts.
 * 
 */
static inline void enable_irq() {
    __asm volatile ("cpsie i" : : : "memory");
}

/**
 * @brief Disable interrupts.
 * 
 */
static inline void disable_irq() {
    __asm volatile ("cpsid i" : : : "memory")
}

#endif // HAL_IRQ_H