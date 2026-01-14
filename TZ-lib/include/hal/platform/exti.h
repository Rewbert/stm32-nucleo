#ifndef PLATFORM_EXTI_H
#define PLATFORM_EXTI_H

#include "hal/platform/domain.h"
#include "hal/platform/cmsis_select.h"
#include "hal/core/exti.h"
#include "stm32l5xx.h"

/**
 * @brief Mark an EXTI line as secure, meaning that only the secure application can use it.
 * This is achieved by configuring the SECCFGR1 and PRIVCFGR1 registers.
 * 
 * @param pin 
 */
static inline void platform_exti_make_secure(uint8_t pin) {
#if HAL_SECURE
    EXTIx->SECCFGR1 |= (1U << pin);
    EXTIx->PRIVCFGR1 |= (1U << pin);
#endif
}

/**
 * @brief Set a EXTI line to trigger on a falling edge
 * 
 */
static inline void platform_exti_trigger_on_falling(uint8_t pin) {
#if HAL_SECURE
    EXTIx->FTSR1 |= (1U << pin);
#endif
}

/**
 * @brief Set a EXTI line to trigger on a rising edge
 * 
 */
static inline void platform_exti_trigger_on_rising(uint8_t pin) {
#if HAL_SECURE
    EXTIx->RTSR1 |= (1U << pin);
#endif
}

/**
 * @brief Set the edge on which an EXTI will trigger
 * 
 * @param exti The exti line in question
 * @param edge the edge, which can be falling, rising, or both
 */
static inline void platform_exti_trigger_on(exti_line_t exti, exti_edge_t edge) {
#if HAL_SECURE
    switch(edge) {
        case EXTI_EDGE_RISING: platform_exti_trigger_on_rising(exti.pin); break;
        case EXTI_EDGE_FALLING: platform_exti_trigger_on_falling(exti.pin); break;
        case EXTI_EDGE_BOTH:
            platform_exti_trigger_on_falling(exti.pin);
            platform_exti_trigger_on_rising(exti.pin);
            break;
        default: break;
    }
#endif
}

/**
 * @brief Unmask interrupt requests for a specific EXTI pin
 * 
 */
static inline void platform_exti_unmask_interrupts(uint8_t pin) {
#if HAL_SECURE
    EXTIx->IMR1 |= (1U << pin);
#endif
}

/**
 * @brief This selects which CR register to write to in order to route the pin. There are 15 pins and
 * 4 CR registers, and these are the mappings.
 * pin 0-3   -> CR1
 * pin 4-7   -> CR2
 * pin 8-11  -> CR3
 * pin 12-15 -> CR4
 * 
 */
static inline uint8_t map_pin_to_cr(uint8_t pin) {
    // >> 2 will divide by 4, and adding 1 will yield the resulting CR register
    return (pin >> 2) + 1;
}

/**
 * @brief Return the shift to modify the CR register with.
 * 
 */
static inline uint32_t exti_exticr_shift(exti_line_t exti) {
    // The lowest 2 bits designate the segment of the register that should be modified.
    // each segment is 8-bit aligned
    return (exti.pin & 0x3U) * 8U;
}

/**
 * @brief Configure and route a pin to the EXTI peripheral, such that we can trigger
 * interrupts on changes in edge on the pin.
 * 
 */
static inline void platform_exti_route_pin(exti_line_t exti) {
#if HAL_SECURE
    uint8_t cr = map_pin_to_cr(exti.pin);
    uint32_t shift = exti_exticr_shift(exti);

    volatile uint32_t *reg = &EXTIx->EXTICR[cr-1];
    *reg &= ~(0x7UL << shift);
    *reg |= (uint32_t)(exti.port << shift);
#endif
}

/**
 * @brief Configure the NVIC to allow EXTI IRQs, and set their priority.
 * 
 */
static inline void platform_exti_configure_NVIC(exti_line_t exti, int priority) {
    int irqnum = exti.pin + 11; // the EXTI IRQn starts at 11, so pin 0 is irqn 11, pin 1 irqn 12, etc.
    NVIC_SetPriority(irqnum, priority);
    NVIC_EnableIRQ(irqnum);
}

#endif // PLATFORM_EXTI_H