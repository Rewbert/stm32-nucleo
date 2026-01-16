#ifndef PLATFORM_UART_H
#define PLATFORM_UART_H

#include <stdint.h>
#include "hal/platform/domain.h"
#include "hal/platform/cmsis_select.h"
#include "hal/core/uart.h"
#include "hal/core/clock.h"
#include "stm32l5xx.h"

/**
 * @brief Set the BRR register value of the LPUART1 peripheral.
 * 
 */
static inline void platform_set_lpuart1_brr(uint16_t brr) {
#if HAL_SECURE
    volatile uint32_t dummy;
    LPUART1x->BRR = brr;
    dummy = RCCx->AHB2ENR;
    dummy = RCCx->AHB2ENR;
#endif
}

/**
 * @brief Set the clock source of the LPUART1 peripheral.
 * 
 */
static inline void platform_lpuart1_select_clock_source(clock_t clock) {
#if HAL_SECURE
    RCCx->CCIPR1 &= ~(3U << RCC_CCIPR1_LPUART1SEL_Pos);
    RCCx->CCIPR1 |= (clock << RCC_CCIPR1_LPUART1SEL_Pos);
#endif
}

/**
 * @brief Activate the LPUART1 peripheral. This is done by modifying the UE (LPUART Enable) bit, the
 * TE (transmitter enable) bit, and the RE (receiver enable) bit.
 * 
 */
static inline void platform_lpuart1_activate() {
#if HAL_SECURE
    LPUART1x->CR1 |= USART_CR1_UE | USART_CR1_TE | USART_CR1_RE;  
#endif
}

/**
 * @brief Write a character to the LPUART1 peripheral.
 * 
 */
void platform_lpuart1_write(char c);

/**
 * @brief Read a character from the LPUART1 peripheral.
 * 
 */
char platform_lpuart1_read(void);

#endif // PLATFORM_UART_H