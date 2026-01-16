
#include "hal/platform/domain.h"
#include "hal/platform/cmsis_select.h"
#include "hal/core/uart.h"
#include "hal/platform/uart.h"
#include "stm32l5xx.h"

/**
 * @brief Write a character to the LPUART1 peripheral. The non-secure application performs
 * a check whether the LPUART1 peripheral is marked as secure or not before proceeding. If
 * it is marked as secure, it will not do anything.
 * 
 */
void platform_lpuart1_write(char c) {
#if !HAL_SECURE
    if(!(RCCx->APB1SECSR2 & RCC_APB1SECSR2_LPUART1SECF_Msk)) {
#endif
        // Wait until the transmit data register is empty
        while (!(LPUART1x->ISR & USART_ISR_TXE)); // Check TXE flag
        LPUART1x->TDR = c; // Write character to transmit
        while (!(LPUART1x->ISR & USART_ISR_TC)); // Wait for transmission to complete
#if !HAL_SECURE
    }
#endif
}

/**
 * @brief Read a character from the LPUART1 peripheral.
 * 
 */
char platform_lpuart1_read(void) {
    // Wait until the receive data register is not empty
    while (!(LPUART1x->ISR & USART_ISR_RXNE));
    return (char)(LPUART1x->RDR & 0xFF); // Read received character
}