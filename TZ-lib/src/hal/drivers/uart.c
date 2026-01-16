
#include "hal/drivers/uart.h"
#include "hal/platform/uart.h"

/**
 * @brief Write a character to a specific UART.
 * 
 */
void uart_write(uart_t uart, char c) {
    switch(uart) {
      case HAL_USART1: break;
      case HAL_USART2: break;
      case HAL_USART3: break;
      case HAL_UART4: break;
      case HAL_UART5: break;
      case HAL_LPUART1: platform_lpuart1_write(c); break;
      default: break;
    }
}

/**
 * @brief Read a character from a specific UART.
 * 
 */
char uart_read(uart_t uart) {
    switch(uart) {
      case HAL_USART1: return -1;
      case HAL_USART2: return -1;
      case HAL_USART3: return -1;
      case HAL_UART4: return -1;
      case HAL_UART5: return -1;
      case HAL_LPUART1: return platform_lpuart1_read(); break;
      default: return -1;
    }
}













// *** from secure world *** //


void lpuart1_write(char c) {
    // Wait until the transmit data register is empty
    while (!(LPUART1_S->ISR & USART_ISR_TXE)); // Check TXE flag
    LPUART1_S->TDR = c; // Write character to transmit
    while (!(LPUART1_S->ISR & USART_ISR_TC)); // Wait for transmission to complete
  }
  
  char lpuart1_read(void) {
    // Wait until the receive data register is not empty
    while (!(LPUART1_S->ISR & USART_ISR_RXNE));
    return (char)(LPUART1_S->RDR & 0xFF); // Read received character
  }

// *** from nonsecure world *** //

void lpuart1_write(char c) {
    // we don't proceed if the LPUART1 is marked as secure. We are allowed from the non secure application to
    // observe whether it is secured or not, but not to alter the actual configuration
    if(!(RCC_NS->APB1SECSR2 & RCC_APB1SECSR2_LPUART1SECF_Msk)) {
      // Wait until the transmit data register is empty
      while (!(LPUART1_NS->ISR & USART_ISR_TXE)); // Check TXE flag
      LPUART1_NS->TDR = c; // Write character to transmit
      while (!(LPUART1_NS->ISR & USART_ISR_TC)); // Wait for transmission to complete
    }
  }

  char lpuart1_read(void) {
    // Wait until the receive data register is not empty
    while (!(LPUART1_NS->ISR & USART_ISR_RXNE));
    return (char)(LPUART1_NS->RDR & 0xFF); // Read received character
  }