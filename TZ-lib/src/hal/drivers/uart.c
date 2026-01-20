
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

/**
 * @brief Write a NULL-terminated string to the specified UART.
 * 
 */
void uart_write_string(uart_t uart, char *str) {
    while(*str != '\0') {
        uart_write(uart, *str++);
    }
}