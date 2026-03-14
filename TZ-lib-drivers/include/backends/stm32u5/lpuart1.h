#ifndef BACKENDS_STM32U5_LPUART1_H
#define BACKENDS_STM32U5_LPUART1_H

#include "drivers/uart.h"
#include "stm32u5xx.h"

typedef struct {
    USART_TypeDef *uart;
} stm32u5_lpuart1_backend_t;

void stm32u5_lpuart1_create(uart_dev_t *dev,
                             USART_TypeDef *uart,
                             stm32u5_lpuart1_backend_t *backend);

#endif // BACKENDS_STM32U5_LPUART1_H
