#ifndef BACKENDS_STM32L5_LPUART1_H
#define BACKENDS_STM32L5_LPUART1_H

#include "drivers/uart.h"
#include "stm32l5xx.h"

typedef struct {
    USART_TypeDef *uart;
} stm32l5_lpuart1_backend_t;

void stm32l5_lpuart1_create(uart_dev_t *dev,
                             USART_TypeDef *uart,
                             stm32l5_lpuart1_backend_t *backend);

#endif // BACKENDS_STM32L5_LPUART1_H
