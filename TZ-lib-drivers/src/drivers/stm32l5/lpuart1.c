
#include "domain/domain.h"
#include "domain/cmsis_select.h"

#include "backends/stm32l5/lpuart1.h"

#include "stm32l5xx.h"

static inline void stm32l5_set_baudrate(struct uart_dev *dev, uint32_t baudrate) {
    stm32l5_lpuart1_backend_t *backend = (stm32l5_lpuart1_backend_t*) dev->backend;

    uint32_t brr = (256U * 110000000U) / baudrate;
    backend->uart->BRR = brr;
}

static inline void stm32l5_set_word_length(struct uart_dev *dev, uint8_t word_length) {
    stm32l5_lpuart1_backend_t *backend = (stm32l5_lpuart1_backend_t*) dev->backend;

    uint32_t copy = backend->uart->CR1;
    copy &= ~(USART_CR1_M0 | USART_CR1_M1); // mask off the bits we configure, so they are both 0 before we begin setting them

    switch(word_length) {
        case 7:
            copy |= USART_CR1_M1;
            break;
        case 8:
            break;
        case 9:
            copy |= USART_CR1_M0;
            break;
        default: return;
    }

    backend->uart->CR1 = copy;
}

static inline void stm32l5_set_stop_bits(struct uart_dev *dev, uint8_t stop_bits) {
    stm32l5_lpuart1_backend_t *backend = (stm32l5_lpuart1_backend_t*) dev->backend;

    uint32_t copy = backend->uart->CR2;
    copy &= ~(USART_CR2_STOP);

    switch(stop_bits) {
        case 1:
            break;
        case 2:
            copy |= USART_CR2_STOP_1;
            break;
        default: return;
    }

    backend->uart->CR2 = copy;
}

static inline void stm32l5_set_parity(struct uart_dev *dev, uart_parity_t parity) {
    stm32l5_lpuart1_backend_t *backend = (stm32l5_lpuart1_backend_t*) dev->backend;

    uint32_t copy = backend->uart->CR1;
    copy &= ~(USART_CR1_PCE | USART_CR1_PS);

    switch(parity) {
        case UART_PARITY_NONE: break; // PCE already 0 from masking above, so no need to set anything else
        case UART_PARITY_EVEN:
            copy |= (USART_CR1_PCE);
            break;
        case UART_PARITY_ODD:
            copy |= (USART_CR1_PCE | USART_CR1_PS);
            break;
        default: return;
    }

    backend->uart->CR1 = copy;
}

void stm32l5_init(struct uart_dev *dev, uart_config_t *config) {
    stm32l5_lpuart1_backend_t *backend = (stm32l5_lpuart1_backend_t*) dev->backend;

    stm32l5_set_baudrate(dev, config->baudrate);
    stm32l5_set_word_length(dev, config->word_length);
    stm32l5_set_stop_bits(dev, config->stop_bits);
    stm32l5_set_parity(dev, config->parity);

    backend->uart->CR1 |= (USART_CR1_UE | USART_CR1_TE | USART_CR1_RE);
}

void stm32l5_write(struct uart_dev *dev, const uint8_t *data, size_t len) {
    stm32l5_lpuart1_backend_t *backend = (stm32l5_lpuart1_backend_t*) dev->backend;

    if(len > 0) {
        while(len--) {
            // Wait until the transmit data register is empty
            while (!(backend->uart->ISR & USART_ISR_TXE)); // Check TXE flag
    
            backend->uart->TDR = *data++; // Write character to transmit
        }
    
        while (!(backend->uart->ISR & USART_ISR_TC)); // Wait for transmission to complete
    }
}

size_t stm32l5_read(struct uart_dev *dev, uint8_t *data, size_t len) {
    stm32l5_lpuart1_backend_t *backend = (stm32l5_lpuart1_backend_t*) dev->backend;

    size_t count = 0;

    while (count < len) {

        /* Wait for RXNE */
        while (!(backend->uart->ISR & USART_ISR_RXNE));

        data[count++] = (uint8_t)backend->uart->RDR;
    }

    return count;

}

static const uart_driver_api_t stm32l5_lpuart1_api = {
    .init = stm32l5_init,
    .write = stm32l5_write,
    .read = stm32l5_read,
};

void stm32l5_lpuart1_create(uart_dev_t *dev,
                            USART_TypeDef *uart,
                            stm32l5_lpuart1_backend_t *backend) {
    backend->uart = uart;

    dev->api = &stm32l5_lpuart1_api;
    dev->backend = backend;
}