#include "domain/domain.h"
#include "domain/cmsis_select.h"

#include "backends/stm32u5/usart1.h"

#include "stm32u5xx.h"

/* USART_TypeDef register layout is identical to LPUART1; the only difference is
 * the BRR formula: USART BRR = f_clk / baudrate (no 256× factor — that is
 * specific to LPUART's fractional baud-rate generator). */

#define USART1_CLOCK_HZ 160000000UL

static inline void stm32u5_usart1_set_baudrate(struct uart_dev *dev, uint32_t baudrate) {
    stm32u5_usart1_backend_t *backend = (stm32u5_usart1_backend_t *) dev->backend;
    backend->uart->BRR = USART1_CLOCK_HZ / baudrate;
}

static inline void stm32u5_usart1_set_word_length(struct uart_dev *dev, uint8_t word_length) {
    stm32u5_usart1_backend_t *backend = (stm32u5_usart1_backend_t *) dev->backend;

    uint32_t copy = backend->uart->CR1;
    copy &= ~(USART_CR1_M0 | USART_CR1_M1);

    switch (word_length) {
        case 7: copy |= USART_CR1_M1; break;
        case 8: break;
        case 9: copy |= USART_CR1_M0; break;
        default: return;
    }

    backend->uart->CR1 = copy;
}

static inline void stm32u5_usart1_set_stop_bits(struct uart_dev *dev, uint8_t stop_bits) {
    stm32u5_usart1_backend_t *backend = (stm32u5_usart1_backend_t *) dev->backend;

    uint32_t copy = backend->uart->CR2;
    copy &= ~USART_CR2_STOP;

    switch (stop_bits) {
        case 1: break;
        case 2: copy |= USART_CR2_STOP_1; break;
        default: return;
    }

    backend->uart->CR2 = copy;
}

static inline void stm32u5_usart1_set_parity(struct uart_dev *dev, uart_parity_t parity) {
    stm32u5_usart1_backend_t *backend = (stm32u5_usart1_backend_t *) dev->backend;

    uint32_t copy = backend->uart->CR1;
    copy &= ~(USART_CR1_PCE | USART_CR1_PS);

    switch (parity) {
        case UART_PARITY_NONE: break;
        case UART_PARITY_EVEN: copy |= USART_CR1_PCE; break;
        case UART_PARITY_ODD:  copy |= (USART_CR1_PCE | USART_CR1_PS); break;
        default: return;
    }

    backend->uart->CR1 = copy;
}

static void stm32u5_usart1_init(struct uart_dev *dev, uart_config_t *config) {
    stm32u5_usart1_set_baudrate(dev, config->baudrate);
    stm32u5_usart1_set_word_length(dev, config->word_length);
    stm32u5_usart1_set_stop_bits(dev, config->stop_bits);
    stm32u5_usart1_set_parity(dev, config->parity);

    stm32u5_usart1_backend_t *backend = (stm32u5_usart1_backend_t *) dev->backend;
    backend->uart->CR1 |= (USART_CR1_UE | USART_CR1_TE | USART_CR1_RE);
}

static void stm32u5_usart1_write(struct uart_dev *dev, const uint8_t *data, size_t len) {
    stm32u5_usart1_backend_t *backend = (stm32u5_usart1_backend_t *) dev->backend;

    if (len > 0) {
        while (len--) {
            while (!(backend->uart->ISR & USART_ISR_TXE));
            backend->uart->TDR = *data++;
        }
        while (!(backend->uart->ISR & USART_ISR_TC));
    }
}

static size_t stm32u5_usart1_read(struct uart_dev *dev, uint8_t *data, size_t len) {
    stm32u5_usart1_backend_t *backend = (stm32u5_usart1_backend_t *) dev->backend;

    size_t count = 0;
    while (count < len) {
        while (!(backend->uart->ISR & USART_ISR_RXNE));
        data[count++] = (uint8_t) backend->uart->RDR;
    }
    return count;
}

static const uart_driver_api_t stm32u5_usart1_api = {
    .init  = stm32u5_usart1_init,
    .write = stm32u5_usart1_write,
    .read  = stm32u5_usart1_read,
};

void stm32u5_usart1_create(uart_dev_t *dev,
                            USART_TypeDef *uart,
                            stm32u5_usart1_backend_t *backend) {
    backend->uart = uart;
    dev->api     = &stm32u5_usart1_api;
    dev->backend = backend;
}
