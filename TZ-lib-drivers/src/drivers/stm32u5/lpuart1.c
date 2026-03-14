
#include "domain/domain.h"
#include "domain/cmsis_select.h"

#include "backends/stm32u5/lpuart1.h"

#include "stm32u5xx.h"

/* USART_TypeDef register layout (CR1, CR2, BRR, ISR, TDR, RDR) is identical on
 * STM32U5 and STM32L5. The only change is the BRR formula: the U5 SYSCLK target
 * is 160 MHz instead of 110 MHz. */

#define LPUART1_CLOCK_HZ 160000000ULL

static inline void stm32u5_set_baudrate(struct uart_dev *dev, uint32_t baudrate) {
    stm32u5_lpuart1_backend_t *backend = (stm32u5_lpuart1_backend_t*) dev->backend;

    /* LPUART BRR = 256 * f_clk / baudrate. Use 64-bit arithmetic to avoid overflow. */
    uint32_t brr = (uint32_t) ((256ULL * LPUART1_CLOCK_HZ) / baudrate);
    backend->uart->BRR = brr;
}

static inline void stm32u5_set_word_length(struct uart_dev *dev, uint8_t word_length) {
    stm32u5_lpuart1_backend_t *backend = (stm32u5_lpuart1_backend_t*) dev->backend;

    uint32_t copy = backend->uart->CR1;
    copy &= ~(USART_CR1_M0 | USART_CR1_M1);

    switch(word_length) {
        case 7: copy |= USART_CR1_M1; break;
        case 8: break;
        case 9: copy |= USART_CR1_M0; break;
        default: return;
    }

    backend->uart->CR1 = copy;
}

static inline void stm32u5_set_stop_bits(struct uart_dev *dev, uint8_t stop_bits) {
    stm32u5_lpuart1_backend_t *backend = (stm32u5_lpuart1_backend_t*) dev->backend;

    uint32_t copy = backend->uart->CR2;
    copy &= ~(USART_CR2_STOP);

    switch(stop_bits) {
        case 1: break;
        case 2: copy |= USART_CR2_STOP_1; break;
        default: return;
    }

    backend->uart->CR2 = copy;
}

static inline void stm32u5_set_parity(struct uart_dev *dev, uart_parity_t parity) {
    stm32u5_lpuart1_backend_t *backend = (stm32u5_lpuart1_backend_t*) dev->backend;

    uint32_t copy = backend->uart->CR1;
    copy &= ~(USART_CR1_PCE | USART_CR1_PS);

    switch(parity) {
        case UART_PARITY_NONE: break;
        case UART_PARITY_EVEN: copy |= USART_CR1_PCE; break;
        case UART_PARITY_ODD:  copy |= (USART_CR1_PCE | USART_CR1_PS); break;
        default: return;
    }

    backend->uart->CR1 = copy;
}

void stm32u5_init(struct uart_dev *dev, uart_config_t *config) {
    stm32u5_lpuart1_backend_t *backend = (stm32u5_lpuart1_backend_t*) dev->backend;

    stm32u5_set_baudrate(dev, config->baudrate);
    stm32u5_set_word_length(dev, config->word_length);
    stm32u5_set_stop_bits(dev, config->stop_bits);
    stm32u5_set_parity(dev, config->parity);

    backend->uart->CR1 |= (USART_CR1_UE | USART_CR1_TE | USART_CR1_RE);
}

void stm32u5_write(struct uart_dev *dev, const uint8_t *data, size_t len) {
    stm32u5_lpuart1_backend_t *backend = (stm32u5_lpuart1_backend_t*) dev->backend;

    if(len > 0) {
        while(len--) {
            while (!(backend->uart->ISR & USART_ISR_TXE));
            backend->uart->TDR = *data++;
        }
        while (!(backend->uart->ISR & USART_ISR_TC));
    }
}

size_t stm32u5_read(struct uart_dev *dev, uint8_t *data, size_t len) {
    stm32u5_lpuart1_backend_t *backend = (stm32u5_lpuart1_backend_t*) dev->backend;

    size_t count = 0;

    while (count < len) {
        while (!(backend->uart->ISR & USART_ISR_RXNE));
        data[count++] = (uint8_t)backend->uart->RDR;
    }

    return count;
}

static const uart_driver_api_t stm32u5_lpuart1_api = {
    .init  = stm32u5_init,
    .write = stm32u5_write,
    .read  = stm32u5_read,
};

void stm32u5_lpuart1_create(uart_dev_t *dev,
                             USART_TypeDef *uart,
                             stm32u5_lpuart1_backend_t *backend) {
    backend->uart = uart;

    dev->api     = &stm32u5_lpuart1_api;
    dev->backend = backend;
}
