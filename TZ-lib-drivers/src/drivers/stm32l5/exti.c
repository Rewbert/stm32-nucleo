#include "domain/domain.h"
#include "domain/cmsis_select.h"

#include "drivers/exti.h"

#include "stm32l5xx.h"

typedef struct {
    uint8_t pin;
} stm32l5_exti_backend_t;

static exti_callback_t exti_callbacks[16] = {0};

static inline void stm32l5_exti_route_pin(uint8_t pin, exti_port_t port) {
    // There are different CR registers for different pins, so we must compute which one to use. It is divisible by 4, so
    // shifting the bits twice should be enough to figure it out
    uint8_t  cr    = pin >> 2;
    uint32_t shift = (pin & 0x3U) * 8U;

    EXTIx->EXTICR[cr] &= ~(0xFFUL << shift);
    EXTIx->EXTICR[cr] |=  ((uint32_t)port << shift);
}

static inline void stm32l5_exti_set_edge(uint8_t pin, exti_edge_t edge) {
    switch (edge) {
        case EXTI_EDGE_RISING:
            EXTIx->RTSR1 |=  (1U << pin);
            EXTIx->FTSR1 &= ~(1U << pin);
            break;
        case EXTI_EDGE_FALLING:
            EXTIx->FTSR1 |=  (1U << pin);
            EXTIx->RTSR1 &= ~(1U << pin);
            break;
        case EXTI_EDGE_BOTH:
            EXTIx->RTSR1 |= (1U << pin);
            EXTIx->FTSR1 |= (1U << pin);
            break;
    }
}

/* Driver API functions */

static void stm32l5_exti_init(struct exti_dev *dev, exti_config_t *config) {
    stm32l5_exti_backend_t *backend = (stm32l5_exti_backend_t *) dev->backend;
    backend->pin = config->pin;

    stm32l5_exti_route_pin(config->pin, config->port);
    stm32l5_exti_set_edge(config->pin, config->edge);

    EXTIx->IMR1 |= (1U << config->pin);

#if HAL_SECURE
    if (config->secure) {
        EXTIx->SECCFGR1  |= (1U << config->pin);
        EXTIx->PRIVCFGR1 |= (1U << config->pin);
    }
#endif

    int irqn = config->pin + 11;
    NVIC_SetPriority(irqn, config->priority);
    NVIC_EnableIRQ(irqn);

#if HAL_SECURE
    if (config->target_nonsecure) {
        NVIC_SetTargetState(irqn);
    }
#endif
}

static void stm32l5_exti_register_callback(struct exti_dev *dev, exti_callback_t cb) {
    stm32l5_exti_backend_t *backend = (stm32l5_exti_backend_t *) dev->backend;
    if (backend->pin < 16) {
        exti_callbacks[backend->pin] = cb;
    }
}

static void stm32l5_exti_enable(struct exti_dev *dev) {
    stm32l5_exti_backend_t *backend = (stm32l5_exti_backend_t *) dev->backend;
    EXTIx->IMR1 |= (1U << backend->pin);
}

static void stm32l5_exti_disable(struct exti_dev *dev) {
    stm32l5_exti_backend_t *backend = (stm32l5_exti_backend_t *) dev->backend;
    EXTIx->IMR1 &= ~(1U << backend->pin);
}

/* IRQ handlers */

#define EXTI_HANDLER(n)                                     \
void exti##n##_handler(void) {                              \
    if (EXTIx->FPR1 & (1U << n)) {                          \
        EXTIx->FPR1 = (1U << n);                            \
        if (exti_callbacks[n]) {                            \
            exti_callbacks[n](EXTI_EDGE_FALLING);           \
        }                                                   \
    }                                                       \
    if (EXTIx->RPR1 & (1U << n)) {                          \
        EXTIx->RPR1 = (1U << n);                            \
        if (exti_callbacks[n]) {                            \
            exti_callbacks[n](EXTI_EDGE_RISING);            \
        }                                                   \
    }                                                       \
}

EXTI_HANDLER(0)
EXTI_HANDLER(1)
EXTI_HANDLER(2)
EXTI_HANDLER(3)
EXTI_HANDLER(4)
EXTI_HANDLER(5)
EXTI_HANDLER(6)
EXTI_HANDLER(7)
EXTI_HANDLER(8)
EXTI_HANDLER(9)
EXTI_HANDLER(10)
EXTI_HANDLER(11)
EXTI_HANDLER(12)
EXTI_HANDLER(13)
EXTI_HANDLER(14)
EXTI_HANDLER(15)

/* Wrapping up */

static const exti_driver_api_t stm32l5_exti_api = {
    .init              = stm32l5_exti_init,
    .register_callback = stm32l5_exti_register_callback,
    .enable            = stm32l5_exti_enable,
    .disable           = stm32l5_exti_disable,
};

void stm32l5_exti_create(exti_dev_t *dev, stm32l5_exti_backend_t *backend_storage) {
    dev->api     = &stm32l5_exti_api;
    dev->backend = backend_storage;
}
