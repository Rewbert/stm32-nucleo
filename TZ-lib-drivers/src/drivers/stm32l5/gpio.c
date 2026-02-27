
#include "domain/domain.h"
#include "domain/cmsis_select.h"

#include "drivers/gpio.h"

#include "stm32l5xx.h"

/**
 * @brief This is information that the device driver needs to operate a unique GPIO
 */
typedef struct {
    GPIO_TypeDef *gpio;
    uint8_t       pin;
} stm32l5_gpio_backend_t;

static inline uint32_t pin_shift(uint8_t pin) {
    return pin * 2U;
}

/* Helpers */

static inline void stm32l5_gpio_set_mode(stm32l5_gpio_backend_t *backend, gpio_mode_t mode) {
    backend->gpio->MODER &= ~(0x3U << pin_shift(backend->pin));
    backend->gpio->MODER |= (((uint32_t) mode) << pin_shift(backend->pin));
}

static inline void stm32l5_gpio_set_pupdr(stm32l5_gpio_backend_t *backend, gpio_pull_t pull) {
    backend->gpio->PUPDR &= ~(0x3U << pin_shift(backend->pin));
    backend->gpio->PUPDR |= (((uint32_t) pull) << pin_shift(backend->pin));
}

static inline void stm32l5_gpio_set_af(stm32l5_gpio_backend_t *backend, gpio_mode_t mode, gpio_af_t af) {
    if(mode == GPIO_MODE_AF) {
        if(backend->pin >= 0 && backend->pin < 8) {
            backend->gpio->AFR[0] &= ~(0xFU << (backend->pin * 4));
            backend->gpio->AFR[0] |= (af << (backend->pin * 4));
        } else if(backend->pin >= 8 && backend->pin < 16) {
            backend->gpio->AFR[1] &= ~(0xFU << ((backend->pin - 8) * 4));
            backend->gpio->AFR[1] |= (af << ((backend->pin - 8) * 4));
        }
    }
}

/* Driver API functions */

static void stm32l5_gpio_init(struct gpio_dev *dev, gpio_config_t *config) {
    // disable interrupts here

    stm32l5_gpio_backend_t *backend = (stm32l5_gpio_backend_t*) dev->backend;

    stm32l5_gpio_set_mode(backend, config->mode);
    stm32l5_gpio_set_pupdr(backend, config->pull);
    stm32l5_gpio_set_af(backend, config->mode, config->alternate);

    // enable interrupts here
}

static void stm32l5_gpio_write(struct gpio_dev *dev, gpio_level_t level) {
    stm32l5_gpio_backend_t *backend = (stm32l5_gpio_backend_t*) dev->backend;
    uint32_t mask = 0x1U << backend->pin;

    if(level == GPIO_LOW) {
        backend->gpio->BSRR = mask << 16;
    } else {
        backend->gpio->BSRR = mask;
    }
}

static gpio_level_t stm32l5_gpio_read(struct gpio_dev *dev) {
    stm32l5_gpio_backend_t *backend = (stm32l5_gpio_backend_t*) dev->backend;

    uint32_t val = backend->gpio->IDR & (0x1U << backend->pin);
    return val ? GPIO_HIGH : GPIO_LOW;
}

static void stm32l5_gpio_toggle(struct gpio_dev *dev) {
    stm32l5_gpio_backend_t *backend = (stm32l5_gpio_backend_t*) dev->backend;

    /**
     * @brief ODR represents the latest output _we_ wrote to the pin, but not necessarily the true value. For that, we
     * should look at IDR (the real-time voltage reading). However, I do not believe that I will have different
     * peripherals/pieces of code fighting for the same pin, so modifying ODR should probably be OK enough. I believe
     * that ODR will accurately reflect what state the actual pin has.
     */
    backend->gpio->ODR ^= (0x1U << backend->pin);
}

/* Wrapping up */

static const gpio_driver_api_t stm32l5_gpio_api = {
    .init   = stm32l5_gpio_init,
    .write  = stm32l5_gpio_write,
    .read   = stm32l5_gpio_read,
    .toggle = stm32l5_gpio_toggle,
};

void stm32l5_gpio_create(gpio_dev_t *dev,
                      GPIO_TypeDef *port,
                      uint8_t pin,
                      stm32l5_gpio_backend_t *backend_storage) {
    backend_storage->gpio = port;
    backend_storage->pin  = pin;

    dev->api = &stm32l5_gpio_api;
    dev->backend = backend_storage;
}