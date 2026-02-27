#ifndef DRIVERS_GPIO_H
#define DRIVERS_GPIO_H

/**
 * @brief Please note that this GPIO driver is (as far as I understand it) STM32 specific. I mainly have
 * this low level experience with STM32 boards, where we configure stuff like MODE, PUPDR, etc. I
 * have come to known that e.g. NRF52 uses other concepts to manage peripherals. In the example of
 * GPIO, NRF52 have no concept of an alternating function like STM32 does.
 *
 * TL;DR - Don't try to implement this driver for anything other than an STM32 board.
 * 
 */

/* Forward declaring this struct so that I can use it in type signatures */
struct gpio_dev;

/* These are types related to using a GPIO */

typedef enum {
    GPIO_LOW = 0,
    GPIO_HIGH,
} gpio_level_t;

typedef enum {
    GPIO_NOPULL=0,
    GPIO_PULLUP,
    GPIO_PULLDOWN,
} gpio_pull_t;

typedef enum {
    GPIO_MODE_INPUT = 0,
    GPIO_MODE_OUTPUT,
    GPIO_MODE_AF,
    GPIO_MODE_ANALOG,
} gpio_mode_t;

typedef enum {
    GPIO_AF0  = 0,
    GPIO_AF1  = 1,
    GPIO_AF2  = 2,
    GPIO_AF3  = 3,
    GPIO_AF4  = 4,
    GPIO_AF5  = 5,
    GPIO_AF6  = 6,
    GPIO_AF7  = 7,
    GPIO_AF8  = 8,
    GPIO_AF9  = 9,
    GPIO_AF10 = 10,
    GPIO_AF11 = 11,
    GPIO_AF12 = 12,
    GPIO_AF13 = 13,
    GPIO_AF14 = 14,
    GPIO_AF15 = 15,
} gpio_af_t;

/* Rather than the API exposing individual functions to set mode, pull, etc, we just offer
   a init function that takes all of these options, and does them all (seemingly) at once. This
   should hopefully make us not enter an interrupt while we are in the middle of configuring
   an GPIO pin. */
typedef struct {
    gpio_mode_t mode;
    gpio_pull_t pull;
    gpio_af_t   alternate;
} gpio_config_t;

/* The API of the GPIO device driver */

typedef struct {
    void (*init)(struct gpio_dev *dev, gpio_config_t *config);

    void (*write)(struct gpio_dev *dev, gpio_level_t level);
    gpio_level_t (*read)(struct gpio_dev *dev);
    void (*toggle)(struct gpio_dev *dev);
} gpio_driver_api_t;

/* The device driver for GPIO */

typedef struct gpio_dev {
    const gpio_driver_api_t *api;
    void *backend;
} gpio_dev_t;

/* Convenience functions that I expose to the users */

static inline void gpio_init(gpio_dev_t *dev, gpio_config_t *config) {
    dev->api->init(dev, config);
}

static inline void gpio_write(gpio_dev_t *dev, gpio_level_t level) {
    dev->api->write(dev, level);
}

static inline gpio_level_t gpio_read(gpio_dev_t *dev) {
    return dev->api->read(dev);
}

static inline void gpio_toggle(gpio_dev_t *dev) {
    dev->api->toggle(dev);
}

#endif // DRIVERS_GPIO_H