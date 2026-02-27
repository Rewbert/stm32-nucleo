#ifndef DRIVERS_RCC_H
#define DRIVERS_RCC_H

#include "drivers/flash.h"

/**
 * @brief A simple attempt at a board-agnostic (up to STM32) RCC driver. There is an enum of peripherals, but
 * if a board does not implement a specific peripheral, engaging with it will result in either an error or a
 * no-op. I have not decided yet.
 */

struct rcc_dev;

/**
 * @brief Different peripherals.
 *
 * NOTE: This list is not exhaustive. These are just the peripherals I've engaged with.
 */
typedef enum {
    RCC_GPIOA = 0,
    RCC_GPIOB,
    RCC_GPIOC,
    RCC_GPIOD,
    RCC_GPIOE,
    RCC_GPIOF,
    RCC_GPIOG,
    RCC_GPIOH,

    RCC_LPUART1,
    RCC_PWR,
    RCC_GTZC,
    RCC_PERIPH_COUNT,
} rcc_periph_t;

typedef enum {
    RCC_PCKL1 = 0,
    RCC_PCKL2,
    RCC_SYSCLK,
    RCC_HSI16,
    RCC_HSI48,
    RCC_LSE,
    RCC_HSE,
    RCC_MSI,
    RCC_PLL_Q,
    RCC_PLL_SAI1_P,
    RCC_PLL_SAI1_R,
    RCC_NO_CLOCK,
    RCC_PERIPHERAL_CLOCK_COUNT,
} rcc_periph_clock_source_t;

/**
 * @brief API of the RCC driver.
 */
typedef struct {
    void (*enable)(struct rcc_dev *dev, rcc_periph_t periph);
    void (*disable)(struct rcc_dev *dev, rcc_periph_t periph);
    int (*is_enabled)(struct rcc_dev *dev, rcc_periph_t periph);
    void (*set_peripheral_clock)(struct rcc_dev *dev, rcc_periph_t periph, rcc_periph_clock_source_t clock);

    void (*configure_pll)(struct rcc_dev *dev, struct flash_dev *flash, uint32_t pll_n, uint32_t pll_m, uint32_t pll_div, uint32_t target_sysclk_hz);
} rcc_driver_api_t;

typedef struct rcc_dev {
    const rcc_driver_api_t *api;
    void *backend;
} rcc_dev_t;

/**
 * @brief Enable power to a peripheral
 * 
 */
static inline void rcc_enable(rcc_dev_t *dev, rcc_periph_t periph) {
    dev->api->enable(dev, periph);
}

/**
 * @brief Disable power to a peripheral
 * 
 */
static inline void rcc_disable(rcc_dev_t *dev, rcc_periph_t periph) {
    dev->api->disable(dev, periph);
}

/**
 * @brief Is the power to a peripheral enabled?
 * 
 */
static inline int rcc_is_enabled(rcc_dev_t *dev, rcc_periph_t periph) {
    return dev->api->is_enabled(dev, periph);
}

static inline void rcc_set_peripheral_clock(rcc_dev_t *dev, rcc_periph_t periph, rcc_periph_clock_source_t clock) {
    dev->api->set_peripheral_clock(dev, periph, clock);
}

static inline void rcc_configure_pll(struct rcc_dev *dev, struct flash_dev *flash, uint32_t pll_n, uint32_t pll_m, uint32_t pll_div, uint32_t target_sysclk_hz) {
    dev->api->configure_pll(dev, flash, pll_n, pll_m, pll_div, target_sysclk_hz);
}

#endif // DRIVERS_RCC_H