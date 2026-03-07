#ifndef DRIVERS_TZSC_H                                                                                                                                                                      
#define DRIVERS_TZSC_H

#include <stdint.h>

/**
* @brief TZSC: TrustZone Security Controller.
*
* Peripherals can be marked as belonging to either the secure of non-secure world.
* This peripheral controls that. Only the secure application can configure TZSC.
*
* The TZSC can be locked via tzsc_lock(), after which security configuration
* is frozen until the next reset.
*/

struct tzsc_dev;

typedef enum {
    TZSC_SECURE    = 0,
    TZSC_NONSECURE = 1,
} tzsc_security_t;

/**
 * @brief I've only tries using one or two of these peripherals, (lpuart1 and one of the timers), but
 * I've pulled all these other names from the datasheet. There is one bit each controlling their clock.
 * 
 */
typedef enum {
    /* SECCFGR1 peripherals */
    TZSC_PERIPH_TIM2,
    TZSC_PERIPH_TIM3,
    TZSC_PERIPH_TIM4,
    TZSC_PERIPH_TIM5,
    TZSC_PERIPH_TIM6,
    TZSC_PERIPH_TIM7,
    TZSC_PERIPH_WWDG,
    TZSC_PERIPH_IWDG,
    TZSC_PERIPH_SPI2,
    TZSC_PERIPH_SPI3,
    TZSC_PERIPH_USART2,
    TZSC_PERIPH_USART3,
    TZSC_PERIPH_UART4,
    TZSC_PERIPH_UART5,
    TZSC_PERIPH_I2C1,
    TZSC_PERIPH_I2C2,
    TZSC_PERIPH_I2C3,
    TZSC_PERIPH_CRS,
    TZSC_PERIPH_DAC1,
    TZSC_PERIPH_OPAMP,
    TZSC_PERIPH_LPTIM1,
    TZSC_PERIPH_LPUART1,
    TZSC_PERIPH_I2C4,
    TZSC_PERIPH_LPTIM2,
    TZSC_PERIPH_LPTIM3,
    TZSC_PERIPH_FDCAN1,
    TZSC_PERIPH_USBFS,
    TZSC_PERIPH_UCPD1,
    TZSC_PERIPH_VREFBUF,
    TZSC_PERIPH_COMP,
    TZSC_PERIPH_TIM1,
    TZSC_PERIPH_SPI1,
    /* SECCFGR2 peripherals */
    TZSC_PERIPH_TIM8,
    TZSC_PERIPH_USART1,
    TZSC_PERIPH_TIM15,
    TZSC_PERIPH_TIM16,
    TZSC_PERIPH_TIM17,
    TZSC_PERIPH_SAI1,
    TZSC_PERIPH_SAI2,
    TZSC_PERIPH_DFSDM1,
    TZSC_PERIPH_CRC,
    TZSC_PERIPH_TSC,
    TZSC_PERIPH_ICACHE_REG,
    TZSC_PERIPH_ADC,
    TZSC_PERIPH_HASH,
    TZSC_PERIPH_RNG,
    TZSC_PERIPH_SDMMC1,
    TZSC_PERIPH_FMC_REG,
    TZSC_PERIPH_OCTOSPI1_REG,
} tzsc_periph_t;

typedef struct {
    void (*set_periph)(struct tzsc_dev *dev, tzsc_periph_t periph, tzsc_security_t sec);
    void (*lock)(struct tzsc_dev *dev);
} tzsc_driver_api_t;

typedef struct tzsc_dev {
    const tzsc_driver_api_t *api;
    void *backend;
} tzsc_dev_t;

static inline void tzsc_set_periph(tzsc_dev_t *dev, tzsc_periph_t periph, tzsc_security_t sec) {
    dev->api->set_periph(dev, periph, sec);
}

static inline void tzsc_lock(tzsc_dev_t *dev) {
    dev->api->lock(dev);
}

#endif // DRIVERS_TZSC_H
