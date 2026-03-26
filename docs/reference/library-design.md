# Library Design Principles

The library is used to acess the peripherals of the board without having to worry about registers and such details. The library is aware of TrustZone, meaning that some operations can only be done by the secure application.

An important design choice is that _every peripheral is compiled twice_, once for the secure application and once for the nonsecure one. Functionality is conditionally inserted by guards of `#if HAL_SECURE`.

## Library Structure

The library is implemented using a driver model. The idea is that a generic API is defined in a driver format, and then the driver is reimplemented once per board. There are currently two implementations of each peripheral, one for STM32L5 and one for STM32U5. The structure is

```
TZ-lib-drivers/
    include/
        backends/
            stm32l5/
                gpio.h
                ...
            stm32u5/
                gpio.h
                ...
        domain/
        drivers/
            gpio.h
            ...
    src/
        stm32l5/
            gpio.h
            ...
        stm32u5/
            gpio.h
            ...
        sau.c
        systick.c
```

I have singled out the `gpio.h` driver to showcase where GPIO relevant files would go, when adding support for a generic GPIO driver.

## Driver Model

A device is a struct holding two things, a pointer to an API, and a pointer to an opaqie, board-specific state. The GPIO device driver looks like this

```
typedef struct gpio_dev {
    const gpio_driver_api_t *api;
    void *backend;
} gpio_dev_t;
```

The `backend` should never be touched by application code, but will instead be used by the device functions from the vtable `api`. It will carry hardware-specific information required to control the peripheral.

Each of the API functions take the whole device driver as input (aside from other required parameters). The GPIO vtable looks like

```
typedef struct {
    void (*init)(struct gpio_dev *dev, gpio_config_t *config);
    void (*write)(struct gpio_dev *dev, gpio_level_t level);
    gpio_level_t (*read)(struct gpio_dev *dev);
    void (*toggle)(struct gpio_dev *dev);
} gpio_driver_api_t;
```

There is a _create_ functions for every driver, that is board-specific and will make sure to initialise the `backend` and construct the vtable properly. The `backend` type itself may differ between boards, and is therefore defined once for each board, in `include/backend/`. For STM32L5, the backend definition is

```
#ifndef BACKENDS_STM32L5_GPIO_H
#define BACKENDS_STM32L5_GPIO_H

#include <stdint.h>
#include "drivers/gpio.h"
#include "stm32l5xx.h"

typedef struct {
    GPIO_TypeDef *gpio;
    uint8_t       pin;
} stm32l5_gpio_backend_t;

void stm32l5_gpio_create(gpio_dev_t *dev,
                         GPIO_TypeDef *port,
                         uint8_t pin,
                         stm32l5_gpio_backend_t *backend);

#endif // BACKENDS_STM32L5_GPIO_H
```

## Secure vs Non-Secure

By default, a majority of the peripherals on the boards are owned by the secure application after reset. This means that the nonsecure application cannot do anything with them unless the secure application 'releases them' to the nonsecure world.

An example is the MPCBB driver. It configures the security policy of the SRAM, and can only be operated by the secure application. The generic API for the MPCBB is

```
struct mpcbb_dev;

typedef enum {
    MPCBB_SECURE    = 0,
    MPCBB_NONSECURE = 1,
} mpcbb_security_t;

typedef struct {
    void (*set_superblocks)(struct mpcbb_dev *dev,
                            uint32_t first, uint32_t count,
                            mpcbb_security_t sec);
    void (*lock)(struct mpcbb_dev *dev);
} mpcbb_driver_api_t;

typedef struct mpcbb_dev {
    const mpcbb_driver_api_t *api;
    void *backend;
} mpcbb_dev_t;

static inline void mpcbb_set_superblocks(mpcbb_dev_t *dev,
                                         uint32_t first, uint32_t count,
                                         mpcbb_security_t sec) {
    dev->api->set_superblocks(dev, first, count, sec);
}

static inline void mpcbb_lock(mpcbb_dev_t *dev) {
    dev->api->lock(dev);
}
```

If we look at the actual implementation of this peripheral, the functionality is guarded by CPP macros

```
void stm32l5_set_superblocks(struct mpcbb_dev *dev, uint32_t first, uint32_t count, mpcbb_security_t sec) {
#if HAL_SECURE
  ...
#endif
}

void stm32l5_lock(struct mpcbb_dev *dev) {
#if HAL_SECURE
  ...
#endif
}

static const mpcbb_driver_api_t stm32l5_mpcbb_api = {
    .set_superblocks = stm32l5_set_superblocks,
    .lock = stm32l5_lock,
};

void stm32l5_mpcbb_create(mpcbb_dev_t *dev,
                          GTZC_MPCBB_TypeDef *mpcbb,
                          stm32l5_mpcbb_backend_t *backend) {
#if HAL_SECURE
  ...
#endif
```

The whole driver is functionally a NO-OP in the nonsecure application, as everything is guarded by `#if HAL_SECURE`.

## API Reference for Implemented Drivers
