#ifndef DRIVERS_FLASH_H
#define DRIVERS_FLASH_H

#include <stdint.h>

struct flash_dev;

typedef struct {
    void (*set_latency)(struct flash_dev *dev, uint32_t wait_states);
    uint32_t (*get_latency)(struct flash_dev *dev);
} flash_driver_api_t;

typedef struct flash_dev {
    const flash_driver_api_t *api;
    void *backend;
} flash_dev_t;

static inline void flash_set_latency(flash_dev_t *dev, uint32_t ws) {
    dev->api->set_latency(dev, ws);
}

static inline uint32_t flash_get_latency(flash_dev_t *dev) {
    return dev->api->get_latency(dev);
}

#endif // DRIVERS_FLASH_H