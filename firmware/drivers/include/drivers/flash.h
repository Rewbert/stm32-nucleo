#ifndef DRIVERS_FLASH_H
#define DRIVERS_FLASH_H

#include <stdint.h>

struct flash_dev;

typedef struct {
    void (*set_latency)(struct flash_dev *dev, uint32_t wait_states);
    uint32_t (*get_latency)(struct flash_dev *dev);

    /* read/program/erase operate on absolute addresses in this domain's flash
     * alias. program() and erase_page() drive the FLASH controller (secure or
     * non-secure register bank, selected at build time); read() is a plain copy
     * from memory-mapped flash. All return 0 on success, -1 on error.
     * program(): addr and len must be program-unit aligned.
     * erase_page(): erases the whole erase page containing addr. */
    int (*read)(struct flash_dev *dev, uint32_t addr, void *buf, uint32_t len);
    int (*program)(struct flash_dev *dev, uint32_t addr, const void *data, uint32_t len);
    int (*erase_page)(struct flash_dev *dev, uint32_t addr);

    /* Flash geometry: bytes per program unit (L5: 8, U5: 16) and erase-page size. */
    uint32_t (*prog_size)(struct flash_dev *dev);
    uint32_t (*page_size)(struct flash_dev *dev);
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

static inline int flash_read(flash_dev_t *dev, uint32_t addr, void *buf, uint32_t len) {
    return dev->api->read(dev, addr, buf, len);
}

static inline int flash_program(flash_dev_t *dev, uint32_t addr, const void *data, uint32_t len) {
    return dev->api->program(dev, addr, data, len);
}

static inline int flash_erase_page(flash_dev_t *dev, uint32_t addr) {
    return dev->api->erase_page(dev, addr);
}

static inline uint32_t flash_prog_size(flash_dev_t *dev) {
    return dev->api->prog_size(dev);
}

static inline uint32_t flash_page_size(flash_dev_t *dev) {
    return dev->api->page_size(dev);
}

#endif // DRIVERS_FLASH_H