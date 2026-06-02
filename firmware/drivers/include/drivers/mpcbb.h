#ifndef DRIVERS_MCPBB_H
#define DRIVERS_MCPBB_H

#include <stdint.h>

/**
 * @brief This peripheral (MPCBBx) protects SRAM.
 *
 * STM32L5: It is done on a block-basis, where each block seems to be 256 bytes.
 * Internally, the MPCBBx peripheral appears to organise blocks into superblocks of 32 each, taking up 8KB.
 * MPCBB1 protects SRAM1 (24 superblocks, 192KB), whereas MPCBB2 protects SRAM2 (8 superblocks, 64KB)
 *
 * STM32U5: Each block is 512 bytes, and each superblock is 32 such blocks, taking up 16KB.
 * I have yet to try this, but it appears as if this device has 4 SRAM's and MPCBB's.
 *
 * My driver API here exposes only a superblock configuration function, because I do not need smaller granularity.
 * It can be added later if it is required, I suppose.
 * 
 */

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

#endif // DRIVERS_MPCBB_H