#ifndef DRIVERS_TIMER_H
#define DRIVERS_TIMER_H

#include <stdint.h>

/**
 * @brief A basic hardware timer: a free-running counter with a prescaler and an
 * auto-reload value, wired to fire one interrupt (and one callback) when it
 * expires. It runs in one-pulse mode, so calling timer_start() re-arms it —
 * each call can pass a different tick count.
 */

struct timer_dev;

typedef void (*timer_callback_t)(void);

typedef struct {
    uint16_t prescaler; // divides the timer's input clock; the timer counts at (input clock) / (prescaler + 1)
} timer_config_t;

typedef struct {
    void (*init)(struct timer_dev *dev, timer_config_t *config);
    void (*register_callback)(struct timer_dev *dev, timer_callback_t cb);
    void (*start)(struct timer_dev *dev, uint32_t ticks);
    void (*stop)(struct timer_dev *dev);
    int  (*irqn)(struct timer_dev *dev);
} timer_driver_api_t;

typedef struct timer_dev {
    const timer_driver_api_t *api;
    void *backend;
} timer_dev_t;

static inline void timer_init(timer_dev_t *dev, timer_config_t *config) {
    dev->api->init(dev, config);
}

static inline void timer_register_callback(timer_dev_t *dev, timer_callback_t cb) {
    dev->api->register_callback(dev, cb);
}

static inline void timer_start(timer_dev_t *dev, uint32_t ticks) {
    dev->api->start(dev, ticks);
}

static inline void timer_stop(timer_dev_t *dev) {
    dev->api->stop(dev);
}

static inline int timer_irqn(timer_dev_t *dev) {
    return dev->api->irqn(dev);
}

#endif // DRIVERS_TIMER_H
