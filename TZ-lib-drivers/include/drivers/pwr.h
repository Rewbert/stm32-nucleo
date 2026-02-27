#ifndef DRIVERS_PWR_H
#define DRIVERS_PWR_H

struct pwr_dev;

typedef struct {
    void (*enable_vddio2)(struct pwr_dev *dev);
    void (*disable_vddio2)(struct pwr_dev *dev);
} pwr_driver_api_t;

typedef struct pwr_dev {
    const pwr_driver_api_t *api;
    void *backend;
} pwr_dev_t;

static inline void pwr_enable_vddio2(pwr_dev_t *dev) {
    dev->api->enable_vddio2(dev);
}

static inline void pwr_disable_vddio2(pwr_dev_t *dev) {
    dev->api->disable_vddio2(dev);
}

#endif // DRIVERS_PWR_H