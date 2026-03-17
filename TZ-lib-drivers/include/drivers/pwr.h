#ifndef DRIVERS_PWR_H
#define DRIVERS_PWR_H

typedef enum {
    PWR_RANGE_1=0,
    PWR_RANGE_2,
    PWR_RANGE_3,
    PWR_RANGE_4,
} pwr_voltage_scaling_t;

struct pwr_dev;

typedef struct {
    void (*enable_vddio2)(struct pwr_dev *dev);
    void (*disable_vddio2)(struct pwr_dev *dev);

    /* Sets voltage scaling and waits until it is stable, meaning that it is blocking */
    void (*set_voltage_scaling)(struct pwr_dev *dev, pwr_voltage_scaling_t range);
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

static inline void pwr_set_voltage_scaling(pwr_dev_t *dev, pwr_voltage_scaling_t range) {
    dev->api->set_voltage_scaling(dev, range);
}

#endif // DRIVERS_PWR_H