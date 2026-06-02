#ifndef DRIVERS_EXTI_H                                                                                                                                                                      
#define DRIVERS_EXTI_H                                                                                                                                                                      
                                                                                                                                                                                              
#include <stdint.h>                                                                                                                                                                         
#include <stdbool.h>

struct exti_dev;

typedef enum {
    EXTI_EDGE_RISING = 0,
    EXTI_EDGE_FALLING,
    EXTI_EDGE_BOTH,
} exti_edge_t;

typedef enum {
    EXTI_PORT_A = 0,
    EXTI_PORT_B,
    EXTI_PORT_C,
    EXTI_PORT_D,
    EXTI_PORT_E,
    EXTI_PORT_F,
    EXTI_PORT_G,
    EXTI_PORT_H,
} exti_port_t;

typedef void (*exti_callback_t)(exti_edge_t edge);

typedef struct {
    exti_port_t port;             // GPIO port
    uint8_t     pin;              // Pin 0 to 15
    exti_edge_t edge;             // edge
    uint8_t     priority;         // IRQ priority for the NVIC
    bool        secure;           // is it a secure line?
    bool        target_nonsecure; // Route NVIC IRQ to non-secure world
} exti_config_t;

typedef struct {
    void (*init)(struct exti_dev *dev, exti_config_t *config);
    void (*register_callback)(struct exti_dev *dev, exti_callback_t cb);
    void (*enable)(struct exti_dev *dev);
    void (*disable)(struct exti_dev *dev);
} exti_driver_api_t;

typedef struct exti_dev {
    const exti_driver_api_t *api;
    void *backend;
} exti_dev_t;

static inline void exti_init(exti_dev_t *dev, exti_config_t *config) {
    dev->api->init(dev, config);
}

static inline void exti_register_callback(exti_dev_t *dev, exti_callback_t cb) {
    dev->api->register_callback(dev, cb);
}

static inline void exti_enable(exti_dev_t *dev) {
    dev->api->enable(dev);
}

static inline void exti_disable(exti_dev_t *dev) {
    dev->api->disable(dev);
}

#endif // DRIVERS_EXTI_H