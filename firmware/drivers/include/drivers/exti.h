#ifndef DRIVERS_EXTI_H                                                                                                                                                                      
#define DRIVERS_EXTI_H                                                                                                                                                                      
                                                                                                                                                                                              
#include <stdint.h>                                                                                                                                                                         

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

typedef enum {
    EXTI_SECURE = 0,
    EXTI_NONSECURE,
} exti_security_t;

typedef struct {
    uint8_t port;                 // GPIO port; use exti_port_t values
    uint8_t pin;                  // Pin 0 to 15
    uint8_t edge;                 // use exti_edge_t values
} exti_config_t;

typedef struct {
    void (*init)(struct exti_dev *dev, exti_config_t *config);
    void (*register_callback)(struct exti_dev *dev, exti_callback_t cb);
    void (*set_security)(struct exti_dev *dev, exti_security_t security);
    int  (*irqn)(struct exti_dev *dev);
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

static inline void exti_set_security(exti_dev_t *dev, exti_security_t security) {
    dev->api->set_security(dev, security);
}

static inline int exti_irqn(exti_dev_t *dev) {
    return dev->api->irqn(dev);
}

static inline void exti_enable(exti_dev_t *dev) {
    dev->api->enable(dev);
}

static inline void exti_disable(exti_dev_t *dev) {
    dev->api->disable(dev);
}

#endif // DRIVERS_EXTI_H
