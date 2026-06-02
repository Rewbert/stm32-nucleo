#ifndef DRIVERS_UART_H
#define DRIVERS_UART_H

#include <stdint.h>
#include <stddef.h>

struct uart_dev;

typedef enum {
    UART_PARITY_NONE = 0,
    UART_PARITY_EVEN,
    UART_PARITY_ODD,
} uart_parity_t;

typedef struct {
    uint32_t baudrate;
    uint8_t  word_length;
    uint8_t  stop_bits;
    uart_parity_t  parity;
} uart_config_t;

typedef struct {
    void (*init)(struct uart_dev *dev, uart_config_t *config);
    void (*write)(struct uart_dev *dev, const uint8_t *data, size_t len);
    size_t (*read)(struct uart_dev *dev, uint8_t *data, size_t len);
} uart_driver_api_t;

typedef struct uart_dev {
    const uart_driver_api_t *api;
    void *backend;
} uart_dev_t;

static inline void uart_init(uart_dev_t *dev, uart_config_t *config) {
    dev->api->init(dev, config);
}

static inline void uart_write(uart_dev_t *dev, const uint8_t *data, size_t len) {
    dev->api->write(dev, data, len);
}

static inline size_t uart_read(uart_dev_t *dev, uint8_t *data, size_t len) {
    return dev->api->read(dev, data, len);
}

#endif // DRIVERS_UART_H
