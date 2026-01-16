#ifndef CORE_CLOCK_H
#define CORE_CLOCK_H

typedef enum {
    AHB1 = 0,
    AHB2,
    AHB3,
    APB1,
    APB2,
} peripheral_clock_t;

typedef enum {
    PERIPH_GPIO,
    PERIPH_USART,
    PERIPH_SPI,
    PERIPH_I2C,
    PERIPH_DMA,
} peripheral_type_t;

typedef enum {
    PCLK1=0,
    SYSCLK,
    HSI16,
    LSE,
} clock_t;

#endif // CORE_CLOCK_H