#ifndef GPIO_H
#define GPIO_H

#include "stm32l5xx.h"

/* Creates a GPIO object that can be used to flash an LED */
#define MAKE_GPIO(name, port, pin) \
    struct GPIO name##_data = { &RCC->AHB2ENR, \
                                RCC_AHB2ENR_GPIO##port##EN_Pos, \
                                GPIO_MODER_MODE##pin##_Pos, \
                                GPIO_MODER_MODE##pin##_Msk, \
                                GPIO##port, \
                                pin, \
                              }; \
    struct GPIO *name = &name##_data; \

struct GPIO {
    /* turning on the port */
    volatile void *enable_register;
    int enable_position;

    /* mode configuration */
    int mode_pos;
    int mode_msk;

    GPIO_TypeDef *gpio;
    int pin;
};

/* Statically allocate 3 such structs to control the LEDs */
extern struct GPIO *red_led;
extern struct GPIO *blue_led;
extern struct GPIO *green_led;

// Check if the port the GPIO belongs to is powered on
int is_port_enabled(struct GPIO *gpio);

// Enable the port the GPIO is on
void enable_port(struct GPIO *gpio);

// Initialise the LED (turn on power to the port, and configure the GPIO)
void initialise_led(struct GPIO *led);

// Read the GPIO state (1 or 0)
int read_led(struct GPIO *led);

// Set the GPIO state (1 or 0)
void set_led(struct GPIO *led, int val);

// Toggle the LED, turning it on if it is off, or vise versa
void toggle_led(struct GPIO *led);

#endif /* GPIO_H */