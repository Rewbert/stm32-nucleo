#ifndef GPIO_H
#define GPIO_H

#include "stm32l5xx.h"

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

extern struct GPIO *red_led;
extern struct GPIO *blue_led;
extern struct GPIO *green_led;

int is_port_enabled(struct GPIO *gpio);
void enable_port(struct GPIO *gpio);
void initialise_led(struct GPIO *led);
int read_led(struct GPIO *led);
void set_led(struct GPIO *led, int val);
void toggle_led(struct GPIO *led);

#endif /* GPIO_H */