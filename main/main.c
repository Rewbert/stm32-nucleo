// #include <stdint.h>
// #include <stdlib.h>
#include <stdio.h>
#include "stm32l5xx.h"

#include "uart.h"
#include "gpio.h"
#include "clock.h"
#include "timer.h"
#include "button.h"

void sys_init(void) {
    configure_clock();
    SysTick_Config(110000);
    __enable_irq();
    enable_lpuart1();
}

void my_button_callback() {
    printf("hello from the button callback!\r\n");
}

void main(void) {
    sys_init();

    initialise_led(red_led);
    initialise_led(blue_led);
    initialise_led(green_led);

    // setvbuf(stdin, NULL, _IONBF, 0); // this code works
    // while(1) {
    //     printf("please type a character\r\n");
    //     char c = getchar();
    //     printf("received %c\r\n", c);
    // }

    /*
    This configures the blue user button, B1, which is bound to PC13. It is handled specially, and it is
    enough to write this code to use it.

    To add additional buttons to other GPIO pins, instead use the CONFIGURE_BUTTON(port, pin) macro, and
    write the actual interrupt routine yoursef. Please see void exti13_handler(void) in `button.c`. You must
    write a similar handler for your other inputs.

    NOTE: The way EXTI works, only one GPIO-n can be bound per EXTI-n. We can bind e.g. either of
    PA13, PB13, ..., PH13 to the EXTI13 line, but not several.
    */
    CONFIGURE_BUTTON_B1();
    add_b1_callback(&my_button_callback);

    while(1) {
        toggle_led(red_led);
        toggle_led(blue_led);
        toggle_led(green_led);
        printf("hello world from printf!\r\n");
        delay_ms(500);
    }
}