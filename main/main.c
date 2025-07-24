// #include <stdint.h>
// #include <stdlib.h>
#include <stdio.h>
#include "stm32l5xx.h"

#include "uart.h"
#include "gpio.h"
#include "clock.h"
#include "timer.h"

void sys_init(void) {
    configure_clock();
    SysTick_Config(110000);
    __enable_irq();
    enable_lpuart1();
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

    while(1) {
        toggle_led(red_led);
        toggle_led(blue_led);
        toggle_led(green_led);
        printf("hello world from printf!\r\n");
        delay_ms(500);
    }
}