#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "stm32l5xx.h"

#include "uart.h"
#include "gpio.h"

#define LED_PIN_PA9 9 // PA9 is one of the LEDs
// GPIOA_PIN9 is off 0x14 base 0x42020000
#define LED_PIN_PC7 7
#define LED_PIN_PB7 7

void configure_clock(void);

// since the systick handler in startup.c is weak aliased, this will override that one
// and take effect
uint32_t ticks;
void systick_handler() {
    ticks++;
}

// this implements a delay by busy looping
void delay_ms(uint32_t milliseconds) {
    uint32_t start = ticks;
    uint32_t end = start + milliseconds;

    if (end < start) {
        while (ticks > start);
    }
    while(ticks < end);
}

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

    // setvbuf(stdin, NULL, _IONBF, 0);
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