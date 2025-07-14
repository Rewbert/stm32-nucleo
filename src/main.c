#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "stm32l5xx.h"

#include "uart.h"

#define LED_PIN 9 // PA9 is one of the LEDs
// GPIOA_PIN9 is off 0x14 base 0x42020000

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

void main(void) {
    configure_clock(); // configure clock to run faster

    SysTick_Config(110000); // this gives us millisecond precision
    __enable_irq();

    enable_lpuart1();

    /* THIS LITTLE BLOCK MANUALLY ENABLES AN LED (PA9) */
    // enable the peripheral clock
    RCC->AHB2ENR |= (1 << RCC_AHB2ENR_GPIOAEN_Pos);

    volatile uint32_t dummy;
    dummy = RCC->AHB2ENR;
    dummy = RCC->AHB2ENR;

    // set PA9 as output
    GPIOA->MODER &= ~(GPIO_MODER_MODE9_Msk);
    GPIOA->MODER |= (1 << GPIO_MODER_MODE9_Pos);
    /* END OF LITTLE BLOCK */

    // toggle the pin on and off
    while(1) {
        GPIOA->ODR ^= (1 << LED_PIN);
        printf("hello world from printf!\r\n");
        delay_ms(500);
    }
}