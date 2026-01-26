// #include "config.h"

// #include "../MicroHs/src/runtime/eval.c"

#include "stm32l5xx.h"
#include "clock.h"
#include "uart.h"
#include "gpio.h"

int mhs_main(int argc, char **argv);

void stm32_init() {
    configure_clock();
    SysTick_Config(110000);
    __enable_irq();
    enable_lpuart1();
    initialise_led(red_led);
}

void stm32_exit() {
    initialise_led(red_led);
    set_led(red_led, 0);
    while(1) {}
}

void main(void) {

    stm32_init();

    mhs_main(0, 0);

}