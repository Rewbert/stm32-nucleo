#include "stm32l5xx.h"
#include "clock.h"
#include "uart.h"
#include "gpio.h"

// #define INITIALIZATION
// void main_setup(void) {
//     configure_clock();
//     SysTick_Config(110000);
//     __enable_irq();
//     enable_lpuart1();
//     initialise_led(red_led);
// }

// void myexit(int n) {
//     initialise_led(red_led);
//     set_led(red_led, 0);
//     while(1) {}
// }

int ffs(int x) {
  if (!x)
    return 0;
  x &= -x;                      /* keep lowest bit */
  int i = __CLZ(x);             /* count leading 0s */
  return 32 - i;                /* 31 leading zeros should return 1 */
}

void toggle_red_led(void) {
  toggle_led(red_led);
}