#ifndef CONFIG_STM32L5_H
#define CONFIG_STM32L5_H

#define WANT_STDIO 1

#define WANT_FLOAT 1

#define WANT_MD5 0

#define WANT_TICK 0

#define WANT_TICK 0

#define WANT_ARGS 0

#define GCRED    0
#define FASTTAGS 1
#define INTTABLE 1
#define SANITY   1
#define STACKOVL 1

#define HEAP_CELLS 8000
#define STACK_SIZE 500

#include "stm32l5xx.h"
#include "clock.h"
#include "uart.h"
#include "gpio.h"

#define INITIALIZATION
void
main_setup(void) {
    configure_clock();
    SysTick_Config(110000);
    __enable_irq();
    enable_lpuart1();
    initialise_led(red_led);
}

void myexit(int n) {
    initialise_led(red_led);
    set_led(red_led, 0);
    while(1) {}
}

#define EXIT myexit

int
ffs(int x)
{
  if (!x)
    return 0;
  x &= -x;                      /* keep lowest bit */
  int i = __CLZ(x);             /* count leading 0s */
  return 32 - i;                /* 31 leading zeros should return 1 */
}
#define FFS ffs

void toggle_red_led(void) {
  toggle_led(red_led);
}

// extern void mhs_from_Unit(int, int);
// extern int mhs_to_Int(int, int);

// void toggle_red_led(int s) {
//     toggle_led(red_led);
//     mhs_from_Unit(s,0);
// }

// void delay_ms(uint32_t milliseconds);

// void mhs_delay(int s) {
//   int value = mhs_to_Int(s, 0);
//   delay_ms(value);
//   mhs_from_Unit(s, 1);
// }

// #define FFI_EXTRA \
//   { "toggle_led", toggle_red_led }, \
//   { "delay", mhs_delay },

#endif /* CONFIG_STM32L5_H */