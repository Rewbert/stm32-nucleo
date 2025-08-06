#include "stm32l5xx.h"

#define ENABLE_IRQ()  __asm volatile ("cpsie i" : : : "memory")
#define DISABLE_IRQ() __asm volatile ("cpsid i" : : : "memory")

void secure_app_initialise() {
    // configure_clock();
    // SysTick_Config2(110000);

    // ENABLE_IRQ();

    // initialise_led(red_led);
    // while(1) {
    //     toggle_led(red_led);
    //     delay_ms(500);
    // }
}

#define NSC __attribute__((cmse_nonsecure_entry))

int NSC add10(int a) {
  return 10+a;
}