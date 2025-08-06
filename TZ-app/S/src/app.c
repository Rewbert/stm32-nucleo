#include "stm32l5xx.h"

#define ENABLE_IRQ()  __asm volatile ("cpsie i" : : : "memory")
#define DISABLE_IRQ() __asm volatile ("cpsid i" : : : "memory")

void configure_red_led(void) {
  volatile int dummy;
  RCC_S->AHB2ENR |= (1 << RCC_AHB2ENR_GPIOAEN_Pos);
  dummy = RCC_S->AHB2ENR;
  dummy = RCC_S->AHB2ENR;

  GPIOA_S->MODER &= ~(GPIO_MODER_MODE9_Msk);
  GPIOA_S->MODER |= (1 << GPIO_MODER_MODE9_Pos);
}

volatile uint32_t ticks;
void systick_handler() {
  ticks++;
}

void delay_ms(uint32_t milliseconds) {
  uint32_t start = ticks;
  uint32_t end = start + milliseconds;

  if (end < start) {
      while (ticks > start);
  }
  while(ticks < end);
}

void secure_app_initialise() {
  configure_red_led();

  GPIOA_S->ODR |= (1 << 9);

  SysTick_Config(4000);
  ENABLE_IRQ();

  while(1) {
    GPIOA_S->ODR ^= (1 << 9);
    delay_ms(500);
  }
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