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

void make_red_led_nonsecure(void) {
  uint32_t val = GPIOA_S->SECCFGR;
  val &= ~(1 << 9);
  GPIOA_S->SECCFGR = val;
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
  SysTick_Config(4000);
  TZ_SysTick_Config_NS(4000);

  ENABLE_IRQ();

  configure_red_led();
  make_red_led_nonsecure();
}

#define NSC __attribute__((cmse_nonsecure_entry))

int NSC add10(int a) {
  return 10+a;
}