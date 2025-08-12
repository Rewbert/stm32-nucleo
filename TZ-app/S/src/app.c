#include "stm32l5xx.h"

#include "config.h"

#define ENABLE_IRQ()  __asm volatile ("cpsie i" : : : "memory")
#define DISABLE_IRQ() __asm volatile ("cpsid i" : : : "memory")

void exti5_handler(void) {
  if(EXTI_S->FPR1 & EXTI_FPR1_FPIF5)
    EXTI_S->FPR1 |= EXTI_FPR1_FPIF5;
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
  CONFIGURE_CLOCK_110_MHZ();
  SysTick_Config(110000);
  TZ_SysTick_Config_NS(110000);

  CONFIGURE_NONSECURE_BUTTON(A, 5);
  CONFIGURE_NONSECURE_LED(A, 9);

  ENABLE_IRQ();

  // while(1) {
  //   delay_ms(500);
  //   TOGGLE_LED(A, 9);
  // }
}

#define NSC __attribute__((cmse_nonsecure_entry))

int NSC add10(int a) {
  return 10+a;
}

// secure GPIOA 0x52020000 <---- my button is here, PA.05
// secure GPIOB 0x52020400
// secure GPIOC 0x52020800
// secure GPIOD 0x52020C00
// secure GPIOE 0x52021000
// secure GPIOF 0x52021400
// secure GPIOG 0x52021800
// secure GPIOH 0x52021C00

// input register for GPIOx is offset 0x10

// secure PWR 0x50007000
// secure SYSCFG 0x5001000
// secure EXTI 0x5002F400, FPR1 is offset by 0x10