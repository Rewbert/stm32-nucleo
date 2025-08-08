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

void configure_button() {
  // button is PC13, but my own button is PC10
  // make sure the GPIOA port is powered on through RCC->AHB1ENR

  GPIOA_S->MODER &= ~(GPIO_MODER_MODE5_Msk); // input mode
  GPIOA_S->PUPDR &= ~(GPIO_PUPDR_PUPD5_Msk);     // We configure the pin to be pull-up, meaning that it is HIGH when we don't press the button
  GPIOA_S->PUPDR |= (1 << GPIO_PUPDR_PUPD5_Pos);

  EXTI_S->EXTICR[1] &= ~(EXTI_EXTICR2_EXTI5_Msk);
  EXTI_S->EXTICR[1] |= (0x0 << EXTI_EXTICR2_EXTI5_Pos); // Set EXTI5 to map to PA5

  EXTI_S->IMR1 |= (1 << EXTI_IMR1_IM5_Pos); // Enable interrupts on the EXTI5 line

  EXTI_S->FTSR1 |= (1 << EXTI_FTSR1_FT5_Pos); // Since our button is HIGH, and turns LOW when we push it, we configure the EXTI5 to trigger on a falling edge
                                              // the alternative is to give the same treatment to RTSR1, which enables triggers on rising edges. We can also
                                              // configure both to generate events on both events.

  // EXTI_S->SECCFGR1 to configure the interrupt as secure

  NVIC_SetPriority(EXTI5_IRQn, 2);
  NVIC_EnableIRQ(EXTI5_IRQn);
}

volatile uint32_t mask = 0x20;
void exti5_handler(void) {
  if(EXTI_S->FPR1 & mask) { // (1 << EXTI_FPR1_FPIF5_Msk)) {
    EXTI_S->FPR1 |= EXTI_FPR1_FPIF5; // EXTI_FPR1_FPIF5_Msk);
  }
  //while(1) {}
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
  configure_button();
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