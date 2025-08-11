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

#define MAKE_GPIO_NONSECURE(port, pin) GPIO##port##_S->SECCFGR &= ~(1 << pin)

#define CONFIGURE_BUTTON_0(port) CONFIGURE_BUTTON_CR(1, port, 0)
#define CONFIGURE_BUTTON_1(port) CONFIGURE_BUTTON_CR(1, port, 1)
#define CONFIGURE_BUTTON_2(port) CONFIGURE_BUTTON_CR(1, port, 2)
#define CONFIGURE_BUTTON_3(port) CONFIGURE_BUTTON_CR(1, port, 3)
#define CONFIGURE_BUTTON_4(port) CONFIGURE_BUTTON_CR(2, port, 4)
#define CONFIGURE_BUTTON_5(port) CONFIGURE_BUTTON_CR(2, port, 5)
#define CONFIGURE_BUTTON_6(port) CONFIGURE_BUTTON_CR(2, port, 6)
#define CONFIGURE_BUTTON_7(port) CONFIGURE_BUTTON_CR(2, port, 7)
#define CONFIGURE_BUTTON_8(port) CONFIGURE_BUTTON_CR(3, port, 8)
#define CONFIGURE_BUTTON_9(port) CONFIGURE_BUTTON_CR(3, port, 9)
#define CONFIGURE_BUTTON_10(port) CONFIGURE_BUTTON_CR(3, port, 10)
#define CONFIGURE_BUTTON_11(port) CONFIGURE_BUTTON_CR(3, port, 11)
#define CONFIGURE_BUTTON_12(port) CONFIGURE_BUTTON_CR(4, port, 12)
#define CONFIGURE_BUTTON_13(port) CONFIGURE_BUTTON_CR(4, port, 13)
#define CONFIGURE_BUTTON_14(port) CONFIGURE_BUTTON_CR(4, port, 14)
#define CONFIGURE_BUTTON_15(port) CONFIGURE_BUTTON_CR(4, port, 15)

#define CONFIGURE_BUTTON(port, pin) CONFIGURE_BUTTON_##pin(port)

#define CONFIGURE_BUTTON_CR(cr, port, pin)                              \
  GPIO##port##_S->MODER &= ~(GPIO_MODER_MODE##pin##_Msk);               \
  GPIO##port##_S->PUPDR &= ~(GPIO_PUPDR_PUPD##pin##_Msk);               \
  GPIO##port##_S->PUPDR |= (1 << GPIO_PUPDR_PUPD##pin##_Pos);           \
  EXTI_S->EXTICR[cr - 1] &= ~(EXTI_EXTICR##cr##_EXTI##pin##_Msk);       \
  EXTI_S->EXTICR[cr - 1] |= (0x0 << EXTI_EXTICR##cr##_EXTI##pin##_Pos); \
  EXTI_S->IMR1 |= (1 << EXTI_IMR1_IM##pin##_Pos);                       \
  EXTI_S->FTSR1 |= (1 << EXTI_FTSR1_FT##pin##_Pos);                     \
  EXTI_S->SECCFGR1 |= (1 << EXTI_SECCFGR1_SEC##pin##_Pos);              \
  EXTI_S->PRIVCFGR1 |= (1 << EXTI_PRIVCFGR1_PRIV##pin##_Pos);           \
  NVIC_SetPriority(EXTI##pin##_IRQn, 2);                                \
  NVIC_EnableIRQ(EXTI##pin##_IRQn);

  // #define CONFIGURE_BUTTON(port, pin) \
  // GPIO##port##_S->MODER &= ~(GPIO_MODER_MODE##pin##_Msk); \
  // GPIO##port##_S->PUPDR &= ~(GPIO_PUPDR_PUPD##pin##_Msk); \
  // GPIO##port##_S->PUPDR |= (1 << GPIO_PUPDR_PUPD##pin##_Pos); \
  // EXTI_S->EXTICR[1] &= ~(EXTI_EXTICR2_EXTI##pin##_Msk); \
  // EXTI_S->EXTICR[1] |= (0x0 << EXTI_EXTICR2_EXTI##pin##_Pos); \
  // EXTI_S->IMR1 |= (1 << EXTI_IMR1_IM##pin##_Pos); \
  // EXTI_S->FTSR1 |= (1 << EXTI_FTSR1_FT##pin##_Pos); \
  // EXTI_S->SECCFGR1 |= (1 << EXTI_SECCFGR1_SEC##pin##_Pos); \
  // NVIC_SetPriority(EXTI##pin##_IRQn, 2); \
  // NVIC_EnableIRQ(EXTI##pin##_IRQn);

// void configure_button() {
//   // button is PC13, but my own button is PC10
//   // make sure the GPIOA port is powered on through RCC->AHB1ENR

//   GPIOA_S->MODER &= ~(GPIO_MODER_MODE5_Msk); // input mode
//   GPIOA_S->PUPDR &= ~(GPIO_PUPDR_PUPD5_Msk);     // We configure the pin to be pull-up, meaning that it is HIGH when we don't press the button
//   GPIOA_S->PUPDR |= (1 << GPIO_PUPDR_PUPD5_Pos);

//   EXTI_S->EXTICR[1] &= ~(EXTI_EXTICR2_EXTI5_Msk);
//   EXTI_S->EXTICR[1] |= (0x0 << EXTI_EXTICR2_EXTI5_Pos); // Set EXTI5 to map to PA5

//   EXTI_S->IMR1 |= (1 << EXTI_IMR1_IM5_Pos); // Enable interrupts on the EXTI5 line

//   EXTI_S->FTSR1 |= (1 << EXTI_FTSR1_FT5_Pos); // Since our button is HIGH, and turns LOW when we push it, we configure the EXTI5 to trigger on a falling edge
//                                               // the alternative is to give the same treatment to RTSR1, which enables triggers on rising edges. We can also
//                                               // configure both to generate events on both events.

//   // EXTI_S->SECCFGR1 to configure the interrupt as secure

//   NVIC_SetPriority(EXTI5_IRQn, 2);
//   NVIC_EnableIRQ(EXTI5_IRQn);
// }

void exti2_handler(void) {
  if(EXTI_S->FPR1 & EXTI_FPR1_FPIF2)
    EXTI_S->FPR1 |= EXTI_FPR1_FPIF2;
}

void exti3_handler(void) {
  if(EXTI_S->FPR1 & EXTI_FPR1_FPIF3)
    EXTI_S->FPR1 |= EXTI_FPR1_FPIF3;
}

void exti5_handler(void) {
  if(EXTI_S->FPR1 & EXTI_FPR1_FPIF5)
    EXTI_S->FPR1 |= EXTI_FPR1_FPIF5;
}

void exti6_handler(void) {
  if(EXTI_S->FPR1 & EXTI_FPR1_FPIF6)
    EXTI_S->FPR1 |= EXTI_FPR1_FPIF6;
}

void exti7_handler(void) {
  if(EXTI_S->FPR1 & EXTI_FPR1_FPIF7)
    EXTI_S->FPR1 |= EXTI_FPR1_FPIF7;
}

void exti8_handler(void) {
  if(EXTI_S->FPR1 & EXTI_FPR1_FPIF8)
    EXTI_S->FPR1 |= EXTI_FPR1_FPIF8;
}

void exti10_handler(void) {
  if(EXTI_S->FPR1 & EXTI_FPR1_FPIF10)
    EXTI_S->FPR1 |= EXTI_FPR1_FPIF10;
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
  MAKE_GPIO_NONSECURE(A, 9);

 MAKE_GPIO_NONSECURE(A, 5);
 NVIC_SetTargetState(EXTI5_IRQn); // mark this EXTI line as non secure

//  CONFIGURE_BUTTON(A, 5);

//  configure_button();
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

/*

MODER: 0xabc4030f
            10  9 8  7 6  5 4  3 2  1 0
xxxx xxxx xx00 xx00 0000 00xx 0000 1111
 OK   OK   OK   OK   OK   OK   OK   OK

PUPDR: 0x64115450
            10  9 8  7 6  5 4  3 2  1 0
xxxx xxxx xx01 xx01 0101 01xx 0101 xxxx
 OK   OK   OK   OK   OK   OK   OK   OK

IDR: 0x0000a7ec
                         11
                          10
                           98 7654 3210
xxxx xxxx xxxx xxxx xxxx x1x1 111x 11xx
 OK   OK   OK   OK   OK   OK   OK   OK

IMR1: 0xff9e05ec
xxxx xxxx xxxx xxxx xxxx x1x1 111x 11xx
 OK   OK   OK   OK   OK   OK   OK   OK

FTSR1: 0x000005ec
xxxx xxxx xxxx xxxx xxxx x1x1 111x 11xx
 OK   OK   OK   OK   OK   OK   OK   OK

EXTICR1: 0x00000000
0000 0000 0000 0000 0000 0000 0000 0000

EXTICR2: 0x00000000
0000 0000 0000 0000 0000 0000 0000 0000

EXTICR3: 0x00000000
0000 0000 0000 0000 0000 0000 0000 0000

EXTICR4: 0x00000000
0000 0000 0000 0000 0000 0000 0000 0000
*/