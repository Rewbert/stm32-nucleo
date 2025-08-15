#include "stm32l5xx.h"

#include "config.h"

#define ENABLE_IRQ()  __asm volatile ("cpsie i" : : : "memory")
#define DISABLE_IRQ() __asm volatile ("cpsid i" : : : "memory")

void lpuart1_write(char c);

void exti5_handler(void) {
  if(EXTI_S->FPR1 & EXTI_FPR1_FPIF5) {
    EXTI_S->FPR1 |= EXTI_FPR1_FPIF5;
    lpuart1_write('f');
    lpuart1_write('a');
    lpuart1_write('l');
    lpuart1_write('l');
    lpuart1_write('i');
    lpuart1_write('n');
    lpuart1_write('g');
    lpuart1_write('\r');
    lpuart1_write('\n');
  }
  if(EXTI_S->RPR1 & EXTI_RPR1_RPIF5) {
    EXTI_S->RPR1 |= EXTI_RPR1_RPIF5;
    lpuart1_write('r');
    lpuart1_write('i');
    lpuart1_write('s');
    lpuart1_write('i');
    lpuart1_write('n');
    lpuart1_write('g');
    lpuart1_write('\r');
    lpuart1_write('\n');
  }
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

void lpuart1_write(char c) {
  // Wait until the transmit data register is empty
  while (!(LPUART1_S->ISR & USART_ISR_TXE)); // Check TXE flag
  LPUART1_S->TDR = c; // Write character to transmit
  while (!(LPUART1_S->ISR & USART_ISR_TC)); // Wait for transmission to complete
}

char lpuart1_read(void) {
  // Wait until the receive data register is not empty
  while (!(LPUART1_S->ISR & USART_ISR_RXNE));
  return (char)(LPUART1_S->RDR & 0xFF); // Read received character
}

void secure_app_initialise() {
  CONFIGURE_CLOCK_110_MHZ();
  SysTick_Config(110000);
  TZ_SysTick_Config_NS(110000);

  ENABLE_SECURE_LPUART1();

  ENABLE_IRQ();

  CONFIGURE_SECURE_BUTTON(A, 5);
  CONFIGURE_NONSECURE_LED(A, 9);

  lpuart1_write('D');
  lpuart1_write('o');
  lpuart1_write('n');
  lpuart1_write('e');
  lpuart1_write('\r');
  lpuart1_write('\n');

}

#define NSC __attribute__((cmse_nonsecure_entry))

void NSC secure_lpuart1_write(char c) {
  lpuart1_write(c);
}

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