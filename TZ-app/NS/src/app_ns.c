#include "stm32l5xx.h"

#include "config.h"

#define ENABLE_IRQ()  __asm volatile ("cpsie i" : : : "memory")
#define DISABLE_IRQ() __asm volatile ("cpsid i" : : : "memory")

int myadd();

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

void exti5_handler(void) {
  if(EXTI_NS->FPR1 & EXTI_FPR1_FPIF5) {
     EXTI_NS->FPR1 |= EXTI_FPR1_FPIF5;
//     TOGGLE_LED(A, 9);
//     GPIOA_NS->ODR ^= (1 << 9);
     // user code
  }
}

void lpuart1_write(char c) {
  // we don't proceed if the LPUART1 is marked as secure. We are allowed from the non secure application to
  // observe whether it is secured or not, but not to alter the actual configuration
  if(!(RCC_NS->APB1SECSR2 & RCC_APB1SECSR2_LPUART1SECF_Msk)) {
    // Wait until the transmit data register is empty
    while (!(LPUART1_NS->ISR & USART_ISR_TXE)); // Check TXE flag
    LPUART1_NS->TDR = c; // Write character to transmit
    while (!(LPUART1_NS->ISR & USART_ISR_TC)); // Wait for transmission to complete
  }
}

extern void secure_lpuart1_write(char);

char lpuart1_read(void) {
  // Wait until the receive data register is not empty
  while (!(LPUART1_NS->ISR & USART_ISR_RXNE));
  return (char)(LPUART1_NS->RDR & 0xFF); // Read received character
}

void main() {
  CONFIGURE_NONSECURE_BUTTON(A, 5);

  ENABLE_IRQ();

  while(1) {
    TOGGLE_LED(A, 9);
    delay_ms(500);
    // lpuart1_write('N');
    // lpuart1_write('S');
    // lpuart1_write('\r');
    // lpuart1_write('\n');
    // secure_lpuart1_write('F');
    // secure_lpuart1_write('r');
    // secure_lpuart1_write('o');
    // secure_lpuart1_write('m');
    // secure_lpuart1_write(' ');
    // secure_lpuart1_write('N');
    // secure_lpuart1_write('S');
    // secure_lpuart1_write('C');
    // secure_lpuart1_write('\r');
    // secure_lpuart1_write('\n');
  }
}

// GPIOA 0x42020000
// RCC   0x40021000
// EXTI  0x4002F400