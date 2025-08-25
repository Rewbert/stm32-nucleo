#include "stm32l5xx.h"

#include "config.h"
#include "persist.h"

#define ENABLE_IRQ()  __asm volatile ("cpsie i" : : : "memory")
#define DISABLE_IRQ() __asm volatile ("cpsid i" : : : "memory")

#define NSC        __attribute__((cmse_nonsecure_entry))
#define PERSISTENT __attribute__((section(".persist"), used, aligned(8))) volatile const

void lpuart1_write(char c);
void write_string(char *str);

PERSISTENT uint32_t def;
PERSISTENT uint32_t pin[4];

int isDefaultConfigured() {
  return def == 0xFFFFFFFF;
}

/* Verify whether a code is the correct one or not */
int verifyPin(uint32_t *candidate) {
  for(int i = 0; i < 4; i++) {
    if (!(pin[i] == candidate[i])) {
      return 0;
    }
  }
  return 1;
}

NSC int changePin(uint32_t *old, uint32_t *new) {
  if(verifyPin(old)) {
    DISABLE_IRQ();
    if(!flash_secure_erase_page(1, 125)) {
      return 0;
    }
    if(!flash_secure_program_dw((uint32_t)&pin[0], ((uint64_t)new[1] << 32 | new[0]))) {
      return 0;
    }
    if(!flash_secure_program_dw((uint32_t)&pin[2], ((uint64_t)new[3] << 32 | new[2]))) {
      return 0;
    }
    if(def) {
      if(!flash_secure_program_dw((uint32_t)&def, 0x00000000)) {
        return 0;
      }
    }
    ENABLE_IRQ();
    return 1;
  }
  return 0;
}

void lock() {
  TURN_ON_LED(A, 9);
  TURN_OFF_LED(C, 7);
}

void unlock() {
  TURN_ON_LED(C, 7);
  TURN_OFF_LED(A, 9);
}

NSC int lockDoor(uint32_t *code) {
  if(verifyPin(code)) {
    lock();
    return 1;
  }
  return 0;
}

NSC int unlockDoor(uint32_t *code) {
  if(verifyPin(code)) {
    unlock();
    return 1;
  }
  return 0;
}

void print_result(int code) {
  if(code == 0) {
    write_string("no\r\n");
  } else {
    write_string("ok\r\n");
  }
}

void exti5_handler(void) {
  if(EXTI_S->FPR1 & EXTI_FPR1_FPIF5) {
    EXTI_S->FPR1 |= EXTI_FPR1_FPIF5;
    write_string("falling\r\n");
  }
  if(EXTI_S->RPR1 & EXTI_RPR1_RPIF5) {
    EXTI_S->RPR1 |= EXTI_RPR1_RPIF5;
    write_string("rising\r\n");
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

void write_string(char *str) {
  while(*str != '\0') {
    lpuart1_write(*str++);
  }
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

  ENABLE_LPUART1();

  ENABLE_IRQ();

  CONFIGURE_NONSECURE_BUTTON(A, 2);
  CONFIGURE_NONSECURE_BUTTON(A, 3);
  CONFIGURE_NONSECURE_BUTTON(A, 5);
  CONFIGURE_NONSECURE_BUTTON(A, 6);
  CONFIGURE_NONSECURE_BUTTON(A, 7);
  CONFIGURE_NONSECURE_BUTTON(A, 8);
  CONFIGURE_NONSECURE_BUTTON(A, 10);
  CONFIGURE_SECURE_LED(A, 9);
  CONFIGURE_SECURE_LED(C, 7);

  write_string("Done\r\n");

//  flash_secure_erase_page(1, 125);
  if(def) {
    write_string("Default pin-code is used\r\n");
  }
  
  TURN_ON_LED(A, 9);
  TURN_OFF_LED(C, 7);

  // uint32_t first[4] = { 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF };
  // print_result(verifyPin(first));

  // uint32_t new[4] = {1, 2, 3, 4};
  // print_result(changePin(first, new));
  // print_result(verifyPin(new));

  // write_string("please enter a character> ");
  // char c = lpuart1_read();
  // lpuart1_write(c);
  // write_string("\r\nreceived character was> ");
  // lpuart1_write(c);
  // write_string("\r\n");

}

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

// secure FLASH 0x50022000, and SECSR offset is 0x24. Reset value 0x00000000
// SECCR offset is 0x2C, reset value 0x80000000
// 00000001000000000000001111101010