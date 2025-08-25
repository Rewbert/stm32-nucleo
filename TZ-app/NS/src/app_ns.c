#include "stm32l5xx.h"

#include "config.h"

#define ENABLE_IRQ()  __asm volatile ("cpsie i" : : : "memory")
#define DISABLE_IRQ() __asm volatile ("cpsid i" : : : "memory")

int myadd();

volatile uint32_t ticks;
void systick_handler() {
  ticks++;
}

void lpuart1_write(char c);
void write_string(char *str);

void delay_ms(uint32_t milliseconds) {
  uint32_t start = ticks;
  uint32_t end = start + milliseconds;

  if (end < start) {
      while (ticks > start);
  }
  while(ticks < end);
}

int key = 0;
int keyready = 0;

enum KEY { ONE=1,
           TWO,
           THREE,
           FOUR,
           CHANGE_PIN,
           LOCK,
           UNLOCK
           };

void exti2_handler(void) {
  if(EXTI_NS->FPR1 & EXTI_FPR1_FPIF2) {
     EXTI_NS->FPR1 |= EXTI_FPR1_FPIF2;
     key = CHANGE_PIN;
     keyready = 1;
  }
  if(EXTI_NS->RPR1 & EXTI_RPR1_RPIF2) {
    EXTI_NS->RPR1 |= EXTI_RPR1_RPIF2;
  }
}

void exti3_handler(void) {
  if(EXTI_NS->FPR1 & EXTI_FPR1_FPIF3) {
     EXTI_NS->FPR1 |= EXTI_FPR1_FPIF3;
     key = LOCK;
     keyready = 1;
  }
  if(EXTI_NS->RPR1 & EXTI_RPR1_RPIF3) {
    EXTI_NS->RPR1 |= EXTI_RPR1_RPIF3;
  }
}

void exti5_handler(void) {
  if(EXTI_NS->FPR1 & EXTI_FPR1_FPIF5) {
     EXTI_NS->FPR1 |= EXTI_FPR1_FPIF5;
     key = ONE;
     keyready = 1;
  }
  if(EXTI_NS->RPR1 & EXTI_RPR1_RPIF5) {
    EXTI_NS->RPR1 |= EXTI_RPR1_RPIF5;
  }
}

void exti6_handler(void) {
  if(EXTI_NS->FPR1 & EXTI_FPR1_FPIF6) {
     EXTI_NS->FPR1 |= EXTI_FPR1_FPIF6;
     key = TWO;
     keyready = 1;
  }
  if(EXTI_NS->RPR1 & EXTI_RPR1_RPIF6) {
    EXTI_NS->RPR1 |= EXTI_RPR1_RPIF6;
  }
}

void exti7_handler(void) {
  if(EXTI_NS->FPR1 & EXTI_FPR1_FPIF7) {
     EXTI_NS->FPR1 |= EXTI_FPR1_FPIF7;
     key = THREE;
     keyready = 1;
  }
  if(EXTI_NS->RPR1 & EXTI_RPR1_RPIF7) {
    EXTI_NS->RPR1 |= EXTI_RPR1_RPIF7;
  }
}

void exti8_handler(void) {
  if(EXTI_NS->FPR1 & EXTI_FPR1_FPIF8) {
     EXTI_NS->FPR1 |= EXTI_FPR1_FPIF8;
     key = FOUR;
     keyready = 1;
  }
  if(EXTI_NS->RPR1 & EXTI_RPR1_RPIF8) {
    EXTI_NS->RPR1 |= EXTI_RPR1_RPIF8;
  }
}

void exti10_handler(void) {
  if(EXTI_NS->FPR1 & EXTI_FPR1_FPIF10) {
     EXTI_NS->FPR1 |= EXTI_FPR1_FPIF10;
     key = UNLOCK;
     keyready = 1;
  }
  if(EXTI_NS->RPR1 & EXTI_RPR1_RPIF10) {
    EXTI_NS->RPR1 |= EXTI_RPR1_RPIF10;
  }
}

uint32_t read_key(uint32_t timeout) {
  uint32_t end = ticks + timeout;
  while(!keyready) {
    if(ticks > end) return -1;
  }
  keyready = 0;
  return key;
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
extern int changePin(uint32_t *old, uint32_t *new);
extern int unlockDoor(uint32_t *code);
extern int lockDoor(uint32_t *code);

char lpuart1_read(void) {
  // Wait until the receive data register is not empty
  while (!(LPUART1_NS->ISR & USART_ISR_RXNE));
  return (char)(LPUART1_NS->RDR & 0xFF); // Read received character
}

void write_string(char *str) {
  while(*str != '\0') {
    lpuart1_write(*str++);
  }
}

int readPin(uint32_t *pin) {
  write_string("> ");

  uint32_t read[4];
  for(int i = 0; i < 4; i++) {
    int key = read_key(10000); // 10 seconds
    if(key == -1) {
      return 0;
    }
    lpuart1_write(0x30 + key);
    read[i] = key;
  }

  write_string("\r\n");

  for(int i = 0; i < 4; i++) {
    pin[i] = read[i];
  }

  return 1;
}

void change_pin() {
  uint32_t pin[4];
  readPin(pin);

  uint32_t new[4];
  readPin(new);

  if(changePin(pin, new)) {
    write_string("Pin changed\r\n");
  } else {
    write_string("Wrong pin entered\r\n");
  }
}

void unlock_door() {
  uint32_t pin[4];
  readPin(pin);

  if(unlockDoor(pin)) {
    write_string("Door unlocked\r\n");
  } else {
    write_string("Wrong pin entered\r\n");
  }
}

void lock_door() {
  uint32_t pin[4];
  readPin(pin);

  if(lockDoor(pin)) {
    write_string("Door locked\r\n");
  } else{
    write_string("Wrong pin entered\r\n");
  }
}

void app() {
  while(1) {
    write_string("Please select a command\r\n");
    uint32_t command = read_key(0x0FFFFFFF);
    switch(command) {
      case CHANGE_PIN: write_string("To change the code, please enter the current code followed by the new code\r\n"); change_pin();  break;
      case LOCK:       write_string("To lock the door, please enter the code\r\n");                                    lock_door();   break;
      case UNLOCK:     write_string("To unlock the door, please enter the code\r\n");                                  unlock_door(); break;
      default: continue; 
    }
  }
}

void main() {
  CONFIGURE_NONSECURE_BUTTON(A, 2);
  CONFIGURE_NONSECURE_BUTTON(A, 3);
  CONFIGURE_NONSECURE_BUTTON(A, 5);
  CONFIGURE_NONSECURE_BUTTON(A, 6);
  CONFIGURE_NONSECURE_BUTTON(A, 7);
  CONFIGURE_NONSECURE_BUTTON(A, 8);
  CONFIGURE_NONSECURE_BUTTON(A, 10);

  ENABLE_IRQ();

  while(1) app();
}

// GPIOA 0x42020000
// RCC   0x40021000
// EXTI  0x4002F400