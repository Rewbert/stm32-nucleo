#include "stm32l5xx.h"
#include "button.h"

button_callback_t b1_callback;

void add_b1_callback(button_callback_t b1_user_callback) {
    b1_callback = b1_user_callback;
}

// this handler will go directly into the vector table in the position for EXTI13 IRQ
void exti13_handler(void) {
  // Did we fire an interrupt on a rising edge event?
  if(EXTI_NS->RPR1 & EXTI_RPR1_RPIF13) {
    // clear the rising edge event if so
    EXTI_NS->RPR1 |= EXTI_RPR1_RPIF13;
    if(b1_callback != NULL) b1_callback();
  }
  if(EXTI_NS->FPR1 & EXTI_FPR1_FPIF13) {
    EXTI_NS->FPR1 |= EXTI_FPR1_FPIF13;
    if(b1_callback) b1_callback();
  }
}