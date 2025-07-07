#include <stdint.h>
#include "stm32l5xx.h"

#include "uart.h"

void enable_lpuart1(void) {
   volatile uint32_t dummy;

    // I forget what this does, it enables something
    RCC->CCIPR1 &= ~(3 << RCC_CCIPR1_LPUART1SEL_Pos);
    RCC->CCIPR1 |= (1 << RCC_CCIPR1_LPUART1SEL_Pos);

    // turn on the LPUART
    RCC->APB1ENR2 |= (1 << RCC_APB1ENR2_LPUART1EN_Pos);
    dummy = RCC->APB1ENR2; // do some dummy instructions to make some time pass
    dummy = RCC->APB1ENR2;

    RCC->APB1ENR1 |= RCC_APB1ENR1_PWREN; // this turns on VDDIO2, the power to the GPIOG part of the board
    PWR->CR2 |= PWR_CR2_IOSV; // without this, LPUART1 does not work at all

    // turn on GPIOG, since this is where we will find the G-pins
    RCC->AHB2ENR |= 0x000000ff; // RCC_AHB2ENR_GPIOGEN_Msk; // watch *(int*) 0x4002104c
    dummy = RCC->AHB2ENR;
    dummy = RCC->AHB2ENR;

    GPIOG->MODER &= ~(GPIO_MODER_MODE7_Msk | GPIO_MODER_MODE8_Msk); // reset the values
    GPIOG->MODER |= (0b10 << GPIO_MODER_MODE7_Pos) | (0b10 << GPIO_MODER_MODE8_Pos);  // write the code for alternate mode (0x2)

    GPIOG->PUPDR &= ~(0b11 << GPIO_PUPDR_PUPD7_Pos);  // Clear previous setting
    GPIOG->PUPDR |=  (0b01 << GPIO_PUPDR_PUPD7_Pos);   // Enable pull-up on PG7
    GPIOG->PUPDR &= ~(0b11 << GPIO_PUPDR_PUPD8_Pos); // Clear previous setting
    GPIOG->PUPDR |= (0b01 << GPIO_PUPDR_PUPD8_Pos); // Enable pull-up on PG8

    // the datasheet sets these to alternate function 8. I am not sure what this means, but guides online says that it is important
    GPIOG->AFR[0] &= ~GPIO_AFRL_AFSEL7;
    GPIOG->AFR[1] &= ~GPIO_AFRH_AFSEL8;

    GPIOG->AFR[0] |= (8 << GPIO_AFRL_AFSEL7_Pos);
    GPIOG->AFR[1] |= (8 << GPIO_AFRH_AFSEL8_Pos);

    LPUART1->BRR = 244444; // this is BRR calculation for Baudrate of 115200
    dummy = RCC->AHB2ENR; // not sure if I need these, but too scared to remove them
    dummy = RCC->AHB2ENR;

    LPUART1->CR1 |= USART_CR1_UE | USART_CR1_TE | USART_CR1_RE; // enable everything
}

// chatgpt helped with this function
void lpuart1_write(char c) {
    // Wait until the transmit data register is empty
    while (!(LPUART1->ISR & USART_ISR_TXE)); // Check TXE flag
    LPUART1->TDR = c; // Write character to transmit
    while (!(LPUART1->ISR & USART_ISR_TC)); // Wait for transmission to complete
}
