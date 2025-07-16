#ifndef UART_H
#define UART_H

void enable_lpuart1(void);

void lpuart1_write(char c);

char lpuart1_read(void);

#endif /* UART_H */