#ifndef UART_H
#define UART_H

/* Turn on the whole LPUART1 machinery */
void enable_lpuart1(void);

/* Write a character out over LPUART */
void lpuart1_write(char c);

/* Read a character over LPUART (blocks until one is read) */
char lpuart1_read(void);

#endif /* UART_H */