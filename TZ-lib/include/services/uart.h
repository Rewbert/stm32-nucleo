#ifndef SERVICES_UART_H
#define SERVICES_UART_H

#include "hal/platform/domain.h"

/* The transmit and receive pin of the LPUART1 peripheral. */
extern gpio_t lpuart1_tx;
extern gpio_t lpuart1_rx;

/**
 * @brief Configure and enable the LPUART1 peripheral, such that reading and writing
 * works. Can be configured to belong to the secure or non-secure world.
 * 
 */
void enable_lpuart1(uint32_t brr, security_domain_t domain);

#endif // SERVICES_UART_H