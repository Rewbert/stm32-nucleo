#ifndef SERVICES_UART_H
#define SERVICES_UART_H

#include "hal/platform/domain.h"

/**
 * @brief Configure and enable the LPUART1 peripheral, such that reading and writing
 * works. Can be configured to belong to the secure or non-secure world.
 * 
 */
void enable_lpuart1(uint16_t brr, security_domain_t domain);

#endif // SERVICES_UART_H