/* Copyright 2026 Robert Krook, see LICENSE file for the full license. */

#ifndef UDB_DRV_H
#define UDB_DRV_H

#include "udb.h"

/*
 * Fill in a udb_driver_t for the current board and security domain.
 *
 * There is one implementation per MCU (stm32l5-drv.c, stm32u5-drv.c); the
 * Makefile compiles the one matching $(BOARD). Each is compiled twice (secure
 * and non-secure); -DSECURE selects the flash register block and the flash
 * alias so that each world's database lives in its own flash.
 */
void udb_drv_create(udb_driver_t *drv);

#endif /* UDB_DRV_H */
