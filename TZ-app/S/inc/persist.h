#ifndef PERSIST_H
#define PERSIST_H

#include <stdint.h>

void flash_secure_unlock(void);
void flash_secure_lock(void);
void flash_secure_clear_flags(void);
int flash_secure_erase_page(uint32_t bank, uint32_t page);
int flash_secure_program_dw(uint32_t addr, uint64_t data64);

#endif /* PERSIST_H */