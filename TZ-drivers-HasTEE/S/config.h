#ifndef CONFIG_SECURE_H
#define CONFIG_SECURE_H

/* Wrap the shared MHS config, then pull in vtable declarations
   so auto-generated gen2.c can call get/set_vtable_ptr(). */
#include "../shared/config.h"
#include "vtable.h"

#endif /* CONFIG_SECURE_H */
