#ifndef DOMAIN_H
#define DOMAIN_H

#ifdef SECURE
#define HAL_SECURE 1
#else
#define HAL_SECURE 0
#endif

// Mark your functions as NSC in order to make them callable from the non-secure world
#define NONSECURE_CALLABLE __attribute__((cmse_nonsecure_entry))

typedef enum {
    DOMAIN_SECURE=0,
    DOMAIN_NONSECURE,
} security_domain_t;

#endif // DOMAIN_H