#ifndef DOMAIN_H
#define DOMAIN_H

#ifdef SECURE
#define HAL_SECURE 1
#else
#define HAL_SECURE 0
#endif

typedef enum {
    DOMAIN_SECURE=0,
    DOMAIN_NONSECURE,
} security_domain_t;

#endif // DOMAIN_H