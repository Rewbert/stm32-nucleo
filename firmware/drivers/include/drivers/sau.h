#ifndef DRIVERS_SAU_H
#define DRIVERS_SAU_H

#include <stdint.h>

typedef enum {
    SAU_NS = 0,
    SAU_S_NSC,
} sau_attr_t;

  // SAU: declarative table of regions, passed all at once
typedef struct {
    uint32_t   start;
    uint32_t   end;
    sau_attr_t attr;    // SAU_NS or SAU_S_NSC
} sau_region_t;

#define SAU_REGION(name, start, end, attr) { (start), (end), (attr) }

void sau_configure(const sau_region_t *regions, uint32_t count);

#endif