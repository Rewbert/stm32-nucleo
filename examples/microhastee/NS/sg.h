#ifndef SG_H
#define SG_H

#include <stdint.h>

struct BFILE;

/* output_capacity is the real allocated size of output_buf, so the secure side can
   bound its write instead of trusting the result to fit. On success *output_len is
   the number of valid bytes written to output_buf; on rejection (a bad pointer, or a
   result that would not fit in output_capacity) *output_len is set to -1 and nothing
   is written to output_buf. */
void sg(struct BFILE *input_bfile, uint8_t *output_buf, int output_capacity, int *output_len);

#endif