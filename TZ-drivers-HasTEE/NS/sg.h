#ifndef SG_H
#define SG_H

#include <stdint.h>

struct BFILE;
void sg(struct BFILE *input_bfile, uint8_t *output_buf, int *output_len);

#endif