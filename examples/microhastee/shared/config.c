#include "domain/cmsis_select.h"

int ffs(int x) {
    if (!x)
        return 0;
    x &= -x;           /* keep lowest bit */
    int i = __CLZ(x);  /* count leading 0s */
    return 32 - i;     /* 31 leading zeros should return 1 */
}