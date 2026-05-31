#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>
#include "extern_types.h"

int32_t Test_int_bool_control(int32_t a, int32_t b, bool flag) {
    int32_t zero, one, sum, diff, prod, quot, rem, neg, chosen;
    bool eq, ne, lt, le, gt, ge, notflag, both, or1, or2, or3, cmp_any, flag_mix, final_cond;
    goto bb0;

bb0:
    zero = 0;
    one = 1;
    sum = a + b;
    diff = a - b;
    prod = sum * diff;
    quot = prod / one;
    rem = quot % one;
    neg = -diff;
    eq = a == b;
    ne = a != b;
    lt = a < b;
    le = a <= b;
    gt = a > b;
    ge = a >= b;
    notflag = !flag;
    both = flag && notflag;
    or1 = eq || ne;
    or2 = lt || le;
    or3 = gt || ge;
    cmp_any = or2 || or3;
    flag_mix = both || notflag;
    final_cond = or1 || cmp_any;
    if (final_cond) goto bb1; else goto bb2;

bb1:
    chosen = neg;
    goto bb3;

bb2:
    chosen = rem;
    goto bb3;

bb3:
    return chosen;
}