#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>
#include "extern_types.h"

int64_t Test_calls_casts_numeric(void) {
    int32_t i, fromf, fromsum, call0;
    int64_t wide, fromu, fromf64, fromsum64, call1, tmp, ret;
    uint32_t u;
    double f, negf, sumf, halff;
    bool ok;
    goto bb0;

bb0:
    i = -7;
    wide = (int64_t)i;
    u = 42;
    fromu = (int64_t)u;
    f = 3.5;
    negf = -f;
    sumf = f + negf;
    halff = f / f;
    fromf = (int32_t)halff;
    fromsum = (int32_t)sumf;
    fromf64 = (int64_t)fromf;
    fromsum64 = (int64_t)fromsum;
    call0 = Runtime_zero_i32();
    call1 = Runtime_combine_i64(wide, fromu);
    tmp = call1 + fromf64;
    ok = tmp >= fromsum64;
    if (ok) goto bb1; else goto bb2;

bb1:
    ret = tmp + fromu;
    return ret;

bb2:
    ret = (int64_t)call0;
    return ret;
}
