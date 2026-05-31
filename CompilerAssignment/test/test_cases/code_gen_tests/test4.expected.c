#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>
#include "extern_types.h"

void Test_minimal_empty_void(void) {
    goto bb0;

bb0:
    return;
}
