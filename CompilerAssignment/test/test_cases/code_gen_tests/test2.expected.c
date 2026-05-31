#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>
#include "extern_types.h"

void Test_move_point(Custom_Struct_Point* p, int32_t dx, int32_t dy) {
    int32_t *xptr, *yptr, *copy_ptr;
    bool *vptr;
    Custom_Struct_Point **nptr;
    int32_t x, y, newx, newy, local_copy;
    bool ok, is_root;
    Custom_Struct_Point *nullp;
    goto bb0;

bb0:
    nullp = NULL;
    is_root = p != nullp;
    xptr = &(p->x);
    yptr = &(p->y);
    vptr = &(p->valid);
    nptr = &(p->next);
    x = *xptr;
    y = *yptr;
    newx = x + dx;
    newy = y + dy;
    *xptr = newx;
    *yptr = newy;
    ok = true;
    *vptr = ok;
    local_copy = newx;
    copy_ptr = &local_copy;
    *copy_ptr = newy;
    if (is_root) goto bb1; else goto bb2;

bb1:
    *nptr = nullp;
    return;

bb2:
    return;
}