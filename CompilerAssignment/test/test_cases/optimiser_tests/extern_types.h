//for test 1
typedef struct Custom_Struct_Point {
    int32_t x, y;
    bool valid;
    struct Custom_Struct_Point* next;
} Custom_Struct_Point;

//for test 3
int32_t Runtime_zero_i32(void) {
    return 0;
}

int64_t Runtime_combine_i64(int64_t a, int64_t b){
    return a + b;
}
