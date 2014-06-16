#include <stdint.h>
#include <stdio.h>

struct rust_object_s {
    int32_t a;
};
typedef struct rust_object_s rust_object_t;

extern void callback(rust_object_t* rust_object, int32_t a);

int main()
{
    rust_object_t rust_object = {.a = 5};
    printf("Value is now %d\n", rust_object.a);
    callback(&rust_object, 7);
    printf("Value is now %d\n", rust_object.a);
    return 0;
}
