#include <stdint.h>

struct rust_object_s {
    int32_t a;
};
typedef struct rust_object_s rust_object_t;

void callback(rust_object_t* rust_object, int32_t a) {
    rust_object->a = a;
}
