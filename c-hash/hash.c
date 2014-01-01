#include <stdio.h>

typedef int (*hash_t) (void * b, size_t n);

int
hash1(void * b, size_t n) {
    int h = 0x1337ac3;
    int i;
    char * p = (char *) b;
    for (i = 0; i < n; i++) {
        h ^= p[i];
    }
    return h;
}

int
hash2(void * b, size_t n) {
    int h = 0x1337ac3;
    int i;
    char * p = (char *) b;
    for (i = 0; i < n; i++) {
        h ^= h * p[i];
    }
    return h;
}

int main(int argc, const char *argv[]) {

    int a = 45;
    int b = 54;
    char c[] = "45";
    char d[] = "54";

    hash_t hash = hash2;

    printf("%d\n", hash(&a, sizeof(int)));
    printf("%d\n", hash(&b, sizeof(int)));
    printf("%d\n", hash(&c, 2));
    printf("%d\n", hash(&d, 2));
    return 0;
}
