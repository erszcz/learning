#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void mergesort(char* a, int l, int r);

#ifdef TEST

#include <assert.h>

int cmpint(const void* p, const void* q) { return *(int*)p - *(int*)q; }
int cmpchar(const void* p, const void* q) { return *(char*)p - *(char*)q; }

#define DEFTEST(n, ...) \
    void test##n() \
    { \
        char a[] = {__VA_ARGS__}; \
        char b[sizeof(a) / sizeof(a[0])]; \
        memcpy(b, a, sizeof(a)); \
        mergesort(a, 0, sizeof(a) / sizeof(a[0]) - 1); \
        qsort(b, sizeof(a) / sizeof(a[0]), sizeof(a[0]), cmpchar); \
        for (int i = 0; i < sizeof(a) / sizeof(a[0]); ++i) { \
            printf("%d ", a[i]); \
            assert(a[i] == b[i]); \
        } \
        printf("ok\n"); \
    }

DEFTEST(1, /* data */ 5,4,3,2,1)
DEFTEST(2, /* data */ 5,5,5,5,5)
DEFTEST(3, /* data */ 1,2,3,4,5)
DEFTEST(4, /* data */ -1,2,-3,4,-5)

int main(int argc, const char *argv[])
{
    test1();
    test2();
    test3();
    test4();
    return 0;
}

#else

int main(int argc, const char *argv[])
{
    if (argc != 2) {
        fprintf(stderr, "usage: %s file-to-sort\n", argv[0]);
        exit(1);
    }

    FILE* f = fopen(argv[1], "r");
    char  b[1024];
    memset(b, 0, 1024);
    char* p = b;
    size_t r;
    while ((r = fread(p, 1, b+1024-p, f)) > 0)
        p += r;
    if (ferror(f)) {
        fprintf(stderr, "error reading file: %s\n", argv[1]);
        goto fail;
    }
    fclose(f);

    mergesort(b, 0, p-b-1);

    for (int i = 0; i < p-b; ++i) {
        printf("%d ", b[i]);
    }
    printf("\n");

    exit(0);

fail:
    fclose(f);
    exit(1);
}

#endif /* TEST */

void merge(char* a, int l0, int l1, int r0, int r1)
{
    if (l0 >= r1)
        return;

    int size = r1 - l0 + 1;
    char* b = (char*) malloc(size * sizeof(char));
    int p = l0, q = r0;
    int i;
    for (i = 0; p <= l1 && q <= r1; i++)
        b[i] = a[p] < a[q] ? a[p++] : a[q++];
    while(p <= l1) b[i++] = a[p++];
    while(q <= r1) b[i++] = a[q++];

    memcpy(&a[l0], b, size * sizeof(char));
    if (b)
        free(b);
}

void mergesort(char* a, int l, int r)
{
    if (l >= r)
        return;

    mergesort(a, l, (l+r)/2);
    mergesort(a, (l+r)/2+1, r);
    merge(a, l, (l+r)/2, (l+r)/2+1, r);
}
