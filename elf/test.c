#include <stdio.h>

int add(int a, int b) {
    printf("Numbers are added together\n");
    return a+b;
}

int main(int argc, const char *argv[])
{
    int a, b;
    a = 3;
    b = 4;
    int ret = add(a,b);
    printf("Result: %u\n", ret);
    exit(0);
}
