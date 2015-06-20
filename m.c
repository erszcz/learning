#include <stdlib.h>
#include <stdio.h>

struct asd {
    int a;
    int s;
    int d;
};

int main(int argc, const char *argv[])
{
    struct asd * s = malloc(sizeof * s);
    printf("%ld", sizeof * s);
    return 0;
}
