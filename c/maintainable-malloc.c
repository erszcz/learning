#include <stdio.h>
#include <stdlib.h>

typedef struct {
    int a;
    char b;
    long c;
} somestruct;

int main(int argc, const char *argv[])
{
    size_t length;
    somestruct* s;

    scanf("%ld", &length);

    // Using this form of malloc, the type of the array element has
    // to be changed only once - in the array declaration.
    s = malloc(sizeof(*s) * length);

    return 0;
}
