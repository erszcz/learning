#include <stdio.h>

/* The macro below uses the typeof operator and a statement expression.
 * Both are GCC extensions. */

#define min(a, b) \
    ({ typeof(a) _a = (a); \
       typeof(b) _b = (b); \
       _a < _b ? _a : _b; })

int main(int argc, const char *argv[])
{
    printf("min(4, 3)     = %d\n", min(4, 3));
    printf("min(2.3, 4.7) = %f\n", min(2.3, 4.7));

    return 0;
}
