#include <stdio.h>

/* How to turn some macro on only in portions of code using #define
 * and #undef, but abide to the One Definition Rule and have it
 * defined only once?
 *
 * That's how!!1oneone */

#define DEFINE_S 5
#define DEFINE_T(arg) (arg+3)


int main(int argc, const char *argv[])
{
#define S DEFINE_S
#define T(arg) DEFINE_T(arg)
    printf("%d\n", S);
    printf("%d\n", T(2));
#undef S
#undef T
    /* Uncomment the below lines and compilation will fail. */
    /*printf("%d\n", S);*/
    /*printf("%d\n", T(2));*/
    return 0;
}
