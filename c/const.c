#include <stdio.h>
#include <stdlib.h>

int main(int argc, const char *argv[])
{
    int z = 13;

    // Given this declaration:
    int* const a = &z;
    // this is OK
    *a = 5;
    // but this is not
    /*a = &z;*/
    // That is, the pointer is const, not the value.

    // This time the value is const, not the pointer.
    int const* b = &z;
    /**b = z;*/
    b = &z;

    // In general, the thing to the left of `const` keyword is const:
    // int* const a; // The *pointer* to int is const, the int/value is mutable.
    // int const* a; // The int is const, the pointer is mutable.

    printf("%d\n", z);
    
    return 0;
}
