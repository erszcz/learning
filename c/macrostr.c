#include <stdio.h>

// Use a macro as LHS in a variable definition.
// In other words, "syntactically simulate a function call" on LHS.

#define intvar(n) int var##n

int main(int argc, const char *argv[])
{
    intvar(3) = 5;
    printf("%d\n", var3);
    return 0;
}
