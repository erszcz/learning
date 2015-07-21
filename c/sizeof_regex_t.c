#include <regex.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>

int main(int argc, const char *argv[])
{
    regex_t reg;
    printf("sizeof regex_t: %lu\n", sizeof(regex_t));

    return 0;
}
