#include <stdio.h>
#include <regex.h>

int main(int argc, const char *argv[])
{
    printf("sizeof(regex_t) = %ld\n", sizeof(regex_t));
    return 0;
}
