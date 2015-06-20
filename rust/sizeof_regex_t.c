#include <regex.h>
#include <stdio.h>
#include <sys/types.h>

int main(int argc, const char *argv[])
{
    printf("sizeof(regex_t): %ld\n", sizeof(regex_t));
    printf("REG_EXTENDED = %d\n", REG_EXTENDED);
    return 0;
}
