#include <regex.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>

#define MAXLINE 4096

int main(int argc, const char *argv[])
{
    regex_t reg;
    char line[MAXLINE];

    if (argc < 2) {
        errx(1, "not enough arguments");
    }

    regcomp(&reg, argv[1], REG_EXTENDED);
    memset(line, 0, MAXLINE);

    while (!feof(stdin)) {
        if (NULL != fgets(line, MAXLINE, stdin))
        if (REG_NOMATCH != regexec(&reg, line, 0, NULL, 0)) {
            printf("%s", line);
        }
    }

    regfree(&reg);
    return 0;
}
