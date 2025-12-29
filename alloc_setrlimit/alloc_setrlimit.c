#include <stdlib.h>
#include <stdio.h>
#include <strings.h>

#include <sys/time.h>
#include <sys/resource.h>

#define BUFSIZE_MB      200
#define BUFSIZE_BYTES   (BUFSIZE_MB * 1024 * 1024)

int main() {
    struct rlimit limits = {
        BUFSIZE_BYTES - 1,
        BUFSIZE_BYTES - 1
    };

    //if (!setrlimit(RLIMIT_AS, &limits)) {
    //    printf("set resource limit to {%llu, %llu}\n", limits.rlim_cur, limits.rlim_max);
    //} else {
    //    printf("could not set resource limit to {%llu %llu}\n", limits.rlim_cur, limits.rlim_max);
    //    return 1;
    //}

    char* buf = malloc(BUFSIZE_BYTES);
    if (buf) {
        printf("allocated %d megabytes of memory\n", BUFSIZE_MB);
        // Zero the buffer to actually make it used from `gtime` perspective
        bzero(buf, BUFSIZE_BYTES);
    } else {
        printf("could not allocate %d megabytes of memory\n", BUFSIZE_MB);
    }
    return 0;
}

// vim: sw=4 ts=4 sts=4 et
