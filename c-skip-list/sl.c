#include <assert.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "slimtests.h"

#define PRINT_BUFSIZE 4096
#define MAXL 20

struct sl_node;

struct sl_head {
    struct sl_node* next[MAXL];
};

struct sl_node {
    int data;
    struct sl_node* next[MAXL];
};

// This doesn't really use size in a meaningful way.
// It doesn't check for buffer overflow either, it's shamefully broken!
int sl_snprint(char* str, size_t size, struct sl_head sl) {
    int l;
    char* p = str;
    struct sl_node* n;
    size_t left = size, written;
    for (l = 0; l < MAXL; l++) {
        n = sl.next[l];
        while (n) {
            written = snprintf(p, left, "%d%s",
                               n->data, n->next[l] ? " " : "\n");
            left -= written;
            assert(written < size);
            assert(left >= 0);
            n = n->next[l];
            p += written;
        }
    }
}

int sl_fprint(FILE* f, struct sl_head sl) {
    char buf[PRINT_BUFSIZE+1];
    sl_snprint(buf, PRINT_BUFSIZE, sl);
    buf[PRINT_BUFSIZE] = 0;
    fprintf(f, "%s", buf);
}

int sl_scan(struct sl_head* sl) {
    int i;
    for (i = 0; i < MAXL; i++) {
        sl->next[i] = NULL;
    }
    return 0;
}

void sl_node_init(struct sl_node* n, int data) {
    memset(n, 0, sizeof(*n));
    n->data = data;
}

#define size(arr) (sizeof(arr) / sizeof(arr[0]))

bool sanity_check() { return true; }

bool test_print() {
    char* example = "1 2 3\n"
                    "1 3\n";
    struct sl_node n1, n2, n3;
    sl_node_init(&n1, 1);
    sl_node_init(&n2, 2);
    sl_node_init(&n3, 3);
    n1.next[0] = &n2;
    n2.next[0] = &n3;
    n1.next[1] = &n3;
    struct sl_head sl;
    memset(&sl, 0, sizeof(sl));
    sl.next[0] = &n1;
    sl.next[1] = &n1;
    char buf[PRINT_BUFSIZE+1];
    buf[PRINT_BUFSIZE] = 0;
    sl_snprint(buf, PRINT_BUFSIZE, sl);
    //fprintf(stderr, "%s", example);
    //fprintf(stderr, "%s", buf);
    return strncmp(example, buf, strlen(example)) == 0;
}

test_spec tests[] = {
    TEST_SPEC(sanity_check),
    TEST_SPEC(test_print)
};

int main(int argc, const char *argv[])
{
    return run_tests(tests, size(tests));
}
