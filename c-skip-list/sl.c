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
        while (n != NULL) {
            written = snprintf(p, left, "%d%s",
                               n->data,
                               n->next[l] ? " " : "\n");
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

void sl_head_init(struct sl_head* sl) {
    memset(sl, 0, sizeof(*sl));
}

void sl_node_init(struct sl_node* n, int data) {
    memset(n, 0, sizeof(*n));
    n->data = data;
}

int sl_read(const char* from, size_t size, struct sl_head* sl) {
    char * save_line, * save_token;
    char * str, * l, * t;
    int i;
    int k;
    struct sl_node* n;
    struct sl_node** here;

    // Copy input string - we'll have to cut it in pieces.
    str = calloc(size, 1);
    // So lame!
    assert(str != NULL);
    strlcpy(str, from, size);

    // Find first EOL.
    i = 0;
    l = strtok_r(str, "\n", &save_line);
    // Lameness strikes again!
    assert(l != NULL);

    // Split first line into tokens;
    // for each token allocate a node, then link them into a list.
    t = strtok_r(l, " ", &save_token);
    here = &sl->next[i];
    while (t != NULL) {
        n = calloc(1, sizeof(*n));
        // Lameness again.
        assert(n != NULL);
        sl_node_init(n, atoi(t));
        *here = n;
        here = &n->next[i];
        t = strtok_r(NULL, " ", &save_token);
    }

    // For each line after the first one iterate over the tokens
    // and link only the nodes listed on the line.
    i = 1;
    l = strtok_r(NULL, "\n", &save_line);
    while (l != NULL) {
        n = sl->next[0];
        here = &sl->next[i];
        t = strtok_r(l, " ", &save_token);
        while (t != NULL) {
            k = atoi(t);
            // Instead of allocating, as done on the first line,
            // find n in the already linked list.
            while (n != NULL && n->data != k)
                n = n->next[0];
            // Assert we didn't reach the end of the list without finding
            // a node which already stores k.
            assert(n != NULL);
            *here = n;
            here = &n->next[i];
            t = strtok_r(NULL, " ", &save_token);
        }
        l = strtok_r(NULL, "\n", &save_line);
        i += 1;
    }
    return 0;
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
    return strncmp(example, buf, PRINT_BUFSIZE) == 0;
}

bool test_read() {
    char* example = "1 2 3\n"
                    "1 3\n";
    struct sl_head sl;
    sl_head_init(&sl);
    sl_read(example, strlen(example) + 1, &sl);
    //sl_fprint(stderr, sl);
    return (sl.next[0] && sl.next[0]->data == 1 &&
            sl.next[0]->next[0] && sl.next[0]->next[0]->data == 2 &&
            sl.next[0]->next[0]->next[0] &&
            sl.next[0]->next[0]->next[0]->data == 3 &&
            sl.next[0]->next[0]->next[0]->next[0] == NULL &&
            sl.next[1] && sl.next[1]->data == 1 &&
            sl.next[1]->next[1] && sl.next[1]->next[1]->data == 3 &&
            sl.next[1]->next[1]->next[1] == NULL &&
            sl.next[2] == NULL);
}

bool identity() {
    char* example[] = {
        "1\n",
        "1 2 3\n",
        "1 2 3\n1 3\n",
        "1 2 3 4 5 6 7\n1 3 4 6 7\n1 4 6\n1 4\n"
    };
    int i;
    bool success = true;
    char buf[PRINT_BUFSIZE+1];
    for (i = 0; i < size(example); i++) {
        memset(buf, 0, PRINT_BUFSIZE+1);
        struct sl_head sl;
        sl_head_init(&sl);
        sl_read(example[i], strlen(example[i]) + 1, &sl);
        sl_snprint(buf, PRINT_BUFSIZE, sl);
        //fprintf(stderr, "%s", example[i]);
        //fprintf(stderr, "%s", buf);
        success = success && (strncmp(example[i], buf, PRINT_BUFSIZE) == 0);
    }
    return success;
}

test_spec tests[] = {
    TEST_SPEC(sanity_check),
    TEST_SPEC(test_print),
    TEST_SPEC(test_read),
    TEST_SPEC(identity)
};

int main(int argc, const char *argv[])
{
    return run_tests(tests, size(tests));
}
