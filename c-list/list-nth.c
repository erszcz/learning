#include <assert.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>

#include "slimtests.h"

struct item {
    struct item*    next;
    int             data;
};

void
insert(struct item** root, int data) {
    struct item* new = calloc(1, sizeof(*new));
    new->data = data;
    if (*root == NULL) {
        *root = new;
        return;
    }
    new->next = *root;
    *root = new;
}

struct item*
find(struct item* item, int data) {
    while (item != NULL && item->data != data)
        item = item->next;
    return item;
}

void
delete(struct item** root) {
    if (*root == NULL)
        return;
    struct item* next = (*root)->next;
    while (*root != NULL) {
        free(*root);
        *root = next;
        if (next != NULL)
            next = next->next;
    }
}

void print_element(struct item*, char const*);

void
print(struct item* list) {
    print_element(list, " -> ");
    if (list != NULL) {
        print(list->next);
    }
}

void
print_element(struct item* list, char const* suffix) {
    if (list == NULL) {
        printf("NULL\n");
        return;
    }
    printf("%d @ %p%s", list->data, list, suffix);
}

struct item*
nth(size_t n, struct item* list) {
    while (n > 0 && list != NULL) {
        list = list->next;
        --n;
    }
    return list;
}

//! Return nth element from the end of the list.
struct item*
last_nth(size_t n, struct item* list) {
    struct item* p = list;
    struct item* nth = NULL;
    while (n > 0 && p != NULL) {
        p = p->next;
        --n;
    }
    if (p == NULL)
        return NULL;
    nth = list;
    while (p->next != NULL) {
        p = p->next;
        nth = nth->next;
    }
    return nth;
}

bool
test_nth_1st() {
    // given
    struct item* list = NULL;
    insert(&list, 3);
    insert(&list, 2);
    insert(&list, 1);
    // when
    int data = nth(0, list)->data;
    // then
    return data == 1;
}

bool
test_nth_2nd() {
    // given
    struct item* list = NULL;
    insert(&list, 3);
    insert(&list, 2);
    insert(&list, 1);
    // when
    int data = nth(1, list)->data;
    // then
    return data == 2;
}

bool
test_nth_4th() {
    // given
    struct item* list = NULL;
    insert(&list, 3);
    insert(&list, 2);
    insert(&list, 1);
    // when
    struct item* item = nth(3, list);
    // then
    return item == NULL;
}

bool
test_last_nth_1st() {
    // given
    struct item* list = NULL;
    insert(&list, 3);
    insert(&list, 2);
    insert(&list, 1);
    // when
    int data = last_nth(0, list)->data;
    // then
    return data == 3;
}

bool
test_last_nth_2nd() {
    // given
    struct item* list = NULL;
    insert(&list, 3);
    insert(&list, 2);
    insert(&list, 1);
    // when
    int data = last_nth(1, list)->data;
    // then
    return data == 2;
}

bool
test_last_nth_4th() {
    // given
    struct item* list = NULL;
    insert(&list, 3);
    insert(&list, 2);
    insert(&list, 1);
    // when
    struct item* item = last_nth(3, list);
    // then
    return item == NULL;
}

test_spec tests[] = {
    TEST_SPEC(test_nth_1st),
    TEST_SPEC(test_nth_2nd),
    TEST_SPEC(test_nth_4th),
    TEST_SPEC(test_last_nth_1st),
    TEST_SPEC(test_last_nth_2nd),
    TEST_SPEC(test_last_nth_4th),
};

#define size(arr) (sizeof(arr) / sizeof(arr[0]))

int main(int argc, const char *argv[])
{
    return run_tests(tests, size(tests));
}
