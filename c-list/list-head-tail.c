#include <assert.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>

#include "slimtests.h"

struct item {
    struct item*    next;
    int             data;
};

struct item* head;
struct item* tail;

void
delete_list(struct item** root) {
    if (*root == NULL)
        return;
    struct item* this = *root;
    while (this != NULL) {
        free(this);
        this = (*root)->next;
        *root = this;
    }
}

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

void
insert_after(struct item* item, int data) {
    assert(false); // not implemented yet
}

void
delete(struct item* item) {
    if (head == NULL || tail == NULL) {
        // empty list: if one of head/tail is NULL
        // then the other one must be NULL too
        assert(head == NULL && tail == NULL);
        return;
    }
    if (item == NULL)
        return;
    // one element long list
    if (head == item && tail == item) {
        head = NULL;
        tail = NULL;
        free(item);
        return;
    }
    // at least 2 element long list; removing head
    if (head == item) {
        head = head->next;
        free(item);
        return;
    }
    // at least 2 element long list; removing sth later than head
    struct item* this = head;
    while (this->next != item)
        this = this->next;
    // this->next is now item or NULL (because item is not NULL)
    if (this->next == item) {
        this->next = item->next;
        // are we removing the last element?
        if (tail == item)
            tail = this;
        free(item);
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

void
cleanup() {
    //print(head);
    if (head != NULL) {
        delete_list(&head);
        tail = NULL;
    }
}

bool sanity_check() { return true; }

bool
delete_one_element_long_list() {
    // given
    struct item* list = calloc(1, sizeof(*list));
    list->data = 123;
    //print(list);
    // when
    delete_list(&list);
    // then
    return list == NULL;
}

bool
delete_from_empty_list() {
    cleanup();
    // given
    head = NULL;
    tail = NULL;
    struct item i = {
        .next = NULL,
        .data = 123
    };
    struct item i_copy = i;
    // when
    delete(&i);
    // then
    return (head == NULL &&
            tail == NULL &&
            !memcmp(&i, &i_copy, sizeof(i)));
}

bool
delete_a_null_element() {
    cleanup();
    // given
    head = calloc(1, sizeof(*head));
    head->data = 1;
    tail = calloc(1, sizeof(*tail));
    tail->data = 2;
    head->next = tail;
    // when
    delete(NULL);
    // then
    return (head->data == 1 &&
            tail->data == 2 &&
            head->next == tail);
}

bool
delete_head_from_two_element_list() {
    cleanup();
    // given
    head = calloc(1, sizeof(*head));
    head->data = 1;
    tail = calloc(1, sizeof(*tail));
    tail->data = 2;
    head->next = tail;
    // when
    delete(head);
    // then
    return (head == tail &&
            head->next == NULL &&
            head->data == 2);
}

bool
delete_tail_from_two_element_list() {
    //cleanup();
    // given
    head = calloc(1, sizeof(*head));
    head->data = 3;
    tail = calloc(1, sizeof(*tail));
    tail->data = 4;
    head->next = tail;
    //print(head);
    // when
    delete(tail);
    // then
    print(head);
    return (head == tail &&
            head->next == NULL &&
            head->data == 3);
}

bool
cleanup_after_tests() {
    cleanup();
    return true;
}

test_spec tests[] = {
    // tools
    TEST_SPEC(sanity_check),
    TEST_SPEC(delete_one_element_long_list),
    // exercise
    TEST_SPEC(delete_from_empty_list),
    TEST_SPEC(delete_a_null_element),
    TEST_SPEC(delete_head_from_two_element_list),
    TEST_SPEC(delete_tail_from_two_element_list),
    // cleanup
    TEST_SPEC(cleanup_after_tests)
};

#define size(arr) (sizeof(arr) / sizeof(arr[0]))

int main(int argc, const char *argv[]) {
    return run_tests(tests, size(tests));
}
