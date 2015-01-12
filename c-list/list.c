#include <assert.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>

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
    struct item* this = *root;
    while (this != NULL) {
        free(this);
        this = (*root)->next;
        *root = this;
    }
}

bool
is_cycle(struct item* list1) {
    struct item* list2 = list1;
    while (list1 != NULL && list2 != NULL && list2->next != NULL) {
        list1 = list1->next;
        list2 = list2->next->next;
        if (list1 == list2)
            return true;
    }
    return false;
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

int main(int argc, const char *argv[])
{
    struct item* list = NULL;

    print(list);

    insert(&list, 5);
    print(list);

    struct item* last = list;

    insert(&list, 3);
    print(list);

    insert(&list, 1);
    print(list);

    print_element(find(list, 3), "\n");

    assert(find(list, 7) == NULL);
    printf("list doesn't contain element 7: ");
    print(list);

    assert(!is_cycle(list));
    printf("list *is not* a cycle: ");
    print(list);

    last->next = list;
    assert(is_cycle(list));
    printf("list *is* a cycle: ");

    int i;
    struct item* p = list;
    for (i = 0; i < 7; i++) {
        print_element(p, " -> ");
        p = p->next;
    }
    print_element(p, " ...\n");

    delete(&list);
    assert(list == NULL);

    return 0;
}
