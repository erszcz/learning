#include <assert.h>
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
    if (item == NULL)
        return NULL;
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

    insert(&list, 3);
    print(list);

    insert(&list, 1);
    print(list);

    print_element(find(list, 3), "\n");

    delete(&list);
    assert(list == NULL);

    return 0;
}
