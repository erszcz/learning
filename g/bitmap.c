struct node {
    color c[4];
    node  c[4];
};

struct node *
deep_copy(struct node *);

struct node *
merge(struct node * a, struct node * b) {
    int i;
    struct node * n = malloc(sizeof * n);
    for (i = 0; i < 4; i++) {
        n->c[i] = a->c[i] | b->c[i];
        if (a->n[i] && b->n[i]) {
            n->n[i] = merge(a->n[i], b->n[i]);
            if (is_solid(n->n[i])) {
                free(n->n[i]);
                n->n[i] = NULL;
                n->c[i] = 1;
            }
        }
        if (a->n[i] || b->n[i]) {
            if (a->n[i]) {
            } else if (a->n[i])
        }
    }
}

// cases:
// 1 | 0 = 1
// 1 | 1 = 1
// 0 | 0 = 0
// 1 | M = 1
// 0 | M = M
