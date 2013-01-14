#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

struct trie {
    int end;
    struct trie * next[128];
};

void
trie_insert(struct trie * node, const char * key);

struct trie *
trie_find(struct trie * node, const char * key);

typedef void (*trie_callback) (struct trie * node, const char * key,
                               void * cbarg);

void
trie_iter(struct trie * node, trie_callback cb, void * cbarg);

int
main(int argc, const char *argv[])
{
    int i;
    struct trie index;
    memset(&index, 0, sizeof(struct trie));

    trie_insert(&index, "a");
    assert(trie_find(&index, "a"));

    trie_insert(&index, "abc");
    assert(trie_find(&index, "abc"));

    trie_insert(&index, "zxc");
    assert(trie_find(&index, "zxc"));

    // insert a looong string
    char v[250];
    memset(v, 0, 250);
    for (i = 0; i < 250-1; i++) {
        v[i] = 'v';
    }
    trie_insert(&index, v);
    assert(trie_find(&index, v));

    // iterate over the trie
    char buf[500];
    memset(buf, 0, 500);
    auto void cb(struct trie * node, const char * key, void * vp) {
        sprintf((char*) vp + strlen((char *) vp), "%s\n", key);
    }
    trie_iter(&index, cb, (void*) buf);

    char exemplar[500];
    snprintf(exemplar, 500, "a\nabc\n%s\nzxc\n", v);
    //printf("%s", exemplar);
    //printf("----\n");
    //printf("%s", buf);
    assert(strcmp(exemplar, buf) == 0);
    
    return 0;
}

void
trie_insert(struct trie * node, const char * key) {
    assert(node);
    if (! key[0]) {
        node->end = 1;
        return;
    }
    if (! node->next[key[0]]) {
        node->next[key[0]] = calloc(1, sizeof(struct trie));
        assert(node->next[key[0]]);
    }
    trie_insert(node->next[key[0]], key+1);
}

struct trie *
trie_find(struct trie * node, const char * key) {
    if (! node)
        return NULL;
    if (key[0] == '\0' && node->end)
        return node;
    return trie_find(node->next[key[0]], key+1);
}

static void
trie_iter_rec(struct trie * node, trie_callback cb, void * cbarg,
              char ** key, size_t l, size_t n)
{
    if (node->end) {
        cb(node, (const char *) *key, cbarg);
    }
    if (l >= n) {
        n = 2*n;
        *key = realloc(*key, n);
        assert(*key);
    }
    int i = 0;
    for (i = 0; i < 128; i++) {
        if (node->next[i]) {
            (*key)[l] = (char) i;
            trie_iter_rec(node->next[i], cb, cbarg, key, l+1, n);
            (*key)[l+1] = '\0';
        }
    }
}

void
trie_iter(struct trie * node, trie_callback cb, void * cbarg) {
    size_t keysize;
    char * key;

    if (! node)
        return;

    keysize = 100;
    key = calloc(1, keysize);
    assert(key);

    trie_iter_rec(node, cb, cbarg, &key, 0, keysize);

    if (key)
        free(key);
}
