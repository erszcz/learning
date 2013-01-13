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

typedef void (*trie_callback) (struct trie * node);

//void
//trie_iter(struct trie * node);

int
main(int argc, const char *argv[])
{
    struct trie index;
    memset(&index, 0, sizeof(struct trie));

    trie_insert(&index, "a");
    assert(trie_find(&index, "a"));

    trie_insert(&index, "abc");
    assert(trie_find(&index, "abc"));

    trie_insert(&index, "zxc");
    assert(trie_find(&index, "zxc"));

    //char buf[100];
    //auto trie_callback cb;
    //void cb(struct trie * node) {

    //}
    //assert(strcmp("3\n5\n", ) == 0);
    
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
