#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

/* Let's assume a machine with 2MB of allocatable memory.
 * Page size of the machine is 2KB.
 * Write mem_alloc()/mem_dealloc() pair of functions which will allocate
 * or deallocate such 2KB blocks and manage them. */

typedef struct {
    /* Offset into a page is 11 bits wide. */
    unsigned    ptr     : 11;
    /* We need to index 1024 pages in the table; that requires 10 bits. */
    unsigned    idx     : 10;
    /* We need to know whether a page is free or not. */
    bool        used    : 1;
} pt_entry_t;

/* Constants. */
enum {
    PAGE_SIZE   = 2048,
    /* 2MB */
    MEMORY_SIZE = 2 * 1024 * 1024,
    /* 2MB is addressable by 1024 entries. */
    PT_ENTRIES  = MEMORY_SIZE / PAGE_SIZE,
    MEM_ALLOC_NOMEM = -1
};

union {
    pt_entry_t  page_table[PT_ENTRIES];
    char        _[MEMORY_SIZE];
} memory;

void
page_table_init() {
    size_t i;
    for (i = 0; i < sizeof(memory.page_table) / PAGE_SIZE; i++) {
        pt_entry_t entry = {
            .ptr    = (unsigned long)(&memory.page_table + i*PAGE_SIZE) & 0x7ff,
            .idx    = i,
            .used   = true
        };
        memory.page_table[i] = entry;
    }
}

void
page_table_print_usage() {
    size_t i;
    const unsigned line = 80;

    printf("page table usage:\n");

    for (i = 0; i < PT_ENTRIES; i++) {
        if (!(i % line)) {
            printf("\n  ");
        }
        printf("%c", memory.page_table[i].used ? 'x' : '.');
    }
    printf("\n\n");

    for (i = 0; i < PT_ENTRIES; i++) {
        if (!memory.page_table[i].used) {
            continue;
        }
        printf("  pt_entry_t: used=%c idx=%d addr=0x%x\n",
               memory.page_table[i].used ? '1' : '0',
               memory.page_table[i].idx,
               memory.page_table[i].ptr);
    }
    printf("\n");
}

typedef void* ptr_t;

ptr_t
mem_alloc() {
    size_t i;
    for (i = 0; i < PT_ENTRIES; i++) {
        if (!memory.page_table[i].used) {
            return (ptr_t)(memory.page_table[i].idx | memory.page_table[i].ptr);
        }
    }
    return (void*)MEM_ALLOC_NOMEM;
}

void
mem_dealloc(ptr_t ptr) {
    ;
}

void*
mem_deref(ptr_t ptr) {
}

int main(int argc, const char *argv[])
{
    printf("pt_entry_t size: %lu\n", sizeof(pt_entry_t));
    printf("page size:       %u\n", PAGE_SIZE);
    printf("page_table size: %lu\n", sizeof(memory.page_table));
    printf("memory size:     %lu\n", sizeof(memory));

    page_table_init();
    page_table_print_usage();

    return 0;
}
