// Problem: machine has only 64kb of mem.
// Write two functions:
//
//   void free(void*);
//   void* malloc();
//
// malloc() always allocates 2b blocks.

// I suggested a linear index of blocks using a bitmap.
// Then I also suggested a version with a binary-tree index implemented using
// a linear array.
