// Implement a matrix rotation function.
// For input:
// 1 2 3 4       e a 5 1
// 5 6 7 8  ===\ f b 6 2
// a b c d  ===/ g c 7 3
// e f g h       h d 8 4
//
// How? I proposed:
// x x x x x
// 4 x x x x
// 3 6 p x x
// 2 5 x x x
// 1 x x x x
// 
// p - "already rotated"
//
// The simplest solution:
// 1 2 3 x x
// 4 5 6 x x
// x x p x x
// x x x x x
// x x x x x
//
// How to rotate the elements?
// a[N][N] - matrix
// row, col - indices
//
// tmp = a[row][col]
// a[row][col] = a[n-col-1][row]
// a[n-col-1][row] = a[n-row-1][n-col-1]
// a[n-row-1][n-col-1] = a[...][n-row-1]
// a[...][n-row-1] = tmp
