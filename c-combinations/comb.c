#include <math.h>
#include <stdio.h>

#include <assert.h>

#define K 2
#define N 4

int
is_over_diagonal(int * p, int k) {
    int i;
    for (i = 1; i < k; i++) {
        if (p[i] >= p[0])
            return 0;
    }
    return 1;
}

void
test_is_over_diagonal() {
    int k2d = 2;

    // x o o
    // o o o
    // o o o
    int p1[] = {0,0};
    assert(! is_over_diagonal(p1, k2d));

    // o x o
    // o o o
    // o o o
    int p2[] = {1,0};
    assert(is_over_diagonal(p2, k2d));

    // o o o
    // x o o
    // o o o
    int p3[] = {0,1};
    assert(! is_over_diagonal(p3, k2d));

    // o o o
    // o o x
    // o o o
    int p4[] = {2,1};
    assert(is_over_diagonal(p4, k2d));

    // o o o
    // o o o
    // o o x
    int p5[] = {2,2};
    assert(! is_over_diagonal(p5, k2d));

    int k3d = 3;

    // x o o   o o o   o o o
    // o o o   o o o   o o o
    // o o o   o o o   o o o
    int p6[] = {0,0,0};
    assert(! is_over_diagonal(p6, k3d));

    // o o o   o o o   o o o
    // o o o   o x o   o o o
    // o o o   o o o   o o o
    int p7[] = {1,1,1};
    assert(! is_over_diagonal(p7, k3d));

    // o o o   o o o   o o o
    // o o o   o o o   o o o
    // o o o   o o o   o o x
    int p8[] = {2,2,2};
    assert(! is_over_diagonal(p8, k3d));

    // o o o   o o o   o o o
    // o o o   o o o   o o o
    // o o o   o o o   o o x
    int p9[] = {0,0,2};
    assert(is_over_diagonal(p9, k3d));
}

//int
//main(int argc, const char *argv[]) {
//    int cnt = 0;
//    int comb[K];
//    int i;
//    for (i = 0; i < pow(N,K); i++) {
//        int p[K];
//        for (j = 0; j < K; j++) {
//            p[j] = j == 0 ? i % N : i / (int) pow(N,j);
//            if (!is_over_diagonal(p, K))
//                continue;
//        }
//        [> code <]
//    }
//    printf("cnt = %d\n", cnt);
    
//    return 0;
//}

int main(int argc, const char *argv[])
{
    test_is_over_diagonal();

    return 0;
}
