#include <cassert>
#include <iostream>

using std::cerr;
using std::endl;

void
somefun(int* a) {
    assert(a != NULL);
#ifndef NDEBUG
    cerr << __FILE__ << ":" << __LINE__ << " " <<__func__ << " called" << endl;
#endif
}

int main(int argc, const char *argv[]) {
    int a;
    somefun(&a);
    somefun(NULL);
    return 0;
}
