#include <iostream>

using std::cout;
using std::endl;

struct my_struct {
    int a;
    double b;
};

my_struct my_fun(int a, double b, my_struct& ms) {
    auto ms2 = ms;
    ms2.a = a;
    ms2.b = b;
    return ms2;
}

typedef decltype(my_fun) my_fun_type;

auto my_fun2() -> my_fun_type* {
    return my_fun;
}

int main(int argc, const char *argv[])
{
    
    return 0;
}
