#include <iostream>

template<typename T>
struct Adder {

    Adder(T l, T r):
        _l(l), _r(r) {}

    T do_() {
        return _l + _r;
    }

private:
    T _l, _r;

};

int main () {
    auto ia = Adder<int>(3, 5);
    auto da = Adder<double>(3.3, 5.2);

    std::cout << ia.do_() << std::endl;
    std::cout << da.do_() << std::endl;

    return 0;
}
