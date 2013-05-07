#include <iostream>
#include <string>

using std::cout;
using std::endl;
using std::string;

template <typename T>
class adder {
public:
    T add(T term1, T term2);
};

template <typename T>
T adder<T>::add(T term1, T term2) {
    return term1 + term2;
}

template <typename T, T constant>
T add_constant(T i) {
    return constant + i;
}

int main(int argc, const char *argv[]) {
    adder<int> int_adder;
    adder<double> double_adder;

    cout << "int: " << int_adder.add(3, 5) << endl;
    cout << "double: " << double_adder.add(3.6, 5.3) << endl;

    cout << "add_constant<int, 3>(3) = "
         << add_constant<int, 3>(3) << endl;
    // won't compile!
    // double and std::basic_string<char> are not allowed
    // as a non-type template parameter
    //cout << "add_constant<double, 3.14>(3) = "
    //     << add_constant<double, 3.14>(3.0) << endl;
    //cout << "add_constant<string, \"ala\">(\"ma kota\")"
    //     << add_constant<string, "ala">("ma kota");

    return 0;
}
