#include <iostream>

using namespace std;

template <typename T>
struct Example
{
    typedef T Type;
};

int main(int argc, const char *argv[])
{
    cout << "sizeof(Example<char>::Type) = "
         << sizeof(Example<char>::Type) << endl
         << "sizeof(Example<double>::Type) = "
         << sizeof(Example<double>::Type) << endl;
    
    return 0;
}
