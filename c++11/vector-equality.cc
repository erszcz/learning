#include <iostream>
#include <vector>

using std::cout;
using std::endl;
using std::vector;

int main(int argc, const char *argv[])
{
    vector<int> a{1,2,3};
    vector<int> b{1,2,3};
    vector<int> c{1,2};
    cout << (a == b) << endl;
    cout << (a == c) << endl;
    return 0;
}
