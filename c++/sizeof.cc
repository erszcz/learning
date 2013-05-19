#include <cassert>
#include <iostream>
#include <iterator>
#include <vector>

using std::cout;
using std::endl;
using std::vector;

int main(int argc, const char *argv[]) {
    vector<int> vi{1,2,3};
    cout << sizeof(vector<int>::value_type) << endl;
    return 0;
}
