#include <iostream>
#include <iterator>
#include <vector>

using std::begin;
using std::cout;
using std::end;
using std::endl;
using std::ostream_iterator;
using std::vector;

int main(int argc, const char *argv[])
{
    int __vi[] = {1,2,3,4,55};
    vector<int> vi(std::begin(__vi), std::end(__vi));

    copy(begin(vi), end(vi), ostream_iterator<int>(cout, " "));
    cout << endl;

    int arr[sizeof(__vi) / sizeof(__vi[0])];
    copy(begin(vi), end(vi), std::begin(arr));

    copy(begin(arr), end(arr), ostream_iterator<int>(cout, " "));
    cout << endl;

    return 0;
}
