#include <iostream>
#include <iterator>
#include <vector>

#include <boost/range/algorithm.hpp>
#include <boost/range/irange.hpp>

using boost::copy;
using boost::irange;
using namespace std;

int main(int argc, const char *argv[])
{
    vector<int> v;
    copy(irange(1,11,2), back_inserter(v));
    for (int i = 0; i < v.size(); i++) {
        cout << v[i] << " ";
    }
    cout << endl;
    
    return 0;
}
