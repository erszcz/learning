#include <boost/range/algorithm.hpp>
#include <boost/range/irange.hpp>
#include <boost/thread.hpp>
#include <iostream>
#include <iterator>
#include <vector>

using boost::copy;
using boost::irange;
using std::vector;

void print(std::vector<int>& v) {
    copy(begin(v), end(v), std::ostream_iterator<int>(std::cout, " "));
    std::cout << std::endl;
}

void modify(std::vector<int>& v) {
    transform(begin(v), end(v), begin(v), [](int i){ return i + 1000; });
}

int main(int argc, const char *argv[])
{
    std::vector<int> vi;
    copy(irange(1001,1016), back_inserter(vi));

    // This is an example of INVALID concurrent access to a vector.
    // The order of operations might get mixed up resulting in some
    // numbers being printed before they are modified (100x) and others
    // after they've been modified (200x).
    boost::thread t1(modify, std::ref(vi));
    boost::thread t2(print, std::ref(vi));

    // Passing the vector to the new thread by value results in the vector
    // being copied to the thread local storage. That's a safe approach,
    // but the data is no longer modifiable from BOTH threads
    // (so we will see only 100x even though 'modify' is spawned first).
    //boost::thread t1(modify, vi);
    //boost::thread t2(print, vi);

    t1.join();
    t2.join();

    return 0;
}
