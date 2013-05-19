#include <cassert>
#include <iostream>
#include <iterator>
#include <vector>

using std::vector;

vector<int> add_one(vector<int> v) {
    int c = 0;
    for (auto i = v.rbegin(); i != v.rend(); ++i) {
        *i += 1 + c;
        if (*i >= 10) {
            *i = 0;
            c = 1;
        } else {
            c = 0;
            break;
        }
    }
    if (c)
        v.insert(v.begin(),c);
    return v;
}

int main(int argc, const char *argv[]) {
    assert( (vector<int>{9,9} == add_one(vector<int>{9,8})) );
    assert( (vector<int>{1,0,0} == add_one(vector<int>{9,9})) );
    return 0;
}
