#include <vector>
#include <iostream>

using namespace std;

int main(int argc, const char *argv[])
{
    vector<int> v{1,2,3};
    cout << v[0] << " " << v[1] << " " << v[2] << endl;
    cout << v[500000] << endl;
    return 0;
}
