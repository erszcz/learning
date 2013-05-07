#include <iostream>

using namespace std;

static int nextid = 0;

struct s {
    int id;
    s() : id(nextid++) {}
    ~s() { cout << id << " dying" << endl; }
};

int main(int argc, const char *argv[])
{
    s *ar1 = new s[3];
    s *ar2 = new s[3];
    delete[] ar1;
    //delete ar2;
    delete[] ar2;
    return 0;
}
