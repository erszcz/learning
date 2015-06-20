#include <iostream>

using std::cout;
using std::endl;

struct super {
    int a;
    int b;
    virtual ~super() {}
};

struct sub : public super {
    int c;
    virtual ~sub() {}
};

int main(int argc, const char *argv[])
{
    sub s;
    s.a = 1, s.b = 2, s.c = 3;
    cout << s.a << " " << s.b << " " << s.c << endl;

    super* s2 = &s;
    cout << s2->a << " " << s2->b << endl;

    sub* s3 = dynamic_cast<sub*>(s2);
    cout << s3->a << " " << s3->b << endl;
    s.c = 5;
    cout << s.a << " " << s.b << " " << s.c << endl;
    cout << s3->a << " " << s3->b << " " << s3->c << endl;

    return 0;
}
