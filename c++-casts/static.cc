#include <iostream>

using std::cerr;
using std::cout;
using std::endl;

struct sup {
    virtual ~sup() {};

    int field_a = 1;
};

struct sub : public sup {
    virtual ~sub() {};

    int field_b = 2;
};

void my_function(sup* obj) {
    //sub* obj2 = static_cast<sub*>(obj);
    sub* obj2 = dynamic_cast<sub*>(obj);
    if (obj2)
        cout << "field_b: " << obj2->field_b << endl;
    else {
        cerr << "bad_cast" << endl;
    }
}

int main(int argc, const char *argv[])
{
    auto a = sup();
    auto b = sub();
    
    my_function(&a);
    my_function(&b);

    return 0;
}
