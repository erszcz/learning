//#include <boost/archive/text_oarchive.hpp>
#include <boost/archive/binary_oarchive.hpp>
#include <iostream>
#include <fstream>
#include <string>

void save()
{
    std::ofstream file("archive.dat");
    //boost::archive::text_oarchive oa(file);
    boost::archive::binary_oarchive oa(file);
    std::string s = "Hello World!";
    oa & s;
}

int main()
{
    save();
}
