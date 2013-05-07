//#include <boost/archive/text_iarchive.hpp> 
#include <boost/archive/binary_iarchive.hpp> 
#include <boost/serialization/string.hpp>
#include <iostream> 
#include <fstream> 
#include <string>

void load() 
{
    std::ifstream file("archive.dat");
    //boost::archive::text_iarchive ia(file); 
    boost::archive::binary_iarchive ia(file);
    std::string s;
    ia & s;
    std::cout << s << std::endl;
}

int main()
{
    load();
}
