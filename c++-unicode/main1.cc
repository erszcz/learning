#include <iostream>
#include "Converter.h"

int main()
{
  std::u16string s16 = u"鵝滿是快烙滴好耳痛";

  std::cout << to_u8string(s16) << std::endl;
}

// Local Variables:
// coding:utf-8
// compile-command:"g++ -finput-charset=UTF-8 -std=c++0x -o t2 Converter.cc t2.cc"
// End:
