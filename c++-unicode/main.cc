// -*- compile-command:"g++ -std=c++0x -o t1 Converter.cc t1.cc" -*-

#include <iostream>
#include "Converter.h"

int main()
{
  std::string s8;
  // Read each line from standard input in UTF-8 encoding.
  while (!getline(std::cin, s8).eof())
    {
      std::u32string s32 = to_u32string(s8);

      // Print the value of each code point in hexidecimal.
      for (unsigned int i = 0; i < s32.length(); i++)
        std::cout << std::hex << s32[i] << std::endl;
    }
}
