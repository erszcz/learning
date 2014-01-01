// -*- compile-command:"g++ -std=c++0x -o t1 Converter.cc t1.cc" -*-

#include <iostream>
#include "Converter.h"

int main(int argc, const char* argv[])
{
  std::string s8(argv[1]);
  std::u32string s32 = to_u32string(s8);
  for (unsigned int i = 0; i < s32.length(); i++) {
        std::cout << to_u8string(s32.substr(i, 1)) << "\t" << std::hex << s32[i] << std::endl;
  }
}
