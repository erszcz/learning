// -*- compile-command:"g++ -std=c++0x -o t1 Converter.cc t1.cc" -*-

#include <iostream>
#include <fstream>
#include "Converter.h"

int main()
{
  std::string s8;
  std::ifstream fin("testfile");
  fin >> s8;
  std::u32string s32 = to_u32string(s8);
  //std::cout << to_u8string(s32.substr(2,1)) << std::endl;
  s32 = s32[2];
  std::cout << to_u8string(s32) << std::endl;
  std::cout << "len(s32) = " << s32.size() << std::endl;
}
