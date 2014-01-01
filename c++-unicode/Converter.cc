// The following code is original work by me.  I now place it in the public domain.  Do not plagiarize.
// 
// Andrew Choi.  Calgary, Canada, October 12, 2010.

#include <string>

#include "Converter.h"

template<>
int storageMultiplier<UTF8, UTF32>() { return 4; }

template<>
int storageMultiplier<UTF8, UTF16>() { return 3; }

template<>
int storageMultiplier<UTF16, UTF8>() { return 1; }

template<>
int storageMultiplier<UTF16, UTF32>() { return 2; }

template<>
int storageMultiplier<UTF32, UTF8>() { return 1; }

template<>
int storageMultiplier<UTF32, UTF16>() { return 1; }


std::string to_u8string(const std::u16string& s)
{
  static Converter<UTF8, UTF16> converter;

  return converter(s);
}

std::string to_u8string(const std::u32string& s)
{
  static Converter<UTF8, UTF32> converter;

  return converter(s);
}

std::u16string to_u16string(const std::string& s)
{
  static Converter<UTF16, UTF8> converter;

  return converter(s);
}

std::u16string to_u16string(const std::u32string& s)
{
  static Converter<UTF16, UTF32> converter;

  return converter(s);
}

std::u32string to_u32string(const std::string& s)
{
  static Converter<UTF32, UTF8> converter;

  return converter(s);
}

std::u32string to_u32string(const std::u16string& s)
{
  static Converter<UTF32, UTF16> converter;

  return converter(s);
}
