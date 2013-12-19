#ifndef UTIL_H
#define UTIL_H

#ifndef NULL
#define NULL 0
#endif

#include <iostream>
#include <string>
#include <sstream>

std::string intToString(int theInt);
std::string replaceExEscape(std::string first, std::string search, std::string replace);
std::string strSlice(std::string str, int begin, int end);

#endif
