#ifndef UTIL_H
#define UTIL_H

#ifndef NULL
#define NULL 0
#endif

#include <iostream>
#include <string>
#include <sstream>
#include <vector>

std::string intToString(int theInt);
std::string replaceExEscape(std::string first, std::string search, std::string replace);
std::string strSlice(std::string str, int begin, int end);
int findPerenEnd(std::string str, int i);
std::vector<std::string> split(const std::string &str, char delim);
std::string join(const std::vector<std::string> &strVec, std::string joinStr);

#endif
