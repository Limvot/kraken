#ifndef UTIL_H
#define UTIL_H

#ifndef NULL
#define NULL 0
#endif

#include <string>
#include <sstream>

std::string intToString(int theInt);
std::string truncateEnd(std::string to_truncate);
std::string removeBeginning(std::string to_remove);

#endif