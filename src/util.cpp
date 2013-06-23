#include "util.h"

std::string intToString(int theInt) {
	std::stringstream converter;
	converter << theInt;
	return converter.str();
}
std::string truncateEnd(std::string to_truncate)
{
    std::string to_return = "";
    for (unsigned int i = 0; i < to_truncate.length()-1; i++)
        to_return = to_return + to_truncate[i];
    return to_return;
}

std::string removeBeginning(std::string to_remove)
{
    std::string to_return = "";
    for (unsigned int i = 1; i < to_remove.length(); i++)
        to_return = to_return + to_remove[i];
    return to_return;
}