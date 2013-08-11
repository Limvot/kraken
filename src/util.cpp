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

std::string replaceExEscape(std::string first, std::string search, std::string replace) {
    size_t pos = 0;
    while (pos < first.size()-search.size()) {
        pos = first.find(search, pos);
        if (pos == std::string::npos)
            break;
        //std::cout << "Position is " << pos << " size of first is " << first.size() << " size of replace is " << replace.size() << std::endl;
        //If excaped, don't worry about it.
        if (pos > 0) {
            int numBackslashes = 0;
            int countBack = 1;
            while (pos-countBack >= 0 && first[pos-countBack] == '\\') {
                numBackslashes++;
                countBack++;
            }
            if (numBackslashes % 2 == 1) {
                pos++;
                continue;
            }
        }
        first = first.replace(pos, search.size(), replace);
        pos += replace.size();
    }
    return first;
}
