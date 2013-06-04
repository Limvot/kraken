#include "util.h"

std::string intToString(int theInt) {
	std::stringstream converter;
	converter << theInt;
	return converter.str();
}