#ifndef REGEX_H
#define REGEX_H

#include "util.h"
#include "RegExState.h"
#include "Symbol.h"

#include <string>

class RegEx {
	public:
		RegEx();
		RegEx(std::string inPattern);
		~RegEx();

		int longMatch(std::string stringToMatch);
		std::string getPattern();
	private:
		std::string pattern;
		RegExState* begin;
		std::vector<RegExState*> currentStates;
};
#endif