#ifndef REGEX_H
#define REGEX_H

#include "util.h"
#include "RegExState.h"
#include "Symbol.h"

#include <string>
#include <utility>
#include <stack>
#include <vector>

class RegEx {
	public:
		RegEx();
		RegEx(std::string inPattern);
		~RegEx();

		void construct();
		void deperenthesize();
		int longMatch(std::string stringToMatch);
		std::string getPattern();
		std::string toString();
		static void test();
	private:
		std::string pattern;
		RegExState* begin;
		std::vector<RegExState*> currentStates;
};
#endif
