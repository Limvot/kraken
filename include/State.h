#ifndef STATE_H
#define STATE_H

#ifndef NULL
#define NULL 0
#endif

#include "util.h"
#include "ParseRule.h"

#include <vector>
#include <string>
#include <string>
#include <sstream>

class State {
	public:
		State(int number, ParseRule* basis);
		~State();
		bool const operator==(const State &other);
		bool const operator!=(const State &other);
		std::vector<ParseRule*>* getBasis();
		std::vector<ParseRule*>* getRemaining();
		std::vector<ParseRule*>* getTotal();
		bool containsRule(ParseRule* rule);
		std::string toString();


		std::vector<ParseRule*> basis;
		std::vector<ParseRule*> remaining;
	private:
		std::vector<ParseRule*> total;
		int number;
};

#endif