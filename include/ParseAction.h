#ifndef PARSE_ACTION_H
#define PARSE_ACTION_H

#ifndef NULL
#define NULL 0
#endif

#include "ParseRule.h"

#include <vector>
#include <string>

class ParseAction {
	public:
		enum ActionType { INVALID, REDUCE, SHIFT, ACCEPT, REJECT };
		ParseAction(ActionType action, ParseRule* reduceRule = NULL, int shiftState = 0);
		~ParseAction();
		std::string toString();
		static std::string actionToString(ActionType action);

		ActionType action;
		ParseRule* reduceRule;
		int shiftState;


};

#endif