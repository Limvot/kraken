#ifndef PARSE_ACTION_H
#define PARSE_ACTION_H

#ifndef NULL
#define NULL ((void*)0)
#endif

#include "util.h"
#include "ParseRule.h"

#include <vector>
#include <string>


class ParseAction {
	public:
		enum ActionType { INVALID, REDUCE, SHIFT, ACCEPT, REJECT };
		ParseAction(ActionType action);
		ParseAction(ActionType action, ParseRule* reduceRule);
		ParseAction(ActionType action, int shiftState);
		~ParseAction();
		bool const equalsExceptLookahead(const ParseAction &other) const;
		bool const operator==(const ParseAction &other) const;
		bool const operator!=(const ParseAction &other) const;
        bool const operator<(const ParseAction &other) const;
        std::string toString(bool printRuleLookahead = true);
		static std::string actionToString(ActionType action);

		ActionType action;
		ParseRule* reduceRule;
		int shiftState;


};

#endif
