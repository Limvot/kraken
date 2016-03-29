#ifndef STATE_H
#define STATE_H

#ifndef NULL
#define NULL ((void*)0)
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
		State(int number, ParseRule* basis, State* parent);
		~State();
		bool const operator==(const State &other);
		bool const basisEquals(const State &other);
		bool const basisEqualsExceptLookahead(const State &other);
		bool const operator!=(const State &other);
		std::vector<ParseRule*>* getBasis();
		std::vector<ParseRule*>* getRemaining();
		std::vector<ParseRule*> getTotal();
		bool containsRule(ParseRule* rule);
		void addRuleCombineLookahead(ParseRule* rule);
		std::string toString();

		void combineStates(State &other);
		void addParents(std::vector<State*>* parents);
		std::vector<State*>* getParents();
		std::vector<State*>* getDeepParents(int depth);
		int getNumber();


		std::vector<ParseRule*> basis;
		std::vector<ParseRule*> remaining;
	private:
		std::vector<State*> parents;
		int number;
};

#endif
