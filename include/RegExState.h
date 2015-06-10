#ifndef REGEXSTATE_H
#define REGEXSTATE_H

#include "util.h"
#include "Symbol.h"

#include <string>
#include <vector>

class RegExState {
	public:
		RegExState(char inCharacter);
		RegExState();
		~RegExState();

		void addNext(RegExState* nextState);
		bool characterIs(char inCharacter);
		std::vector<RegExState*> advance(char advanceCharacter);
		std::vector<RegExState*> getNextStates();

		bool isGoal();
		std::string toString();
		std::string toString(RegExState* avoid);
		std::string toString(std::vector<RegExState*>* avoid);

		char getCharacter();

	private:
		std::vector<RegExState*> nextStates;
		char character;
};
#endif
