#ifndef REGEXSTATE_H
#define REGEXSTATE_H

#include "util.h"
#include "Symbol.h"

#include <string>
#include <vector>

class RegExState {
	public:
		RegExState(RegExState* inInnerState);
		RegExState(char inCharacter);

		~RegExState();

		void addNext(RegExState* nextState);
		bool characterIs(char inCharacter);
		std::vector<RegExState*>* advance(char advanceCharacter);
		bool isGoal();

	private:
		std::vector<RegExState*> nextStates;
		RegExState* inner;
		char character;
};
#endif