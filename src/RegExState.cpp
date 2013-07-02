#include "RegExState.h"

RegExState::RegExState(RegExState* inInnerState) {
	inner = inInnerState;
}

RegExState::RegExState(char inCharacter) {
	character = inCharacter;
}

RegExState::~RegExState() {
	//No cleanup necessary
}

void RegExState::addNext(RegExState* nextState) {
	nextStates.push_back(nextState);
}

bool RegExState::characterIs(char inCharacter) {
	return character == inCharacter;
}

std::vector<RegExState*>* RegExState::advance(char advanceCharacter) {
	std::vector<RegExState*>* advanceStates = new std::vector<RegExState*>();
	for (std::vector<RegExState*>::size_type i = 0; i < nextStates.size(); i++) {
		if (nextStates[i]->characterIs(advanceCharacter)) 
			advanceStates->push_back(nextStates[i]);
	}
	return advanceStates;
}

bool RegExState::isGoal() {
	return inner == NULL && nextStates.size() == 0;
}


