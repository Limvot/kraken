#include "RegExState.h"

RegExState::RegExState(RegExState* inInnerState) {
	inner = inInnerState;
}

RegExState::RegExState(char inCharacter) {
	character = inCharacter;
	inner = NULL;
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

std::string RegExState::toString() {
	std::string string = "";
	string += character;
	for (std::vector<RegExState*>::size_type i = 0; i < nextStates.size(); i++)
		string += "->" + nextStates[i]->toString() + " EC ";
	//std::cout << "inner = " << inner << " nextStates size = " << nextStates.size() <<std::endl;
	return string;
}


