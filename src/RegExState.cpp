#include "RegExState.h"

RegExState::RegExState(RegExState* inInnerState) {
	inner = inInnerState;
}

RegExState::RegExState(char inCharacter) {
	character = inCharacter;
	inner = NULL;
}

RegExState::RegExState() {
	character = 0;
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
		if (nextStates[i] != NULL && nextStates[i]->characterIs(advanceCharacter)) 
			advanceStates->push_back(nextStates[i]);
	}
	return advanceStates;
}

RegExState* RegExState::getInner() {
	return inner;
}

std::vector<RegExState*>* RegExState::getNextStates() {
	return &nextStates;
}

bool RegExState::isGoal() {
	//return inner == NULL && nextStates.size() == 0;
	for (std::vector<RegExState*>::size_type i = 0; i < nextStates.size(); i++)
		if (nextStates[i] == NULL)
			return true;
	return false;
}

std::string RegExState::toString() {
	std::vector<RegExState*> avoidList;
	return toString(&avoidList);
}

std::string RegExState::toString(RegExState* avoid) {
	std::vector<RegExState*> avoidList;
	avoidList.push_back(avoid);
	return toString(&avoidList);
}

std::string RegExState::toString(std::vector<RegExState*>* avoid) {
	avoid->push_back(this);
	std::string string = "";
	string += std::string("\"") + character + "\"";
	if (inner != NULL) {
		string += "inner: ";
		string += inner->toString(avoid);
		string += " end inner ";
	}
	for (std::vector<RegExState*>::size_type i = 0; i < nextStates.size(); i++) {
		bool inAvoid = false;
		for (std::vector<RegExState*>::size_type j = 0; j < avoid->size(); j++) {
			if (nextStates[i] == (*avoid)[j]) {
				inAvoid = true;
			}
		}
		if (inAvoid) {
			string += "->LoopDetected";
			continue;
		}

		if (nextStates[i] != this && nextStates[i] != NULL)
			string += "->" + nextStates[i]->toString(avoid) + " EC ";
		else if (nextStates[i] == NULL)
			string += "-> GOAL ";
		else
			string += "->this";
	}
	//std::cout << "inner = " << inner << " nextStates size = " << nextStates.size() <<std::endl;
	return string;
}

char RegExState::getCharacter() {
	return character;
}
