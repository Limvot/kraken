#include "RegEx.h"

RegEx::RegEx(std::string inPattern) {
	pattern = inPattern;
	RegExState* current;
	begin = new RegExState(pattern.at(0));
	current = begin;
	for (int i = 1; i < pattern.length(); i++) {
		RegExState* next = new RegExState(pattern.at(i));
		current->addNext(next);
		current = next;
	}
}

RegEx::~RegEx() {
	//No cleanup necessary
}

int RegEx::longMatch(std::string stringToMatch) {
	int lastMatch = 0;
	currentStates = *(begin->advance(stringToMatch.at(0)));
	std::vector<RegExState*> nextStates;

	for (int i = 1; i < stringToMatch.size(); i++) {
		//Go through every current state. Check to see if it is goal, if so update last goal.
		//Also, add each state's advance to nextStates
		for (std::vector<RegExState*>::size_type j = 0; j < currentStates.size(); j++) {
			if (currentStates[j]->isGoal())
				lastMatch = i-1;
			std::vector<RegExState*>* addStates = currentStates[j]->advance(stringToMatch.at(i));
			nextStates.insert(nextStates.end(), addStates->begin(), addStates->end());
			delete addStates;
		}
		//Now, clear our current states and add eaczh one of our addStates if it is not already in current states
		currentStates.clear();
		for (std::vector<RegExState*>::size_type j = 0; j < nextStates.size(); j++) {
			bool inCurrStates = false;
			for (std::vector<RegExState*>::size_type k = 0; k < currentStates.size(); k++) {
				if (nextStates[j] == currentStates[i])
					inCurrStates = true;
			}
			if (!inCurrStates)
				currentStates.push_back(nextStates[j]);
		}
		nextStates.clear();
		//If we can't continue matching, just return our last matched
		if (currentStates.size() == 0)
			break;
	}
	//Check to see if we match on the last character in the string
	for (std::vector<RegExState*>::size_type j = 0; j < currentStates.size(); j++) {
		if (currentStates[j]->isGoal())
			lastMatch = stringToMatch.size()-1;
	}
	return lastMatch;
}

std::string RegEx::getPattern() {
	return pattern;
}
