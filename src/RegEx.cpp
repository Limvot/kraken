#include "RegEx.h"

RegEx::RegEx(std::string inPattern) {
	pattern = inPattern;
	std::vector<RegExState*> previousStates;
	std::vector<RegExState*> currentStates;
	begin = new RegExState();
	currentStates.push_back(begin);
	for (int i = 0; i < pattern.length(); i++) {
		switch (pattern[i]) {
			case '*':
			{
				std::cout << "Star at " << i << " in " << pattern << std::endl;
				for (std::vector<RegExState*>::size_type j = 0; j < currentStates.size(); j++)
					for (std::vector<RegExState*>::size_type k = 0; k < currentStates.size(); k++)
						currentStates[j]->addNext(currentStates[k]);
				//add all previous states to current states to enable skipping over the starred item
				currentStates.insert(currentStates.end(), previousStates.begin(), previousStates.end());
			}
				break;
			case '+':
			{
				std::cout << "Plus at " << i << " in " << pattern << std::endl;
				//OtherThingy
				//current->addNext(current);
				for (std::vector<RegExState*>::size_type j = 0; j < currentStates.size(); j++)
					for (std::vector<RegExState*>::size_type k = 0; k < currentStates.size(); k++)
						currentStates[j]->addNext(currentStates[k]);
			}
				break;
			case '?':
			{
				std::cout << "Question at " << i << " in " << pattern << std::endl;
				//add all previous states to current states to enable skipping over the questioned item
				currentStates.insert(currentStates.end(), previousStates.begin(), previousStates.end());
			}
				break;
			case '|':
				std::cout << "Alternation at " << i << " in " << pattern << std::endl;
				//alternation
				break;
			case '(':
				std::cout << "Begin peren at " << i << " in " << pattern << std::endl;
				//perentheses
				break;
			default:
			{
				std::cout << "Regular" << std::endl;
				//Ahh, it's regular
				RegExState* next = new RegExState(pattern[i]);
				for (std::vector<RegExState*>::size_type j = 0; j < currentStates.size(); j++)
					currentStates[j]->addNext(next);

				previousStates.clear();
				previousStates.insert(previousStates.begin(), currentStates.begin(), currentStates.end());
				currentStates.clear();
				currentStates.push_back(next);
			}
		}
	}
	//last one is goal state
	for (std::vector<RegExState*>::size_type i = 0; i < currentStates.size(); i++)
		currentStates[i]->addNext(NULL);
}

RegEx::~RegEx() {
	//No cleanup necessary
}

int RegEx::longMatch(std::string stringToMatch) {
	//If the beginning character is wrong, exit immediantly. Otherwise, get all the states we can get from adding the second character to the state where we accepted the first
	int lastMatch = -1;
	currentStates = *(begin->advance(stringToMatch[0]));
	std::vector<RegExState*> nextStates;

	for (int i = 1; i < stringToMatch.size(); i++) {
		//Go through every current state. Check to see if it is goal, if so update last goal.
		//Also, add each state's advance to nextStates
		for (std::vector<RegExState*>::size_type j = 0; j < currentStates.size(); j++) {
			if (currentStates[j]->isGoal()) {
				lastMatch = i-1;
				std::cout << "Hit goal at " << i << " character: " << stringToMatch[i-1] << std::endl;
			} else {
				std::cout << "currentState " << j << ", " << currentStates[j]->toString() << " is not goal" <<std::endl;
			}
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
		if (currentStates.size() != 0)
			std::cout << "Matched " << i << " character: " << stringToMatch[i-1] << std::endl;
/*
		std::cout << "Current states are: ";
		for (std::vector<RegExState*>::size_type j = 0; j < currentStates.size(); j++)
			std::cout << currentStates[j]->toString() << " ";
		std::cout << std::endl;
*/
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

std::string RegEx::toString() {
	return pattern + " -> " + begin->toString();
}
