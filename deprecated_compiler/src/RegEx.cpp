#include "RegEx.h"
#include <cassert>

RegEx::RegEx(std::string inPattern) {
	pattern = inPattern;
	std::vector<RegExState*> ending;
	begin = construct(&ending, inPattern);
	//last one is goal state, add it to the end of all of these last states
	for (std::vector<RegExState*>::size_type i = 0; i < ending.size(); i++)
		ending[i]->addNext(NULL);
}

RegExState* RegEx::construct(std::vector<RegExState*>* ending, std::string pattern) {
	//In the RegEx re-write, instead of doing complicated unperenthesising, we keep track of both the "front" and the "end" of a state.
	//(these could be different if the state is perenthesezed)
	std::vector<RegExState*> previousStatesBegin;
	std::vector<RegExState*> previousStatesEnd;
	std::vector<RegExState*> currentStatesBegin;
	std::vector<RegExState*> currentStatesEnd;

	bool alternating = false;
	RegExState* begin = new RegExState();
	currentStatesBegin.push_back(begin);
	currentStatesEnd.push_back(begin);

	for (int i = 0; i < pattern.length(); i++) {
		switch (pattern[i]) {
			case '*':
			{
				//std::cout << "Star at " << i << " in " << pattern << std::endl;
				//NOTE: Because of the re-write, this is necessary again
				for (std::vector<RegExState*>::size_type j = 0; j < currentStatesEnd.size(); j++)
					for (std::vector<RegExState*>::size_type k = 0; k < currentStatesBegin.size(); k++)
						currentStatesEnd[j]->addNext(currentStatesBegin[k]); //Make the ends point to the beginnings
				//add all previous states to current states to enable skipping over the starred item
				currentStatesBegin.insert(currentStatesBegin.end(), previousStatesBegin.begin(), previousStatesBegin.end());
				currentStatesEnd.insert(currentStatesEnd.end(), previousStatesEnd.begin(), previousStatesEnd.end());
			}
				break;
			case '+':
			{
				//std::cout << "Plus at " << i << " in " << pattern << std::endl;
				//NOTE: Because of the re-write, this is necessary again
				for (std::vector<RegExState*>::size_type j = 0; j < currentStatesEnd.size(); j++)
					for (std::vector<RegExState*>::size_type k = 0; k < currentStatesBegin.size(); k++)
						currentStatesEnd[j]->addNext(currentStatesBegin[k]); //Make the ends point to the beginnings
			}
				break;
			case '?':
			{
				//std::cout << "Question at " << i << " in " << pattern << std::endl;
				//add all previous states to current states to enable skipping over the questioned item
				currentStatesBegin.insert(currentStatesBegin.end(), previousStatesBegin.begin(), previousStatesBegin.end());
				currentStatesEnd.insert(currentStatesEnd.end(), previousStatesEnd.begin(), previousStatesEnd.end());
			}
				break;
			case '|':
			{
				//std::cout << "Alternation at " << i << " in " << pattern << std::endl;
				//alternation
				alternating = true;
			}

				break;
			case '(':
			{
				//std::cout << "Begin peren at " << i << " in " << pattern << std::endl;
				//perentheses
				std::vector<RegExState*> innerEnds;
				int perenEnd = findPerenEnd(pattern, i);
				RegExState* innerBegin = construct(&innerEnds, strSlice(pattern, i+1, perenEnd));
				i = perenEnd;
				std::vector<RegExState*> innerBegins = innerBegin->getNextStates();
				if (alternating) {
					for (std::vector<RegExState*>::size_type j = 0; j < previousStatesEnd.size(); j++)
						for (std::vector<RegExState*>::size_type k = 0; k < innerBegins.size(); k++)
							previousStatesEnd[j]->addNext(innerBegins[k]);
					currentStatesBegin.insert(currentStatesBegin.end(), innerBegins.begin(), innerBegins.end());
					currentStatesEnd.insert(currentStatesEnd.end(), innerEnds.begin(), innerEnds.end());
				} else {
					for (std::vector<RegExState*>::size_type j = 0; j < currentStatesEnd.size(); j++)
						for (std::vector<RegExState*>::size_type k = 0; k < innerBegins.size(); k++)
							currentStatesEnd[j]->addNext(innerBegins[k]);
					previousStatesBegin = currentStatesBegin;
					previousStatesEnd = currentStatesEnd;
					currentStatesBegin = innerBegins;
					currentStatesEnd = innerEnds;
				}
				alternating = false;
			}
				break;

			// ) does not need a case as we skip over it after finding it in ('s case

			case '\\':
			{
				i++;
				//std::cout << "Escape! Escaping: " << pattern[i] << std::endl;
				//Ahh, it's escaping a special character, so fall through to the default.
			}
			default:
			{
				//std::cout << "Regular" << std::endl;
				//Ahh, it's regular
				RegExState* next = new RegExState(pattern[i]);
				//If we're alternating, add next as the next for each previous state, and add self to currentStates
				if (alternating) {
					for (std::vector<RegExState*>::size_type j = 0; j < previousStatesEnd.size(); j++)
						previousStatesEnd[j]->addNext(next);
					currentStatesBegin.push_back(next);
					currentStatesEnd.push_back(next);
					alternating = false;
				} else {
					//If we're not alternating, add next as next for all the current states, make the current states the new
					//previous states, and add ourself as the new current state.
					for (std::vector<RegExState*>::size_type j = 0; j < currentStatesEnd.size(); j++)
						currentStatesEnd[j]->addNext(next);

					previousStatesBegin.clear();
					previousStatesEnd.clear();
					previousStatesBegin = currentStatesBegin;
					previousStatesEnd = currentStatesEnd;
					currentStatesBegin.clear();
					currentStatesEnd.clear();
					currentStatesBegin.push_back(next);
					currentStatesEnd.push_back(next);
				}
			}
		}
	}
	(*ending) = currentStatesEnd;
	return(begin);
}


RegEx::~RegEx() {
	//No cleanup necessary
}

int RegEx::longMatch(std::string stringToMatch) {
	// Start in the begin state (only).
	int lastMatch = -1;
	currentStates.clear();
	currentStates.push_back(begin);
	std::vector<RegExState*> nextStates;

	for (int i = 0; i < stringToMatch.size(); i++) {
		//Go through every current state. Check to see if it is goal, if so update last goal.
		//Also, add each state's advance to nextStates
		for (std::vector<RegExState*>::size_type j = 0; j < currentStates.size(); j++) {
			if (currentStates[j]->isGoal())
				lastMatch = i;
			std::vector<RegExState*> addStates = currentStates[j]->advance(stringToMatch.at(i));
			nextStates.insert(nextStates.end(), addStates.begin(), addStates.end());
		}
		//Now, clear our current states and add eaczh one of our addStates if it is not already in current states

		currentStates.clear();
		for (std::vector<RegExState*>::size_type j = 0; j < nextStates.size(); j++) {
			bool inCurrStates = false;
			for (std::vector<RegExState*>::size_type k = 0; k < currentStates.size(); k++) {
				if (nextStates[j] == currentStates[k])
					inCurrStates = true;
			}
			if (!inCurrStates)
				currentStates.push_back(nextStates[j]);
		}
		// if (currentStates.size() != 0)
		// 	std::cout << "Matched " << i << " character: " << stringToMatch[i-1] << std::endl;

		nextStates.clear();
		//If we can't continue matching, just return our last matched
		if (currentStates.size() == 0)
			break;
	}
	//Check to see if we match on the last character in the string
	for (std::vector<RegExState*>::size_type j = 0; j < currentStates.size(); j++) {
		if (currentStates[j]->isGoal())
			lastMatch = stringToMatch.size();
	}
	return lastMatch;
}

std::string RegEx::getPattern() {
	return pattern;
}

std::string RegEx::toString() {
	return pattern + " -> " + begin->toString();
}

void RegEx::test() {
    {
        RegEx re("a*");
        assert(re.longMatch("a") == 1);
        assert(re.longMatch("aa") == 2);
        assert(re.longMatch("aaaab") == 4);
        assert(re.longMatch("b") == 0);
    }

    {
        RegEx re("a+");
        assert(re.longMatch("aa") == 2);
        assert(re.longMatch("aaaab") == 4);
        assert(re.longMatch("b") == -1);
    }

    {
        RegEx re("a(bc)?");
        assert(re.longMatch("ab") == 1);
    }

    {
    	RegEx re("((ab)|c)*");
    	assert(re.longMatch("ababc") == 5);
    	assert(re.longMatch("ad") == 0);
    	assert(re.longMatch("ababccd") == 6);
    }
    {
    	RegEx re("bbb((bba+)|(ba+))*a*((a+b)|(a+bb)|(a+))*bbb") ;
    	assert(re.longMatch("bbbababbbaaaaaaaaaaaaaaaaaaabbb") == 9);
    }

    std::cout << "RegEx tests pass\n";
}
