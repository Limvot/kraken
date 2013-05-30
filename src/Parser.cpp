#include "Parser.h"

Parser::Parser() {

}

Parser::~Parser() {

}

Symbol* Parser::getOrAddSymbol(std::string symbolString, bool isTerminal) {
	Symbol* symbol;
	if (symbols.find(symbolString) == symbols.end()) {
		symbol = new Symbol(symbolString, isTerminal);
		symbols[symbolString] = symbol;
	} else {
		symbol = symbols[symbolString];
	}
	return(symbol);
}

void Parser::loadGrammer(std::string grammerInputString) {
	reader.setString(grammerInputString);

	std::string currToken = reader.word();

	while(currToken != "") {
		//Load the left of the rule
		ParseRule* currentRule = new ParseRule();
		Symbol* leftSide = getOrAddSymbol(currToken, false); //Left handle is never a terminal
		currentRule->setLeftHandle(leftSide);
		reader.word(); //Remove the =
		//Add the right side, adding new Symbols to symbol map.
		currToken = reader.word();
		while (currToken != ";") {
			currentRule->appendToRight(getOrAddSymbol(currToken, currToken.at(0)=='\"')); //If first character is a ", then is a terminal
			currToken = reader.word();
			//If there are multiple endings to this rule, finish this rule and start a new one with same left handle
			if (currToken == "|") {
				loadedGrammer.push_back(currentRule);
				currentRule = new ParseRule();
				currentRule->setLeftHandle(leftSide);
				currToken = reader.word();
			}
		}
		//Add new rule to grammer
		loadedGrammer.push_back(currentRule);
		//Get next token
		currToken = reader.word();
	}
	std::cout << "Parsed!\n";
}

void Parser::createStateSet() {
	std::cout << "Begining creation of stateSet" << std::endl;
	stateSets.push_back( new State(0, loadedGrammer[0]) );
	std::cout << "Begining for main set for loop" << std::endl;
	for (std::vector< State* >::size_type i = 0; i < stateSets.size(); i++) {
		std::cout << "calling closure on " << stateSets[i]->toString() << std::endl;
		closure(stateSets[i]);
		std::cout << "finished closure" << std::endl;
		std::cout << "Starting inner for loop that adds states" << std::endl;
		std::vector<ParseRule*>* allRules = stateSets[i]->getTotal();
		for (std::vector<ParseRule*>::size_type j = 0; j < allRules->size(); j++) {
			std::cout << "about to call addState" << std::endl;
			addState(&stateSets, stateSets[i], (*allRules)[j]->getAtNextIndex());
			//Closure will be called in the outer loop
		}
	}
}

void Parser::closure(State* state) {
	//Add all the applicable rules.
	std::cout << "Closure on " << state->toString() << " is" << std::endl;
	for (std::vector<ParseRule*>::size_type i = 0; i < state->getTotal()->size(); i++) {
		for (std::vector<ParseRule*>::size_type j = 0; j < loadedGrammer.size(); j++) {
			//If the current symbol in the rule is not null (rule completed) and it equals a grammer's left side
			if ((*state->getTotal())[i]->getAtNextIndex() != NULL && *((*state->getTotal())[i]->getAtNextIndex()) == *(loadedGrammer[j]->getLeftSide())) {
				std::cout << (*state->getTotal())[i]->getAtNextIndex()->toString() << " has an applicable production " << loadedGrammer[j]->toString() << std::endl;
				//Check to make sure not already in
				bool isAlreadyInState = false;
				for (std::vector<ParseRule*>::size_type k = 0; k < state->getTotal()->size(); k++) {
					if ((*state->getTotal())[k] == loadedGrammer[j]) {
						isAlreadyInState = true;
						break;
					}
				}
				if (!isAlreadyInState)
					state->remaining.push_back(loadedGrammer[j]);
			}
		}
	}
	std::cout << state->toString() << std::endl;
}

//Adds state if it doesn't already exist.
void Parser::addState(std::vector< State* >* stateSets, State* state, Symbol* symbol) {
	std::vector< State* > newStates;
	//For each rule in the state we already have
	for (std::vector<ParseRule*>::size_type i = 0; i < state->getTotal()->size(); i++) {
		//Clone the current rule
		ParseRule* advancedRule = (*state->getTotal())[i]->clone();
		//Try to advance the pointer
		if (advancedRule->advancePointer()) { 
			//Technically, it should be the set of rules sharing this symbol advanced past in the basis for new state

			//So search our new states to see if any of them use this advanced symbol as a base.
			//If so, add this rule to them.
			//If not, create it.
			bool symbolAlreadyInState = false;
			for (std::vector< State* >::size_type j = 0; j < newStates.size(); j++) {
				if (*(newStates[j]->basis[0]->getAtIndex()) == *(advancedRule->getAtIndex())) {
					symbolAlreadyInState = true;
					//So now check to see if this exact rule is in this state
					if (!newStates[j]->containsRule(advancedRule)) {
						newStates[j]->basis.push_back(advancedRule);
					}
					//We found a state with the same symbol, so stop searching
					break;
				}
			}
			if (!symbolAlreadyInState) {
				State* newState = new State(stateSets->size()+newStates.size(),advancedRule);
				newStates.push_back(newState);
			}
		}
	}
	//Put all our new states in the set of states only if they're not already there.
	bool stateAlreadyInAllStates = false;
	for (std::vector< State * >::size_type i = 0; i < newStates.size(); i++) {
		for (std::vector< State * >::size_type j = 0; j < stateSets->size(); j++) {
			if (*(newStates[i]) == *((*stateSets)[j])) {
				stateAlreadyInAllStates = true;
				//std::cout << newStates[i]->toString() << " is equal to\n" << (*stateSets)[j]->toString() << std::endl;
			}
		}
		if (!stateAlreadyInAllStates) {
			stateSets->push_back(newStates[i]);
			stateAlreadyInAllStates = false;
		}
	}
}

std::string Parser::stateSetToString() {
	std::string concat = "";
	for (std::vector< State *>::size_type i = 0; i < stateSets.size(); i++) {
		concat += stateSets[i]->toString();
	}
	return concat;
}

int Parser::gotoTable(int state, Symbol* token) {
	std::vector<ParseRule*> allInState = *(stateSets[state]->getTotal());
	ParseRule* currentRule;
	for (std::vector<ParseRule*>::size_type i = 0; i < allInState.size(); i++) {
		currentRule = allInState[i];
		if (*(currentRule->getAtNextIndex()) == *token) {
			ParseRule* advancedCurrent = currentRule->clone();
			advancedCurrent->advancePointer();
			for (std::vector<State*>::size_type j = 0; j < stateSets.size(); j++) {
				for (std::vector<ParseRule*>::size_type k = 0; k < stateSets[j]->basis.size(); k++ ) {
					if ( *(stateSets[j]->basis[k]) == *advancedCurrent)
						return(j);
				}
			}
		}
	}
	return(-1);
}

ParseAction* Parser::actionTable(int state, Symbol* token) {
	std::vector<ParseRule*>* allStateRules = stateSets[state]->getTotal();
	ParseRule* currentRule;
	for (std::vector<ParseRule*>::size_type i = 0; i < allStateRules->size(); i++) {
		currentRule = (*allStateRules)[i];
		//If the current rule in the state is completed, then do a reduce action
		if (currentRule->isAtEnd()) {
			if (*currentRule == *(stateSets[0]->basis[0]))
				return new ParseAction(ParseAction::ACCEPT);
			return new ParseAction(ParseAction::REDUCE, currentRule);
		}
		//If the current rule in the state is not completed, see if it has the next correct token
		std::cout << currentRule->getAtNextIndex()->toString() << " comp to " << token->toString() << std::endl;
		if ( *(currentRule->getAtNextIndex()) == *token){
			//If it does have the correct next token, then find the state that has this rule advanced as basis, that is the state we shift to
			//Goes to n^2 here, really need that table
			ParseRule* advancedCurrent = currentRule->clone();
			advancedCurrent->advancePointer();
			for (std::vector<State*>::size_type j = 0; j < stateSets.size(); j++) {
				for (std::vector<ParseRule*>::size_type k = 0; k < stateSets[j]->basis.size(); k++ ) {
					if ( *(stateSets[j]->basis[k]) == *advancedCurrent)
						return new ParseAction(ParseAction::SHIFT, j);
				}
			}
		}
	}
	return new ParseAction(ParseAction::REJECT);
}

void Parser::parseInput(std::string inputString) {
	StringReader inputReader;
	inputReader.setString(inputString);
	Symbol* token = new Symbol(inputReader.word(), true);
	ParseAction* action;

	stateStack.push(0);
	symbolStack.push(new Symbol("INVALID", false));

	while (true) {
		action = actionTable(stateStack.top(), token);
		switch (action->action) {
			case ParseAction::REDUCE:
			{
				int rightSideLength = action->reduceRule->getRightSide().size();
				for (int i = 0; i < rightSideLength; i++) {
					stateStack.pop();
					symbolStack.pop();
				}
				symbolStack.push(action->reduceRule->getLeftSide());
				stateStack.push(gotoTable(stateStack.top(), symbolStack.top()));
				std::cout << "Reduce by " << action->reduceRule->toString() << std::endl;
				break;
			}
			case ParseAction::SHIFT:
				symbolStack.push(token);
				token = new Symbol(inputReader.word(), true);
				stateStack.push(action->shiftState);
				std::cout << "Shift " << symbolStack.top()->toString() << std::endl;
				break;
			case ParseAction::ACCEPT:
				std::cout << "ACCEPTED!" << std::endl;
				return;
				break;
			case ParseAction::REJECT:
				std::cout << "REJECTED!" << std::endl;
				return;
				break;
		}
	}
}

std::string Parser::grammerToString() {
	//Iterate through the vector, adding string representation of each grammer rule
	std::cout << "About to toString\n";
	std::string concat = "";
	for (int i = 0; i < loadedGrammer.size(); i++) {
		concat += loadedGrammer[i]->toString() + "\n";
	}
	return(concat);
}

std::string Parser::grammerToDOT() {
	//Iterate through the vector, adding DOT representation of each grammer rule
	std::cout << "About to DOT export\n";
	std::string concat = "";
	for (int i = 0; i < loadedGrammer.size(); i++) {
		concat += loadedGrammer[i]->toDOT();
	}
	return("digraph Kraken_Grammer { \n" + concat + "}");
}

