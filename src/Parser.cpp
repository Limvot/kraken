#include "Parser.h"

Parser::Parser() {

}

Parser::~Parser() {

}

std::string Parser::intToString(int theInt) {
	std::stringstream converter;
	converter << theInt;
	return converter.str();
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
	stateSets.push_back( new std::vector<ParseRule*> );
	stateSets[0]->push_back(loadedGrammer[0]);
	std::cout << "Begining for main set for loop" << std::endl;
	for (std::vector< std::vector<ParseRule*>* >::size_type i = 0; i < stateSets.size(); i++) {
		std::cout << "calling closure" << std::endl;
		closure(stateSets[i]);
		std::cout << "finished closure" << std::endl;
		std::cout << "Starting inner for loop that adds states" << std::endl;
		for (std::vector<ParseRule*>::size_type j = 0; j < stateSets[i]->size(); j++) {
			std::cout << "about to call addState" << std::endl;
			addState(&stateSets, stateSets[i], (*stateSets[i])[j]->getRightSide()[(*stateSets[i])[j]->getIndex()]);
			std::cout << "finished addState" << std::endl;
			//Closure will be called in the outer loop
		}
	}
}

void Parser::closure(std::vector<ParseRule*>* state) {
	//Add all the applicable rules.
	for (std::vector<ParseRule*>::size_type i = 0; i < state->size(); i++) {
		for (std::vector<ParseRule*>::size_type j = 0; j < loadedGrammer.size(); j++) {
			if ((*state)[i]->getRightSide()[(*state)[i]->getIndex()] == loadedGrammer[j]->getLeftSide()) {
				//Check to make sure not already in
				bool isAlreadyInState = false;
				for (std::vector<ParseRule*>::size_type k = 0; k < state->size(); k++) {
					if ((*state)[k] == loadedGrammer[j]) {
						isAlreadyInState = true;
						break;
					}
				}
				if (!isAlreadyInState)
					state->push_back(loadedGrammer[j]);
			}
		}
	}
}

//Adds state if it doesn't already exist.
void Parser::addState(std::vector< std::vector<ParseRule*>* >* stateSets, std::vector<ParseRule*>* state, Symbol* symbol) {
	std::vector<std::vector<ParseRule*>* > newStates;
	//For each rule in the state we already have
	for (std::vector<ParseRule*>::size_type i = 0; i < state->size(); i++) {
		//Clone the current rule
		ParseRule* advancedRule = (*state)[i]->clone();
		//Try to advance the pointer
		if (advancedRule->advancePointer()) { 
			//If sucessful, check to see if this the advanced symbol is the basis for any of our new states
			bool symbolAlreadyInState = false;
			for (std::vector<std::vector<ParseRule*>* >::size_type j = 0; j < newStates.size(); j++) {
				if ((*newStates[j])[0]->getRightSide()[(*newStates[j])[0]->getIndex()] == advancedRule->getRightSide()[advancedRule->getIndex()]) {
					symbolAlreadyInState = true;
					//So now check to see if this exact rule is in this state
					bool ruleAlreadyInState = false;
					for (std::vector<ParseRule*>::size_type k = 0; k < newStates[j]->size(); k++) {
						if (*(*newStates[j])[k] == (*advancedRule) ) {
							ruleAlreadyInState = true;
							break;
						}
					}
					if (!ruleAlreadyInState) {
						newStates[j]->push_back(advancedRule);
					}
					//We found a state with the same symbol, so stop searching
					break;
				}
			}
			if (!symbolAlreadyInState) {
				std::vector<ParseRule*>* newState = new std::vector<ParseRule*>;
				newState->push_back(advancedRule);
				newStates.push_back(newState);
			}
		}
	}
	//Put all our new states in the set of states
	for (std::vector< std::vector<ParseRule*> * >::size_type i = 0; i < newStates.size(); i++) {
		stateSets->push_back(newStates[i]);
	}
}

std::string Parser::stateSetToString() {
	std::string concat = "";
	int currentNum = 0;
	for (std::vector< std::vector<ParseRule*> *>::size_type i = 0; i < stateSets.size(); i++) {
		concat += "State " + intToString(currentNum) + ":\n";
		for (std::vector<ParseRule*>::size_type j = 0; j < stateSets[i]->size(); j++) {
			concat += "\t" + (*stateSets[i])[j]->toString() + "\n";
		}
		concat += "\n";
		currentNum++;
	}
	return concat;
}

int Parser::gotoTable(int state, Symbol* token) {
	return 0;
}

ParseAction* Parser::actionTable(int state, Symbol* token) {
	return NULL;
}

void Parser::parseInput(std::string inputString) {
	StringReader inputReader;
	inputReader.setString(inputString);

	Symbol* token = new Symbol(reader.word(), false);
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
				token = new Symbol(inputReader.word(), false);
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

