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

