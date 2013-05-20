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

std::string Parser::grammerToString() {
	//Iterate through the vector, adding string representation of each grammer rule
	std::cout << "About to toString\n";
	std::string concat = "";
	for (int i = 0; i < loadedGrammer.size(); i++) {
		concat += loadedGrammer[i]->toString() + "\n";//->toString();// + std::endl;
	}
	return(concat);
}

