#ifndef PARSER_H
#define PARSER_H

#ifndef NULL
#define NULL 0
#endif

#include "ParseRule.h"
#include "ParseAction.h"
#include "Symbol.h"
#include "StringReader.h"

#include <map>
#include <vector>
#include <stack>
#include <string>
#include <sstream>
#include <iostream>

class Parser {
	public:
		Parser();
		~Parser();

		std::string intToString(int theInt);

		void loadGrammer(std::string grammerInputString);
		void createStateSet();
		void closure(std::vector<ParseRule*>* state);
		void addState(std::vector< std::vector<ParseRule*>* >* stateSets, std::vector<ParseRule*>* state, Symbol*);
		std::string stateSetToString();
		int gotoTable(int state, Symbol* token);
		ParseAction* actionTable(int state, Symbol* token);
		void parseInput(std::string inputString);

		std::string grammerToString();
		std::string grammerToDOT();
	private:
		StringReader reader;
		std::map<std::string, Symbol*> symbols;
		std::vector<ParseRule*> loadedGrammer;

		std::vector< std::vector<ParseRule*>* > stateSets;

		std::stack<int> stateStack;
		std::stack<Symbol*> symbolStack;

		Symbol* getOrAddSymbol(std::string symbolString, bool isTerminal);
};

#endif