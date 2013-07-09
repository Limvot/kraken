#ifndef PARSER_H
#define PARSER_H

#include "util.h"
#include "ParseRule.h"
#include "ParseAction.h"
#include "Symbol.h"
#include "State.h"
#include "StringReader.h"
#include "Lexer.h"
#include "NodeTree.h"

#include <map>
#include <vector>
#include <algorithm>
#include <stack>
#include <string>
#include <iostream>

class Parser {
	public:
		Parser();
		~Parser();

		void loadGrammer(std::string grammerInputString);
		std::vector<Symbol*>* firstSet(Symbol* token);
		void printFirstSets();
		std::vector<Symbol*>* incrementiveFollowSet(ParseRule* rule);
		void createStateSet();
		void closure(State* state);
		void addStates(std::vector< State* >* stateSets, State* state);
		std::string stateSetToString();
		void addToTable(State* fromState, Symbol* tranSymbol, ParseAction* action);
		ParseAction* getTable(int state, Symbol* token);
		NodeTree* parseInput(std::string inputString);

		std::string grammerToString();
		std::string grammerToDOT();

		std::string tableToString();

	private:
		StringReader reader;
		Lexer lexer;
		std::map<std::string, Symbol*> symbols;
		std::vector<ParseRule*> loadedGrammer;

		std::vector< State* > stateSets;

		//The EOFSymbol, a pointer because of use in table, etc
		Symbol* EOFSymbol;
		//The nullSymbol, ditto with above. Also used in comparisons
		Symbol* nullSymbol;

		std::vector< std::vector<ParseAction*>* >  table;
		std::vector<Symbol*> symbolIndexVec;

		std::stack<int> stateStack;
		std::stack<Symbol*> symbolStack;

		Symbol* getOrAddSymbol(std::string symbolString, bool isTerminal);
		NodeTree* reduceTreeCombine(Symbol* newSymbol, std::vector<Symbol*> &symbols);
};

#endif