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
#include "Table.h"

#include <queue>
#include <set>
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

		virtual void loadGrammer(std::string grammerInputString);
		virtual void createStateSet();
		virtual std::string stateSetToString();
		virtual NodeTree<Symbol>* parseInput(std::string inputString, std::string filename, bool highlight_errors) = 0; // filename for error reporting
		virtual std::string grammerToString();
		virtual std::string grammerToDOT();

		std::string tableToString();
		void exportTable(std::ofstream &file);
		void importTable(char* tableData);

	protected:
		std::vector<Symbol> firstSet(Symbol token, std::vector<Symbol> avoidList = std::vector<Symbol>(), bool addNewTokens = true);
        bool isNullable(Symbol token);
        bool isNullableHelper(Symbol token, std::set<Symbol> done);

        std::map<Symbol, std::vector<Symbol>> tokenFirstSet;
        std::map<Symbol, bool> tokenNullable;

		std::vector<Symbol> incrementiveFollowSet(ParseRule* rule);
		virtual void closure(State* state);
		virtual void addStates(std::vector< State* >* stateSets, State* state, std::queue<State*>* toDo);
		int stateNum(State* state);


		StringReader reader;
		Lexer lexer;
		std::map<std::pair<std::string, bool>, Symbol> symbols;
		std::vector<ParseRule*> loadedGrammer;

		std::vector< State* > stateSets;

		Symbol EOFSymbol;
		Symbol nullSymbol;
		Symbol invalidSymbol;

		Table table;


		std::stack<int> stateStack;
		std::stack<Symbol> symbolStack;

		Symbol getOrAddSymbol(std::string symbolString, bool isTerminal);
};

#endif
