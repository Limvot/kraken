#ifndef LALRPARSER_H
#define LALRPARSER_H

#include "Parser.h"
/*
#include "util.h"
#include "ParseRule.h"
#include "ParseAction.h"
#include "Symbol.h"
#include "State.h"
#include "StringReader.h"
#include "Lexer.h"
#include "NodeTree.h"
#include "Table.h"

#include <map>
#include <vector>
#include <algorithm>
#include <stack>
#include <string>
#include <iostream>
*/
class LALRParser: public Parser {
	public:
		LALRParser();
		~LALRParser();

		//using Parser::loadGrammer;

		//Defaults in parser are mostly LALR, so we only need to 
		//implement the actual parsing function
		NodeTree<Symbol*>* parseInput(std::string inputString);

	private:
		//Nothing

};

#endif