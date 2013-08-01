#ifndef LALRPARSER_H
#define LALRPARSER_H

#include "Parser.h"

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