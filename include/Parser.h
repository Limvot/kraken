#ifndef PARSER_H
#define PARSER_H

#ifndef NULL
#define NULL 0
#endif

#include "ParseRule.h"
#include "Symbol.h"
#include "StringReader.h"

#include <map>
#include <vector>
#include <string>
#include <iostream>

class Parser {
	public:
		Parser();
		~Parser();

		void loadGrammer(std::string grammerInputString);
		std::string grammerToString();
	private:
		StringReader reader;
		std::map<std::string, Symbol*> symbols;
		std::vector<ParseRule*> loadedGrammer;

		Symbol* getOrAddSymbol(std::string symbolString, bool isTerminal);
};

#endif