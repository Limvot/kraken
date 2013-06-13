#ifndef LEXER_H
#define LEXER_H

#include "util.h"
#include "StringReader.h"
#include "Symbol.h"

#include <string>

class Lexer {
	public:
		Lexer();
		Lexer(std::string inputString);
		~Lexer();
		void setInput(std::string inputString);
		Symbol* next();
	private:
		StringReader reader;
};
#endif