#ifndef LEXER_H
#define LEXER_H

#include "util.h"
#include "StringReader.h"
#include "RegEx.h"
#include "Symbol.h"

#include <string>

class Lexer {
	public:
		Lexer();
		Lexer(std::string inputString);
		~Lexer();
		void addRegEx(std::string regExString);
		void setInput(std::string inputString);
		Symbol next();
	private:
		std::vector<RegEx*> regExs;
		std::string input;
		int currentPosition;
};
#endif