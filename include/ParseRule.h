#ifndef PARSERULE_H
#define PARSERULE_H

#ifndef NULL
#define NULL 0
#endif

#include "Symbol.h"

#include <vector>
#include <string>
#include <iostream>

class ParseRule {
	public:
		ParseRule();
		ParseRule(Symbol* leftHandle, int pointerIndex, std::vector<Symbol*> &rightSide);
		~ParseRule();

		ParseRule* clone();

		void setLeftHandle(Symbol* leftHandle);
		void appendToRight(Symbol* appendee);

		bool advancePointer();

		std::string toString();
		std::string toDOT();

	private:
		int pointerIndex;
		Symbol* leftHandle;
		std::vector<Symbol*> rightSide;

};

#endif