#include "Lexer.h"

Lexer::Lexer() {
	//Do nothing
}

Lexer::Lexer(std::string inputString) {
	input = inputString;
	currentPosition = 0;
}

Lexer::~Lexer() {
	//No cleanup necessary
}

void Lexer::setInput(std::string inputString) {
	input = inputString;
}

void Lexer::addRegexString(std::string regExString) {
	regExs.push_back(new RegEx(regExString));
}

Symbol* Lexer::next() {
	int longestMatch = 0;
	RegEx * longestRegEx = NULL;
	std::string remainingString = input.substr(currentPosition,input.length()-1);
	for (std::vector<RegEx*>::size_type i = 0; i < regExs.size(); i++) {
		int currentMatch = regExs[i]->longMatch(remainingString);
		if (currentMatch > longestMatch) {
			longestMatch = currentMatch;
			longestRegEx = regExs[i];
		}
	}
	currentPosition += longestMatch;
	return new Symbol(longestRegEx->getPattern(), true);
}