#include "Lexer.h"

Lexer::Lexer() {
	//Do nothing
	currentPosition = 0;
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

void Lexer::addRegEx(std::string regExString) {
	regExs.push_back(new RegEx(regExString));
}

Symbol* Lexer::next() {
	std::cout << "Current at is \"" << input.substr(currentPosition,input.length()-1) << "\" currentPos is " << currentPosition <<std::endl;
	//If we're at the end, return an eof
	if (currentPosition == input.length()-1)
		return new Symbol("$EOF$", true);
	int longestMatch = -1;
	RegEx* longestRegEx = NULL;
	std::string remainingString = input.substr(currentPosition,input.length()-1);
	for (std::vector<RegEx*>::size_type i = 0; i < regExs.size(); i++) {
		std::cout << "Trying regex " << regExs[i]->getPattern() << std::endl;
		int currentMatch = regExs[i]->longMatch(remainingString);
		if (currentMatch > longestMatch) {
			longestMatch = currentMatch;
			longestRegEx = regExs[i];
		}
	}
	if (longestRegEx != NULL) {
		currentPosition += longestMatch + 1;
	std::cout << "Current at is \"" << input.substr(currentPosition,input.length()-1) << "\" currentPos is " << currentPosition <<std::endl;
		return new Symbol(longestRegEx->getPattern(), true);
	} else {
		std::cout << "Found no applicable regex" << std::endl;
		std::cout << "Remaining is " << input.substr(currentPosition,input.length()-1) << std::endl;
		return new Symbol("$NO_APPLICABLE_REGEX$", true);
	}
}