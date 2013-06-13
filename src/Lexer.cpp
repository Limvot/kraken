#include "Lexer.h"

Lexer::Lexer() {
	//Do nothing
}

Lexer::Lexer(std::string inputString) {
	reader.setString(inputString);
}

Lexer::~Lexer() {
	//No cleanup necessary
}

void Lexer::setInput(std::string inputString) {
	reader.setString(inputString);
}

Symbol* Lexer::next() {
	std::string token = reader.word();
	if (token != "")
		return new Symbol("\""+token+"\"", true);
	return new Symbol("$EOF$", false);
}