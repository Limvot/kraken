#include "Symbol.h"

Symbol::Symbol(std::string name, bool isTerminal) {
	this->name = name;
	this->isTerminal = isTerminal;
}

Symbol::~Symbol() {

}

const bool Symbol::operator==(const Symbol &other) {
	return( name == other.name && isTerminal == other.isTerminal);
}

std::string Symbol::toString() {
	return(name + "(" + (isTerminal ? "T" : "NT") + ")");
}

