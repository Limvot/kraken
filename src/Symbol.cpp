#include "Symbol.h"

Symbol::Symbol(std::string name, bool isTerminal) {
	this->name = name;
	this->isTerminal = isTerminal;
}

Symbol::~Symbol() {

}

std::string Symbol::toString() {
	return(name);
}

