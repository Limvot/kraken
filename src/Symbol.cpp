#include "Symbol.h"

Symbol::Symbol() {
	this->name = "UninitlizedSymbol";
	this->terminal = false;
	this->subTree = NULL;
	value = "NoValue";
}

Symbol::Symbol(std::string name, bool isTerminal) {
	this->name = name;
	this->terminal = isTerminal;
	this->subTree = NULL;
	value = "NoValue";
}

Symbol::Symbol(std::string name, bool isTerminal, std::string value) {
	this->name = name;
	this->terminal = isTerminal;
	this->subTree = NULL;
	this->value = value;
}

Symbol::Symbol(std::string name, bool isTerminal, NodeTree<Symbol>* tree) {
	this->name = name;
	this->terminal = isTerminal;
	this->subTree = tree;
}

Symbol::~Symbol() {

}

const bool Symbol::operator==(const Symbol &other) const {
	return( name == other.name && terminal == other.terminal);
}

const bool Symbol::operator!=(const Symbol &other) const {
	return(!this->operator==(other));
}

const bool Symbol::operator<(const Symbol &other) const {
	return name < other.getName();
}

std::string Symbol::getName() const {
	return(name);
}

std::string Symbol::getValue() const {
	return(value);
}

std::string Symbol::toString() const {
	return(name + (terminal ? " " + value : ""));
}

void Symbol::setSubTree(NodeTree<Symbol>* tree) {
	subTree = tree;
}

NodeTree<Symbol>* Symbol::getSubTree() {
	return subTree;
}

bool Symbol::isTerminal() {
	return terminal;
}

