#include "Symbol.h"

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

Symbol::Symbol(std::string name, bool isTerminal, NodeTree<Symbol*>* tree) {
	this->name = name;
	this->terminal = isTerminal;
	this->subTree = tree;
}

Symbol::~Symbol() {

}

const bool Symbol::operator==(const Symbol &other) {
	return( name == other.name && terminal == other.terminal);
}

const bool Symbol::operator!=(const Symbol &other) {
	return(!this->operator==(other));
}

std::string Symbol::getName() {
	return(name);
}

std::string Symbol::toString() {
	return(name + (terminal ? " " + value : ""));
}

Symbol* Symbol::clone() {
	return new Symbol(name, terminal, subTree);
}

void Symbol::setSubTree(NodeTree<Symbol*>* tree) {
	subTree = tree;
}

NodeTree<Symbol*>* Symbol::getSubTree() {
	return subTree;
}

bool Symbol::isTerminal() {
	return terminal;
}

