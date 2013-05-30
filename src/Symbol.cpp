#include "Symbol.h"

Symbol::Symbol(std::string name, bool isTerminal) {
	this->name = name;
	this->terminal = isTerminal;
	this->subTree = NULL;
}

Symbol::Symbol(std::string name, bool isTerminal, NodeTree* tree) {
	this->name = name;
	this->terminal = isTerminal;
	this->subTree = tree;
}

Symbol::~Symbol() {

}

const bool Symbol::operator==(const Symbol &other) {
	return( name == other.name && terminal == other.terminal);
}

std::string Symbol::toString() {
	return(name); //+ "(" + (terminal ? "T" : "NT") + ")");
}

Symbol* Symbol::clone() {
	return new Symbol(name, terminal, subTree);
}

void Symbol::setSubTree(NodeTree* tree) {
	subTree = tree;
}

NodeTree* Symbol::getSubTree() {
	return subTree;
}

bool Symbol::isTerminal() {
	return terminal;
}

