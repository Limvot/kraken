#include "NodeTree.h"

int NodeTree::idCounter;

NodeTree::NodeTree() {
	parent = NULL;
	name = "UnnamedNode";
	symbol = NULL;
	
	id = idCounter++;
}

NodeTree::NodeTree(std::string name, Symbol* inSymbol) {
	parent = NULL;
	symbol = NULL;
	this->name = name;
	this->symbol = inSymbol;
	id = idCounter++;
}

NodeTree::~NodeTree() {
	children.clear();
}

void NodeTree::setParent(NodeTree* parent) {
	if (this->parent != NULL) {
		this->parent->removeChild(this);
	}
	this->parent = parent;
}

NodeTree* NodeTree::getParent() {
	return parent;
}

void NodeTree::addChild(NodeTree* child) {
	if (findChild(child) == -1) 
		children.push_back(child);
}

int NodeTree::findChild(NodeTree* child) {
	for (int i = 0; i < children.size(); i++) {
		if (children[i] == child) {
			return i;
		}
	}
	return -1;
}

void NodeTree::removeChild(int index) {
	children[index] = NULL;
	children.erase(children.begin()+index);
}

void NodeTree::removeChild(NodeTree* child) {
	int index = findChild(child);
	if (index != 0) {
		removeChild(index);
	}
}

int NodeTree::size() {
	int count = 0;
	for (int i = 0; i < children.size(); i++) {
		count += children[i]->size();
	}
	return 1+count;
}

NodeTree* NodeTree::get(int index) {
	return children[index];
}

std::string NodeTree::getName() {
	return name;
}

void NodeTree::setName(std::string name) {
	this->name = name;
}

Symbol* NodeTree::getSymbol() {
	return symbol;
}

void NodeTree::setSymbol(Symbol* symbol) {
	this->symbol = symbol;
}

std::string NodeTree::DOTGraphString() {
	return( "digraph Kraken { \n" + DOTGraphStringHelper() + "}");
}

std::string NodeTree::DOTGraphStringHelper() {
	std::string ourDOTRelation = "";
	for (int i = 0; i < children.size(); i++) {
		ourDOTRelation += getDOTName() + " -> " + children[i]->getDOTName() + ";\n" + children[i]->DOTGraphStringHelper();
	}
	return(ourDOTRelation);
}

std::string NodeTree::getDOTName() {
	if (symbol != NULL)
		return "\"" + name + "-" + symbol->toString() + "_" + intToString(id) + "\""; //Note that terminals already have a quote in the front of their name, so we don't need to add one
	return "\"" + name + "_" + intToString(id) + "\"";
}