#ifndef SYMBOL_H
#define SYMBOL_H

#ifndef NULL
#define NULL 0
#endif

#include "NodeTree.h"

#include <vector>
#include <string>

//Circular references
class NodeTree;

class Symbol {
	public:
		Symbol(std::string name, bool isTerminal);
		Symbol(std::string name, bool isTerminal, std::string value);
		Symbol(std::string name, bool isTerminal, NodeTree* tree);
		~Symbol();
		bool const operator==(const Symbol &other);
		std::string getName();
		std::string toString();
		Symbol* clone();
		void setSubTree(NodeTree* tree);
		NodeTree* getSubTree();
		bool isTerminal();
	private:
		std::string name;
		std::string value;
		bool terminal;
		NodeTree* subTree;
};

#endif