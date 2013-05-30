#ifndef SYMBOL_H
#define SYMBOL_H

#ifndef NULL
#define NULL 0
#endif

#include "NodeTree.h"

#include <vector>
#include <string>

class Symbol {
	public:
		Symbol(std::string name, bool isTerminal);
		Symbol(std::string name, bool isTerminal, NodeTree* tree);
		~Symbol();
		bool const operator==(const Symbol &other);
		std::string toString();
		Symbol* clone();
		void setSubTree(NodeTree* tree);
		NodeTree* getSubTree();
		bool isTerminal();
	private:
		std::string name;
		bool terminal;
		NodeTree* subTree;


};

#endif