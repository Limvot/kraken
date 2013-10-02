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
		Symbol();
		Symbol(std::string name, bool isTerminal);
		Symbol(std::string name, bool isTerminal, std::string value);
		Symbol(std::string name, bool isTerminal, NodeTree<Symbol>* tree);
		~Symbol();
		bool const operator==(const Symbol &other)const;
		bool const operator!=(const Symbol &other)const;
		bool const operator<(const Symbol &other)const;

		std::string getName() const;
		std::string toString() const;
		Symbol clone();
		void setSubTree(NodeTree<Symbol>* tree);
		NodeTree<Symbol>* getSubTree();
		bool isTerminal();
	private:
		std::string name;
		std::string value;
		bool terminal;
		NodeTree<Symbol>* subTree;
};

#endif