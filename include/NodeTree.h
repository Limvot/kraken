#ifndef NODETREE_H
#define NODETREE_H

#ifndef NULL
#define NULL 0
#endif

#include <util.h>
#include <Symbol.h>

#include <vector>
#include <string>
#include <iostream>

//Circular references
class Symbol;

class NodeTree {
	public:
		NodeTree();
		NodeTree(std::string name, Symbol* inSymbol);
		~NodeTree();

		void setParent(NodeTree* parent);
		NodeTree* getParent();

		void addChild(NodeTree* child);
		int findChild(NodeTree* child);
		void removeChild(NodeTree* child);
		void removeChild(int index);

		NodeTree* get(int index);

		std::string getName();
		void setName(std::string);

		Symbol* getSymbol();
		void setSymbol(Symbol* symbol);

		int size();
		std::string DOTGraphString();

	private:
		std::string DOTGraphStringHelper();
		std::string getDOTName();
		std::string name;
		Symbol* symbol;
		NodeTree* parent;
		std::vector<NodeTree*> children;

		static int idCounter;
		int id;
};

#endif