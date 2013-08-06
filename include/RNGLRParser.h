#ifndef RNGLRPARSER_H
#define RNGLRPARSER_H

#include <iostream>
#include <queue>
#include "Parser.h"
#include "GraphStructuredStack.h"

class RNGLRParser: public Parser {
	public:
		RNGLRParser();
		~RNGLRParser();
		NodeTree<Symbol*>* parseInput(std::string inputString);

	private:
		void reducer(int i);
		void shifter(int i);
		void addChildren(NodeTree<Symbol*>* parent, std::vector<NodeTree<Symbol*>*> children, int nullablePartsIndex);

		void addStates(std::vector< State* >* stateSets, State* state);
		bool reducesToNull(ParseRule* rule);
		bool reducesToNull(ParseRule* rule, std::vector<Symbol*> avoidList);

		bool belongsToFamily(NodeTree<Symbol*>* node, std::vector<NodeTree<Symbol*>*>* nodes);
		bool arePacked(std::vector<NodeTree<Symbol*>*>* nodes);
		bool isPacked(NodeTree<Symbol*>* node);
		void setPacked(NodeTree<Symbol*>* node, bool isPacked)

		std::vector<Symbol*> input;
		GraphStructuredStack gss;
		//start node, lefthand side of the reduction, reduction length
		std::queue<std::pair< std::pair<NodeTree<int>*, Symbol*>, int > > toReduce;
		//Node coming from, state going to
		std::queue< std::pair<NodeTree<int>*, int> > toShift;

		std::vector<NodeTree<Symbol*>*> nullableParts;
		std::map<NodeTree<Symbol*>*, bool> packedMap;
};

#endif
