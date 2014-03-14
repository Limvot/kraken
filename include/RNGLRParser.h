#ifndef RNGLRPARSER_H
#define RNGLRPARSER_H

#include <iostream>
#include <queue>
#include <map>
#include <vector>
#include <algorithm>
#include "Parser.h"
#include "Symbol.h"
#include "GraphStructuredStack.h"
#include "util.h"

class RNGLRParser: public Parser {
	public:
		RNGLRParser();
		~RNGLRParser();
		NodeTree<Symbol>* parseInput(std::string inputString);

	private:
		void reducer(int i);
		void shifter(int i);
		void addChildren(NodeTree<Symbol>* parent, std::vector<NodeTree<Symbol>*>* children, NodeTree<Symbol>* nullableParts);

		void addStates(std::vector< State* >* stateSets, State* state, std::queue<State*>* toDo);
		void addStateReductionsToTable(State* state);
		bool fullyReducesToNull(ParseRule* rule);
		bool reducesToNull(ParseRule* rule);
		bool reducesToNull(ParseRule* rule, std::vector<Symbol> avoidList);

		bool belongsToFamily(NodeTree<Symbol>* node, std::vector<NodeTree<Symbol>*>* nodes);
		bool arePacked(std::vector<NodeTree<Symbol>*> nodes);
		bool isPacked(NodeTree<Symbol>* node);
		void setPacked(NodeTree<Symbol>* node, bool isPacked);

		NodeTree<Symbol>* getNullableParts(ParseRule* rule);
		NodeTree<Symbol>* getNullableParts(ParseRule* rule, std::vector<NodeTree<Symbol>*> avoidList);
		NodeTree<Symbol>* getNullableParts(Symbol symbol);

		std::vector<NodeTree<Symbol>*> getPathEdges(std::vector<NodeTree<int>*> path);

		int findLine(int tokenNum); //Get the line number for a token, used for error reporting

		std::vector<Symbol> input;
		GraphStructuredStack gss;
		//start node, lefthand side of the reduction, reduction length
		struct Reduction {
			NodeTree<int>* from;
			Symbol symbol;
			int length;
			NodeTree<Symbol>* nullableParts;
			NodeTree<Symbol>* label;
		} ;
		std::queue<Reduction> toReduce;
		//Node coming from, state going to
		std::queue< std::pair<NodeTree<int>*, int> > toShift;
		std::vector<std::pair<NodeTree<Symbol>*, int> > SPPFStepNodes;

		std::vector<NodeTree<Symbol>*> nullableParts;
		std::map<NodeTree<Symbol>, bool> packedMap;
};

#endif
