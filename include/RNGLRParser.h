#include <iostream>
#include <queue>
#include "Parser.h"
#include "GraphStructuredStack.h"

class RNGLRParser: public Parser {
	public:
		RNGLRParser();
		~RNGLRParser();
		NodeTree<Symbol*>* parseInput(std::string inputString);
		void reducer(int i);
		void shifter(int i);
	private:
		std::vector<Symbol*> input;
		GraphStructuredStack gss;
		//start node, lefthand side of the reduction, reduction length
		std::queue<std::pair< std::pair<NodeTree<int>*, Symbol*>, int > > toReduce;
		//Node coming from, state going to
		std::queue< std::pair<NodeTree<int>*, int> > toShift;
};
