
#include <iostream>

class RNGLRParser {
	public:
		parseInput(std::string inputString);
		reducer(int i);
		shifter(int i);
	private:
		Lexer lexer;
		std::vector<Symbol*> input;
		GraphStructuredStack gss;
		//start node, lefthand side of the reduction, reduction length
		std::queue<std::pair< std::pair<GSSNode*, Symbol*>, int > toReduce;
		//Node coming from, state going to
		std::queue< std::pair<GSSNode*, int> > toShift;
};
