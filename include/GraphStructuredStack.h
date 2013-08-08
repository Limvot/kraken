#include <iostream>
#include <vector>
#include <queue>
#include "NodeTree.h"
#include "util.h"

#ifndef GRAPH_STRUCTURED_STACK
#define GRAPH_STRUCTURED_STACK

class GraphStructuredStack {
	public:
		GraphStructuredStack();
		~GraphStructuredStack();
		NodeTree<int>* newNode(int stateNum);
		void addToFrontier(int frontier, NodeTree<int>* node);
		NodeTree<int>* inFrontier(int frontier, int state);
		int getContainingFrontier(NodeTree<int>* node);
		bool frontierIsEmpty(int frontier);
		NodeTree<int>* frontierGetAccState(int frontier);
		std::vector<NodeTree<int>*>* getReachable(NodeTree<int>* start, int lenght);
		std::vector<std::vector<NodeTree<int>*> >* getReachablePaths(NodeTree<int>* start, int lenght);
		void recursivePathFind(NodeTree<int>* start, int length, std::vector<NodeTree<int>*> currentPath, std::vector<std::vector<NodeTree<int>*> >* paths);
		bool hasEdge(NodeTree<int>* start, NodeTree<int>* end);
		NodeTree<Symbol*>* getEdge(NodeTree<int>* start, NodeTree<int>* end);
		void addEdge(NodeTree<int>* start, NodeTree<int>* end, NodeTree<Symbol*>* edge);

		std::string toString();
	private:
		std::vector<std::vector<NodeTree<int>*>*> gss;
		std::map<std::pair<NodeTree<int>*, NodeTree<int>*>, NodeTree<Symbol*> edges;
};

#endif
