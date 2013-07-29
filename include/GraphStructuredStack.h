#include <iostream>
#include <vector>
#include "NodeTree<int>.h"

#ifndef GRAPH_STRUCTURED_STACK
#define GRAPH_STRUCTURED_STACK

class GraphStructuredStack {
	public:
		GraphStructuredStack();
		~GraphStructuredStack();
		NodeTree<int>* newNode(int stateNum);
		void addToFrontier(int frontier, NodeTree<int>* node);
		NodeTree<int>* inFrontier(int frontier, int state);
		bool frontierIsEmpty(int frontier);
		bool frontierHasAccState(int frontier);
		std::vector<NodeTree<int>*>* getReachable(NodeTree<int>* start, int lenght);
		bool hasEdge(NodeTree<int>* start, NodeTree<int>* end);
		void addEdge(NodeTree<int>* start, NodeTree<int>* end);
	private:
		std::vector<std::vector<NodeTree<int>*>*> gss;
		//
};
