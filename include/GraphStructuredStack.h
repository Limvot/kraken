#include <iostream>
#include <vector>
#include "GSSNode.h"

#ifndef GRAPH_STRUCTURED_STACK
#define GRAPH_STRUCTURED_STACK

class GraphStructuredStack {
	public:
		GraphStructuredStack();
		~GraphStructuredStack();
		GSSNode* newNode(int stateNum);
		void addToFrontier(int frontier, GSSNode* node);
		bool inFrontier(int frontier, int state);
		bool frontierIsEmpty(int frontier);
		bool frontierHasAccState(int frontier);
		std::vector<GSSNode*>* getReachable(GSSNode* start, int lenght);
		bool hasEdge(GSSNode* start, GSSNode* end);
		void addEdge(GSSNode* start, GSSNode* end);
	private:
		std::vector<std::vector<GSSNode*>*> gss;
		//
};
