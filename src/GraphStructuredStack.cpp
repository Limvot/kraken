#include "GraphStructuredStack.h"

GraphStructuredStack::GraphStructuredStack() {
	//
}

GraphStructuredStack::~GraphStructuredStack() {
	//
}

GSSNode* GraphStructuredStack::newNode(int stateNum) {
	//
}

void GraphStructuredStack::addToFrontier(int frontier, GSSNode* node) {
	//
}

bool GraphStructuredStack::inFrontier(int frontier, int state) {
	//
}

bool GraphStructuredStack::frontierIsEmpty(int frontier) {
	//
}

bool GraphStructuredStack::frontierHasAccState(int frontier) {
	//
}

std::vector<GSSNode*>* GraphStructuredStack::getReachable(GSSNode* start, int lenght) {
	//
}

bool GraphStructuredStack::hasEdge(GSSNode* start, GSSNode* end) {
	//
}

void GraphStructuredStack::addEdge(GSSNode* start, GSSNode* end) {
	//
}