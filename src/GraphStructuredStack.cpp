#include "GraphStructuredStack.h"

GraphStructuredStack::GraphStructuredStack() {
	//
}

GraphStructuredStack::~GraphStructuredStack() {
	//
}

NodeTree<int>* GraphStructuredStack::newNode(int stateNum) {
	return new NodeTree<int>("gssNode", stateNum);
}

void GraphStructuredStack::addToFrontier(int frontier, NodeTree<int>* node) {
	//First, make sure our vector has this and lesser frontiers. If not, add it and up to it
	while (frontier >= gss.size()) {
		gss.push_back(new std::vector<NodeTree<int>*>());
	}
	gss[frontier]->push_back(node);
}

NodeTree<int>* GraphStructuredStack::inFrontier(int frontier, int state) {
	if (frontierIsEmpty())
		return NULL;
	for (std::vector<NodeTree<int>*>::size_type i = 0; i < gss[frontier]->size(); i++) {
		if ((*(gss[frontier]))[i]->getData() == state)
			return (*(gss[frontier]))[i];
	}
	return NULL;
}

bool GraphStructuredStack::frontierIsEmpty(int frontier) {
	return frontier >= gss.size() || gss[frontier]->size() == 0;
}

bool GraphStructuredStack::frontierHasAccState(int frontier) {
	//The acc state is always state 1, for now
	return inFrontier(frontier, 1);
}

std::vector<NodeTree<int>*>* GraphStructuredStack::getReachable(NodeTree<int>* start, int lenght) {
	std::vector<NodeTree<int>*>* reachableList = new std::vector<NodeTree<int>*>();
	std::queue<NodeTree<int>*> currentNodes;
	std::queue<NodeTree<int>*> nextNodes;
	currentNodes.push_back(start);
	for (int i = 0; i < lenght; i++) {
		while (!currentNodes.empty()) {
			NodeTree<int>* currentNode = currentNodes.front();
			currentNodes.pop();
			std::vector<NodeTree<T>*> children = currentNode->getChildren();
			for (std::vector<NodeTree<T>*>::size_type j = 0; j < children.size(); j++)
				nextNodes.push_back(children[j]);
		}
		currentNodes = nextNodes;
		nextNodes.clear();
	}
	while (!currentNodes.empty()) {
		reachableList->push_back(currentNodes.front());
		currentNodes.pop();
	}
	return reachableList;
}

bool GraphStructuredStack::hasEdge(NodeTree<int>* start, NodeTree<int>* end) {
	//Really, either testing for parent or child should work.
	return start->findChild(end) != -1;
}

void GraphStructuredStack::addEdge(NodeTree<int>* start, NodeTree<int>* end) {
	start->addChild(end);
	end->addChild(start);
}