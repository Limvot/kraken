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
	while (gss.size() <= frontier) {
		std::cout << "Adding a new frontier: " << gss.size() << std::endl;
		gss.push_back(new std::vector<NodeTree<int>*>());
	}
	std::cout << "Adding " << node << " (" << node->getData() << ") to frontier " << frontier << std::endl;
	gss[frontier]->push_back(node);
}

NodeTree<int>* GraphStructuredStack::inFrontier(int frontier, int state) {
	if (frontierIsEmpty(frontier))
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

std::vector<NodeTree<int>*>* GraphStructuredStack::getReachable(NodeTree<int>* start, int length) {
	std::vector<NodeTree<int>*>* reachableList = new std::vector<NodeTree<int>*>();
	std::queue<NodeTree<int>*> currentNodes;
	std::queue<NodeTree<int>*> nextNodes;
	currentNodes.push(start);
	for (int i = 0; i < length; i++) {
		while (!currentNodes.empty()) {
			NodeTree<int>* currentNode = currentNodes.front();
			currentNodes.pop();
			std::vector<NodeTree<int>*> children = currentNode->getChildren();
			std::cout << currentNode->getData() << " has children ";
			for (std::vector<NodeTree<int>*>::size_type j = 0; j < children.size(); j++) {
				std::cout << children[j]->getData() << " ";
				nextNodes.push(children[j]);
			}
			std::cout << std::endl;
		}
		currentNodes = nextNodes;
		//No clear function, so go through and remove
		while(!nextNodes.empty())
			nextNodes.pop();
	}
	while (!currentNodes.empty()) {
		reachableList->push_back(currentNodes.front());
		std::cout << currentNodes.front()->getData() << " is reachable from " << start->getData() << " by length " << length << std::endl;
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

std::string GraphStructuredStack::toString() {
	std::string tostring = "";
	for (std::vector<std::vector<NodeTree<int>*>*>::size_type i = 0; i < gss.size(); i++) {
		tostring += "Frontier: " + intToString(i) + "\n";
		for (std::vector<NodeTree<int>*>::size_type j = 0; j < gss[i]->size(); j++) {
			tostring += "|" + intToString((*(gss[i]))[j]->getData()) + "| ";
		}
		tostring += "\n";
	}
	return tostring;
}
