#ifndef REMOVALTRANSFORMATION_H
#define REMOVALTRANSFORMATION_H

#include <queue>
#include <vector>

#include "NodeTransformation.h"

template<class T>
class RemovalTransformation: public NodeTransformation<T,T> {
	public:
		RemovalTransformation(T toRemove);
		~RemovalTransformation();
		virtual NodeTree<T>* transform(NodeTree<T>* from);

	private:
		T toRemove;
};

#endif

template<class T>
RemovalTransformation<T>::RemovalTransformation(T toRemove) {
	this->toRemove = toRemove;
}

template<class T>
RemovalTransformation<T>::~RemovalTransformation() {
	//
}

template<class T>
NodeTree<T>* RemovalTransformation<T>::transform(NodeTree<T>* from) {
	std::queue<NodeTree<T>*> toProcess;
	toProcess.push(from);
	while(!toProcess.empty()) {
		NodeTree<T>* node = toProcess.front();
		toProcess.pop();
		std::vector<NodeTree<T>*> children = node->getChildren();
		for (int i = 0; i < children.size(); i++) {
			if (children[i]->getData() == toRemove)
				node->removeChild(children[i]);
			else
				toProcess.push(children[i]);
		}
	}
	return from;
}
