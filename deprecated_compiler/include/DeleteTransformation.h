#ifndef DELETETRANSFORMATION_H
#define DELETETRANSFORMATION_H

#include <queue>
#include <vector>

#include "NodeTransformation.h"

template<class T>
class DeleteTransformation: public NodeTransformation<T,T> {
	public:
		DeleteTransformation(T toDelete);
		~DeleteTransformation();
		virtual NodeTree<T>* transform(NodeTree<T>* from);

	private:
		T toRemove;
};

#endif

template<class T>
DeleteTransformation<T>::DeleteTransformation(T toRemove) {
	this->toRemove = toRemove;
}

template<class T>
DeleteTransformation<T>::~DeleteTransformation() {
	//
}

template<class T>
NodeTree<T>* DeleteTransformation<T>::transform(NodeTree<T>* from) {
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
