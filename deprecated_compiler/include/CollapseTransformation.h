#ifndef COLLAPSETRANSFORMATION_H
#define COLLAPSETRANSFORMATION_H

#include <queue>
#include <vector>

#include "NodeTransformation.h"

template<class T>
class CollapseTransformation: public NodeTransformation<T,T> {
	public:
		CollapseTransformation(T toCollapse);
		~CollapseTransformation();
		virtual NodeTree<T>* transform(NodeTree<T>* from);

	private:
		T toCollapse;
};

#endif

template<class T>
CollapseTransformation<T>::CollapseTransformation(T toCollapse) {
	this->toCollapse = toCollapse;
}

template<class T>
CollapseTransformation<T>::~CollapseTransformation() {
	//
}

template<class T>
NodeTree<T>* CollapseTransformation<T>::transform(NodeTree<T>* from) {
	std::queue<NodeTree<T>*> toProcess;
	toProcess.push(from);
	while(!toProcess.empty()) {
		NodeTree<T>* node = toProcess.front();
		toProcess.pop();
		std::vector<NodeTree<T>*> children = node->getChildren();
		for (int i = 0; i < children.size(); i++) {
			if (children[i]->getData() == toCollapse) {
				node->removeChild(children[i]);
				std::vector<NodeTree<T>*> newChildren = children[i]->getChildren();
				node->insertChildren(i,newChildren);
				toProcess.push(node); //Do this node again
			}
			else
				toProcess.push(children[i]);
		}
	}
	return from;
}
