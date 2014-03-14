#ifndef NODETRANSFORMATION_H
#define NODETRANSFORMATION_H

#include "NodeTree.h"

#ifndef NULL
#define NULL ((void*)0)
#endif

template <class FROM, class TO>
class NodeTransformation {
	public:
		NodeTransformation();
		virtual ~NodeTransformation();
		virtual NodeTree<TO>* transform(NodeTree<FROM>* from)=0;
	private:

};

template <class FROM, class TO>
NodeTransformation<FROM,TO>::NodeTransformation() {
	//Nothing
}

template <class FROM, class TO>
NodeTransformation<FROM,TO>::~NodeTransformation() {
	//Nothing
}

// template <class FROM, class TO>
// NodeTree<TO>* NodeTransformation<FROM,TO>::transform(NodeTree<FROM>* from) {
// 	return (NodeTree<TO>*)0x1234;
// }

#endif