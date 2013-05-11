#ifndef NODETREE_H
#define NODETREE_H

#ifndef NULL
#define NULL 0
#endif NULL

#include <vector>
#include <string>

class NodeTree {
	public:
		NodeTree();
		NodeTree(std::string name);
		~NodeTree();

		void setParent(NodeTree* parent);
		NodeTree* getParent();

		void addChild(NodeTree* child);
		int findChild(NodeTree* child);
		void removeChild(NodeTree* child);
		void removeChild(int index);

		NodeTree* get(int index);
		std::string getName();

		void setName(std::string);

		int size();
		std::string DOTGraphString();

	private:
		std::string DOTGraphStringHelper();
		std::string name;
		NodeTree* parent;
		std::vector<NodeTree*> children;
};

#endif //NODETREE_H