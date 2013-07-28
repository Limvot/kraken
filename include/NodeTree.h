#ifndef NODETREE_H
#define NODETREE_H

#ifndef NULL
#define NULL 0
#endif

#include <util.h>
//#include <Symbol.h>

#include <vector>
#include <string>
#include <iostream>

//Circular references
//class Symbol;

template<class T>
class NodeTree {
	public:
		NodeTree();
		NodeTree(std::string name, T inData);
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

		T getData();
		void setData(T data);

		int size();
		std::string DOTGraphString();

	private:
		std::string DOTGraphStringHelper();
		std::string getDOTName();
		std::string name;
		T data;
		NodeTree* parent;
		std::vector<NodeTree*> children;

		static int idCounter;
		int id;
};

template<class T>

int NodeTree<T>::idCounter;

template<class T>
NodeTree<T>::NodeTree() {
	parent = NULL;
	name = "UnnamedNode";
	data = NULL;
	
	id = idCounter++;
}

template<class T>
NodeTree<T>::NodeTree(std::string name, T inData) {
	parent = NULL;
	data = NULL;
	this->name = name;
	this->data = inData;
	id = idCounter++;
}

template<class T>
NodeTree<T>::~NodeTree() {
	children.clear();
}

template<class T>
void NodeTree<T>::setParent(NodeTree<T>* parent) {
	if (this->parent != NULL) {
		this->parent->removeChild(this);
	}
	this->parent = parent;
}

template<class T>
NodeTree<T>* NodeTree<T>::getParent() {
	return parent;
}

template<class T>
void NodeTree<T>::addChild(NodeTree<T>* child) {
	if (findChild(child) == -1) 
		children.push_back(child);
}

template<class T>
int NodeTree<T>::findChild(NodeTree<T>* child) {
	for (int i = 0; i < children.size(); i++) {
		if (children[i] == child) {
			return i;
		}
	}
	return -1;
}

template<class T>
void NodeTree<T>::removeChild(int index) {
	children[index] = NULL;
	children.erase(children.begin()+index);
}

template<class T>
void NodeTree<T>::removeChild(NodeTree<T>* child) {
	int index = findChild(child);
	if (index != 0) {
		removeChild(index);
	}
}

template<class T>
int NodeTree<T>::size() {
	int count = 0;
	for (int i = 0; i < children.size(); i++) {
		count += children[i]->size();
	}
	return 1+count;
}

template<class T>
NodeTree<T>* NodeTree<T>::get(int index) {
	return children[index];
}

template<class T>
std::string NodeTree<T>::getName() {
	return name;
}

template<class T>
void NodeTree<T>::setName(std::string name) {
	this->name = name;
}

template<class T>
T NodeTree<T>::getData() {
	return data;
}

template<class T>
void NodeTree<T>::setData(T data) {
	this->data = data;
}

template<class T>
std::string NodeTree<T>::DOTGraphString() {
	return( "digraph Kraken { \n" + DOTGraphStringHelper() + "}");
}

template<class T>
std::string NodeTree<T>::DOTGraphStringHelper() {
	std::string ourDOTRelation = "";
	for (int i = 0; i < children.size(); i++) {
		ourDOTRelation += getDOTName() + " -> " + children[i]->getDOTName() + ";\n" + children[i]->DOTGraphStringHelper();
	}
	return(ourDOTRelation);
}

template<class T>
std::string NodeTree<T>::getDOTName() {
	if (data != NULL)
		return "\"" + name + "-" + data->toString() + "_" + intToString(id) + "\""; //Note that terminals already have a quote in the front of their name, so we don't need to add one
	return "\"" + name + "_" + intToString(id) + "\"";
}

#endif