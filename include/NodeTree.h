#ifndef NODETREE_H
#define NODETREE_H

#ifndef NULL
#define NULL 0
#endif

#include <vector>
#include <string>
#include <iostream>

#include "util.h"

template<class T>
class NodeTree {
	public:
		NodeTree();
		NodeTree(std::string name, T inData);
		~NodeTree();

		bool const operator==(NodeTree &other);
		bool const operator<(const NodeTree &other) const;

		void setParent(NodeTree<T>* parent);
		void addParent(NodeTree<T>* parent);
		NodeTree<T>* getParent();
		std::vector<NodeTree<T>*> getParents();

		void addChild(NodeTree<T>* child);
		void addChildren(std::vector<NodeTree<T>*>* children);
		int findChild(NodeTree<T>* child);
		void removeChild(NodeTree<T>* child);
		void removeChild(int index);
		void clearChildren();
		std::vector<NodeTree<T>*> getChildren();

		NodeTree<T>* get(int index);
		std::string getName();
		void setName(std::string);

		T getData() const;
		void setData(T data);

		int size();
		std::string DOTGraphString();

	private:
		std::string DOTGraphStringHelper();
		std::string getDOTName();
		std::string name;
		T data;
		std::vector<NodeTree<T>*> parents;
		std::vector<NodeTree<T>*> children;

		static int idCounter;
		int id;
};

template<class T>

int NodeTree<T>::idCounter;

template<class T>
NodeTree<T>::NodeTree() {
	name = "UnnamedNode";
	data = NULL;
	
	id = idCounter++;
}

template<class T>
NodeTree<T>::NodeTree(std::string name, T inData) {
	this->name = name;
	this->data = inData;
	id = idCounter++;
}

template<class T>
NodeTree<T>::~NodeTree() {
	children.clear();
	parents.clear(); //? Will this segfault?
}

template<class T>
const bool NodeTree<T>::operator==(NodeTree &other) {
	if (!(data == other.data))
		return false;
	if (children.size() != other.getChildren().size())
		return false;
	for (typename std::vector<NodeTree<T>*>::size_type i = 0; i < children.size(); i++)
		if (! (*(children[i]) == *(other.getChildren()[i])))
			return false;
	return true;
}

template<class T>
const bool NodeTree<T>::operator<(const NodeTree &other) const {
	return data < other.getData();
}

template<class T>
void NodeTree<T>::setParent(NodeTree<T>* parent) {
	parents.clear();
	parents.push_back(parent);
}

template<class T>
void NodeTree<T>::addParent(NodeTree<T>* parent) {
	parents.push_back(parent);
}

template<class T>
NodeTree<T>* NodeTree<T>::getParent() {
	if (parents.size() > 0)
		return parents[0];
	return NULL;
}

template<class T>
std::vector<NodeTree<T>*> NodeTree<T>::getParents() {
	return parents;
}


template<class T>
void NodeTree<T>::addChild(NodeTree<T>* child) {
	if (findChild(child) == -1) 
		children.push_back(child);
}

template<class T>
void NodeTree<T>::addChildren(std::vector<NodeTree<T>*>* children) {
	for (typename std::vector<NodeTree<T>*>::size_type i = 0; i < children->size(); i++)
		addChild((*children)[i]);
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
	if (index != -1) {
		removeChild(index);
	}
}

template<class T>
void NodeTree<T>::clearChildren() {
	for (typename std::vector<T>::size_type i = 0; i < children.size(); i++)
		children[i] = NULL;
	children.clear();
}

template<class T>
std::vector<NodeTree<T>*> NodeTree<T>::getChildren() {
	return children;
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
T NodeTree<T>::getData() const {
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
		if (children[i] != NULL)
			ourDOTRelation += getDOTName() + " -> " + children[i]->getDOTName() + ";\n" + children[i]->DOTGraphStringHelper();
		else
			ourDOTRelation += getDOTName() + " -> BAD_NULL_" + getDOTName() + "\n";
	}
	return(ourDOTRelation);
}

template<class T>
std::string NodeTree<T>::getDOTName() {
	std::string DOTName = "";
	if (data != NULL)
		DOTName = "\"" + name + "-" + data->toString() + "_" + intToString(id) + "\""; //Note that terminals already have a quote in the front of their name, so we don't need to add one
	else
		DOTName = "\"" + name + "_" + intToString(id) + "\"";
	return(replace(DOTName, "\n", "\\n"));
}

#endif