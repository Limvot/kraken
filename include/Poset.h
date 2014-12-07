
#ifndef POSET_H
#define POSET_H

#include <vector>
#include <set>
#include <map>
#include <queue>

#include <cassert>

#include "util.h"

template <class T>
class Poset {
	public:
		Poset();
		~Poset();
		void addRelationship(T first, T second);
		void addVertex(T vertex);
		bool zeroDependencies(T vertex);
		std::set<T> getDependsOn(T dependency);
		std::vector<T> getTopoSort();
		static void test();
	private:
		//backing data structures
		std::map<T, std::map<T,bool>> adjMatrix;
		std::set<T> verticies;
};

template <class T>
Poset<T>::Poset() {
	//Nothing needed
}

template <class T>
Poset<T>::~Poset() {
	//Ditto
}


template <class T>
void Poset<T>::addRelationship(T first, T second) {
	verticies.insert(first);
	verticies.insert(second);
	adjMatrix[first][second] = true;
}

template <class T>
void Poset<T>::addVertex(T vertex) {
	verticies.insert(vertex);
}

template <class T>
bool Poset<T>::zeroDependencies(T vertex) {
	auto depMapItr = adjMatrix.find(vertex);
	if (depMapItr == adjMatrix.end())
		return true;
	for (auto i : depMapItr->second)
		if (i.second == true)
			return false;
	return true;
}

template <class T>
std::set<T> Poset<T>::getDependsOn(T dependency) {
	std::set<T> vertsThatDependOn;
	for (auto i : adjMatrix) {
		auto depItr = i.second.find(dependency);
		if (depItr != i.second.end() && depItr->second)
			vertsThatDependOn.insert(i.first);
	}
	return vertsThatDependOn;
}

template <class T>
std::vector<T> Poset<T>::getTopoSort() {
	std::vector<T> sorted;
	std::queue<T> toDo;
	for (auto i : verticies)
		if (zeroDependencies(i))
			toDo.push(i);
	while(!toDo.empty()) {
		T current = toDo.front(); toDo.pop();
		sorted.push_back(current);
		for (T depOnCurrent : getDependsOn(current)) {
			adjMatrix[depOnCurrent][current] = false; //Remove the edge to current, since current's now been taken care of
			if (zeroDependencies(depOnCurrent))
				toDo.push(depOnCurrent);
		}
	}
	return sorted;
}


//would make it just an int specilization, but then we get multiple definition complaints....
template<class T>
void Poset<T>::test() {
	std::string result;
	{
		Poset<int> poset;
		poset.addVertex(1000);
		for (int i = 0; i < 20; i++)
			poset.addRelationship(i,i+1);
		result = "";
		for (int i : poset.getTopoSort())
			result += intToString(i) + " ";
		//std::cout << result << std::endl;
		assert(result == "20 1000 19 18 17 16 15 14 13 12 11 10 9 8 7 6 5 4 3 2 1 0 "); //Note that sets do not have a set order, so this could change
																						//This is why the 1000 is in an odd, yet valid, position
	}
	{
		Poset<int> poset;
		for (int i = 0; i < 20; i+=2)
			poset.addRelationship(i,i+1);
		result = "";
		for (int i : poset.getTopoSort())
			result += intToString(i) + " ";
		//std::cout << result << std::endl;
		assert(result == "1 3 5 7 9 11 13 15 17 19 0 2 4 6 8 10 12 14 16 18 ");
	}

	std::cout << "Poset tests passed" << std::endl;
} 

#endif