#include "State.h"

State::State(int number, ParseRule* basis) {
	this->number = number;
	this->basis.push_back(basis);
}

State::State(int number, ParseRule* basis, State* parent) {
	this->number = number;
	this->basis.push_back(basis);
	parents.push_back(parent);
}

State::~State() {

}

const bool State::operator==(const State &other) {
	//return (basis == other.basis && remaining == other.remaining);
	if (basis.size() != other.basis.size())
		return false;

	for (std::vector< ParseRule* >::size_type i = 0; i < basis.size(); i++) {
		if (*(basis[i]) != *(other.basis[i]))
			return false;
	}

	if (remaining.size() != other.remaining.size()) 
		return false;
	for (std::vector< ParseRule* >::size_type i = 0; i < remaining.size(); i++) {
		if ( *(remaining[i]) != *(other.remaining[i]) ) 
			return false;
	}
	return true;
}

const bool State::operator!=(const State &other) {
	return !(this->operator==(other));
}

const bool State::basisEquals(const State &other) {
	//return (basis == other.basis && remaining == other.remaining);
	if (basis.size() != other.basis.size())
		return false;

	for (std::vector< ParseRule* >::size_type i = 0; i < basis.size(); i++) {
		if (*(basis[i]) != *(other.basis[i]))
			return false;
	}
	return true;
}

std::vector<ParseRule*>* State::getTotal() {
	total.clear();
	for (std::vector<ParseRule*>::size_type i = 0; i < basis.size(); i++) {
		total.push_back(basis[i]);
	}
	for (std::vector<ParseRule*>::size_type i = 0; i < remaining.size(); i++) {
		total.push_back(remaining[i]);
	}
	return(&total);
}
std::vector<ParseRule*>* State::getBasis() {
	return &basis;
}
std::vector<ParseRule*>* State::getRemaining() {
	return &remaining;
}

bool State::containsRule(ParseRule* rule) {
	for (std::vector<ParseRule*>::size_type i = 0; i < basis.size(); i++) {
		if (*rule == *(basis[i]))
			return true;
	}
	for (std::vector<ParseRule*>::size_type i = 0; i < remaining.size(); i++) {
		if (*rule == *(remaining[i]))
			return true;
	}
	return false;
}

std::string State::toString() {
	std::string concat = "";
	concat += "State " + intToString(number) + ":\n";
	for (std::vector<ParseRule*>::size_type j = 0; j < basis.size(); j++) {
		concat += "\t" + basis[j]->toString() + "\n";
	}
	for (std::vector<ParseRule*>::size_type j = 0; j < remaining.size(); j++) {
		concat += "\t+\t" + remaining[j]->toString() + "\n";
	}
	return concat;
}

void State::addParents(std::vector<State*>* parents) {
	bool alreadyIn = false;
	for (std::vector<State*>::size_type i = 0; i < parents->size(); i++) {
		alreadyIn = false;
		for (std::vector<State*>::size_type j = 0; j < this->parents.size(); j++) {
			if (this->parents[j]->basisEquals(*((*parents)[i]))) {
				alreadyIn = true;
				break;
			}
		}
		if (!alreadyIn)
			this->parents.push_back((*parents)[i]);
	}
}
std::vector<State*>* State::getParents() {
	return &parents;
}

std::vector<State*>* State::getDeepParents(int depth) {
	if (depth == 1)
		return &parents;
	std::vector<State*>* recursiveParents = new std::vector<State*>();
	std::vector<State*>* recursiveParentsToAdd;
	for (std::vector<State*>::size_type i = 0; i < parents.size(); i++) {
		recursiveParentsToAdd = parents[i]->getDeepParents(depth-1);
		recursiveParents->insert(recursiveParents->end(), recursiveParentsToAdd->begin(), recursiveParentsToAdd->end());
	}
	return recursiveParents;
}