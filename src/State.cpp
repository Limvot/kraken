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
		if (*(basis[i]) != (*(other.basis[i])))
			return false;
	}
	return true;
}

const bool State::basisEqualsExceptLookahead(const State &other) {
	//return (basis == other.basis && remaining == other.remaining);
	if (basis.size() != other.basis.size())
		return false;

	for (std::vector< ParseRule* >::size_type i = 0; i < basis.size(); i++) {
		if (!basis[i]->equalsExceptLookahead(*(other.basis[i])))
			return false;
	}
	return true;
}

void State::combineStates(State &other) {
	for (std::vector< ParseRule* >::size_type i = 0; i < other.basis.size(); i++) {
		bool alreadyIn = false;
		for (std::vector< ParseRule* >::size_type j = 0; j < basis.size(); j++) {
			if (basis[j]->equalsExceptLookahead(*(other.basis[i]))) {
				basis[j]->addLookahead(other.basis[i]->getLookahead());
				alreadyIn = true;
			}
		}
		if (!alreadyIn)
			basis.push_back(other.basis[i]);
	}
	addParents(other.getParents());
}

std::vector<ParseRule*>* State::getTotal() {
	total.clear();
	//std::cout << "Vector will be " << basis.size() << " + " << remaining.size() << std::endl;
	total.insert(total.begin(), basis.begin(), basis.end());
	total.insert(total.end(), remaining.begin(), remaining.end());
	return(&total);
}
std::vector<ParseRule*>* State::getBasis() {
	return &basis;
}
std::vector<ParseRule*>* State::getRemaining() {
	return &remaining;
}

bool State::containsRule(ParseRule* rule) {
	getTotal();
	for (std::vector<ParseRule*>::size_type i = 0; i < total.size(); i++) {
		if (*rule  == *(total[i])) {
			return true;
		}
	}
	return false;
}

void State::addRuleCombineLookahead(ParseRule* rule) {
	getTotal();
	bool alreadyIn = false;
	for (std::vector<ParseRule*>::size_type i = 0; i < total.size(); i++) {
		if (rule->equalsExceptLookahead(*(total[i]))) {
			total[i]->addLookahead(rule->getLookahead());
			alreadyIn = true;
			break;
		}
	}
	if (!alreadyIn)
		basis.push_back(rule);
}

std::string State::toString() {
	std::string concat = "";
	concat += "State " + intToString(number) + " with " + intToString(parents.size()) + " parents:\n";
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
	if (depth <= 0) {
		std::vector<State*>* returnSelf = new std::vector<State*>();
		returnSelf->push_back(this);
		return returnSelf;
	}
	std::vector<State*>* recursiveParents = new std::vector<State*>();
	std::vector<State*>* recursiveParentsToAdd;
	for (std::vector<State*>::size_type i = 0; i < parents.size(); i++) {
		recursiveParentsToAdd = parents[i]->getDeepParents(depth-1);
		recursiveParents->insert(recursiveParents->end(), recursiveParentsToAdd->begin(), recursiveParentsToAdd->end());
	}
	return recursiveParents;
}

int State::getNumber() {
	return number;
}