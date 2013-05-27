#include "State.h"

State::State(int number, ParseRule* basis) {
	this->number = number;
	this->basis.push_back(basis);
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
		if (remaining[i] != other.remaining[i]) 
			return false;
	}
	return true;
}

const bool State::operator!=(const State &other) {
	return !(this->operator==(other));
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

std::string State::intToString(int theInt) {
	std::stringstream converter;
	converter << theInt;
	return converter.str();
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