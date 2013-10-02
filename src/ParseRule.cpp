#include "ParseRule.h"

ParseRule::ParseRule() {
	pointerIndex = 0;
	lookahead = NULL;
}

ParseRule::ParseRule(Symbol leftHandle, int pointerIndex, std::vector<Symbol> &rightSide, std::vector<Symbol>* lookahead) {
	this->leftHandle = leftHandle;
	this->pointerIndex = pointerIndex;
	this->rightSide = rightSide;
	this->lookahead = lookahead;
}

ParseRule::~ParseRule() {

}

const bool ParseRule::equalsExceptLookahead(const ParseRule &other) {
	return(leftHandle == other.leftHandle && rightSide == other.rightSide && pointerIndex == other.pointerIndex);
}

const bool ParseRule::operator==(const ParseRule &other) {
	return(equalsExceptLookahead(other) && (lookahead == NULL ? other.lookahead == NULL : (*lookahead) == *(other.lookahead)));
}

const bool ParseRule::operator!=(const ParseRule &other) {
	return !(this->operator==(other));
}

ParseRule* ParseRule::clone() {
	return( new ParseRule(leftHandle, pointerIndex, rightSide, lookahead) );
}

void ParseRule::setLeftHandle(Symbol leftHandle) {
	this->leftHandle = leftHandle;
}

void ParseRule::appendToRight(Symbol appendee) {
	rightSide.push_back(appendee);
}

Symbol ParseRule::getLeftSide() {
	return leftHandle;
}

void ParseRule::setRightSide(std::vector<Symbol> rightSide) {
	this->rightSide = rightSide;
}

std::vector<Symbol> ParseRule::getRightSide() {
	return rightSide;
}

Symbol ParseRule::getAtNextIndex() {
	if (pointerIndex >= rightSide.size())
		return Symbol();
	return rightSide[pointerIndex];
}

Symbol ParseRule::getAtIndex() {
	if (pointerIndex < 1)
		return Symbol();
	return rightSide[pointerIndex-1];
}

int ParseRule::getRightSize() {
	return rightSide.size();
}

int ParseRule::getIndex() {
	return pointerIndex;
}

bool ParseRule::advancePointer() {
	if (pointerIndex < rightSide.size()) {
		pointerIndex++;
		return true;
	}
	return false;
}

bool ParseRule::isAtEnd() {
	return pointerIndex == rightSide.size();
}

void ParseRule::setLookahead(std::vector<Symbol>* lookahead) {
	this->lookahead = lookahead;
}

void ParseRule::addLookahead(std::vector<Symbol>* lookahead) {
	for (std::vector<Symbol>::size_type i = 0; i < lookahead->size(); i++) {
		bool alreadyIn = false;
		for (std::vector<Symbol>::size_type j = 0; j < this->lookahead->size(); j++) {
			if ((*lookahead)[i] == (*(this->lookahead))[j]) {
				alreadyIn = true;
				break;
			}
		}
		if (!alreadyIn)
			this->lookahead->push_back((*lookahead)[i]);
	}
}

std::vector<Symbol>* ParseRule::getLookahead() {
	return lookahead;
}

std::string ParseRule::toString() {
	std::string concat = leftHandle.toString() + " -> ";
	for (int i = 0; i < rightSide.size(); i++) {
		if (i == pointerIndex)
			concat += "(*) ";
		concat += rightSide[i].toString() + " ";
	}
	if (pointerIndex >= rightSide.size())
		concat += "(*)";
	if (lookahead != NULL) {
		concat += "**";
		for (std::vector<Symbol>::size_type i = 0; i < lookahead->size(); i++)
			concat += (*lookahead)[i].toString();
		concat += "**";
	}
	return(concat);
}

std::string ParseRule::toDOT() {
	std::string concat = "";
	for (int i = 0; i < rightSide.size(); i++) {
		concat += leftHandle.toString() + " -> " + rightSide[i].toString() + ";\n";
	}
	return(concat);
}

