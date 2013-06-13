#include "ParseRule.h"

ParseRule::ParseRule() {
	pointerIndex = 0;
	leftHandle = NULL;
}

ParseRule::ParseRule(Symbol* leftHandle, int pointerIndex, std::vector<Symbol*> &rightSide, Symbol* lookahead) {
	this->leftHandle = leftHandle;
	this->pointerIndex = pointerIndex;
	this->rightSide = rightSide;
	this->lookahead = lookahead;
}

ParseRule::~ParseRule() {

}

const bool ParseRule::operator==(const ParseRule &other) {
	return( leftHandle == other.leftHandle && rightSide == other.rightSide && pointerIndex == other.pointerIndex );
}

const bool ParseRule::operator!=(const ParseRule &other) {
	return !(this->operator==(other));
}

ParseRule* ParseRule::clone() {
	return( new ParseRule(leftHandle, pointerIndex, rightSide) );
}

void ParseRule::setLeftHandle(Symbol* leftHandle) {
	this->leftHandle = leftHandle;
}

void ParseRule::appendToRight(Symbol* appendee) {
	rightSide.push_back(appendee);
}

Symbol* ParseRule::getLeftSide() {
	return leftHandle;
}

std::vector<Symbol*> ParseRule::getRightSide() {
	return rightSide;
}

Symbol* ParseRule::getAtNextIndex() {
	if (pointerIndex >= rightSide.size())
		return NULL;
	return rightSide[pointerIndex];
}

Symbol* ParseRule::getAtIndex() {
	if (pointerIndex < 1)
		return NULL;
	return rightSide[pointerIndex-1];
}

int ParseRule::getRightSize() {
	return rightSide.size();
}

int ParseRule::getIndex() {
	return pointerIndex-1;
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

std::string ParseRule::toString() {
	std::string concat = leftHandle->toString() + " -> ";
	for (int i = 0; i < rightSide.size(); i++) {
		if (i == pointerIndex)
			concat += "(*) ";
		concat += rightSide[i]->toString() + " ";
	}
	if (pointerIndex >= rightSide.size())
		concat += "(*)";
	return(concat);
}

std::string ParseRule::toDOT() {
	std::string concat = "";
	for (int i = 0; i < rightSide.size(); i++) {
		concat += leftHandle->toString() + " -> " + rightSide[i]->toString() + ";\n";
	}
	return(concat);
}

