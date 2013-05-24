#include "ParseRule.h"

ParseRule::ParseRule() {
	pointerIndex = 0;
	leftHandle = NULL;
}

ParseRule::ParseRule(Symbol* leftHandle, int pointerIndex, std::vector<Symbol*> &rightSide) {
	this->leftHandle = leftHandle;
	this->pointerIndex = pointerIndex;
	this->rightSide = rightSide;
}

ParseRule::~ParseRule() {

}

const bool ParseRule::operator==(const ParseRule &other) {
	return( leftHandle == other.leftHandle && rightSide == other.rightSide && pointerIndex == other.pointerIndex );
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

