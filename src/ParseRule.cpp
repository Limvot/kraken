#include "ParseRule.h"

ParseRule::ParseRule() {
	pointerIndex = 0;
	leftHandle = NULL;
}

ParseRule::~ParseRule() {

}

void ParseRule::setLeftHandle(Symbol* leftHandle) {
	this->leftHandle = leftHandle;
}

void ParseRule::appendToRight(Symbol* appendee) {
	rightSide.push_back(appendee);
}

std::string ParseRule::toString() {
	std::string concat = leftHandle->toString() + " -> ";
	for (int i = 0; i < rightSide.size(); i++) {
		concat += rightSide[i]->toString() + " ";
	}
	return(concat + ";");
}

